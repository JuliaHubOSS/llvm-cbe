//===-- llc.cpp - Implement the LLVM Native Code Generator ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is the llc code generator driver. It provides a convenient
// command-line interface for generating native assembly-language code
// or C code, given LLVM bitcode.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#if LLVM_VERSION_MAJOR >= 7
#include "llvm/CodeGen/CommandFlags.inc"
#else
#include "llvm/CodeGen/CommandFlags.def"
#endif
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetMachine.h"
#include <memory>
using namespace llvm;

extern "C" void LLVMInitializeCBackendTarget();
extern "C" void LLVMInitializeCBackendTargetInfo();
extern "C" void LLVMInitializeCBackendTargetMC();

// General options for llc.  Other pass-specific options are specified
// within the corresponding llc passes, and target-specific options
// and back-end code generation options are specified with the target machine.
//
static cl::opt<std::string>
    InputFilename(cl::Positional, cl::desc("<input bitcode>"), cl::init("-"));

static cl::opt<std::string> OutputFilename("o", cl::desc("Output filename"),
                                           cl::value_desc("filename"));

static cl::opt<unsigned>
    TimeCompilations("time-compilations", cl::Hidden, cl::init(1u),
                     cl::value_desc("N"),
                     cl::desc("Repeat compilation N times for timing"));

// Determine optimization level.
static cl::opt<char>
    OptLevel("O",
             cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
                      "(default = '-O2')"),
             cl::Prefix, cl::ZeroOrMore, cl::init(' '));

static cl::opt<std::string>
    TargetTriple("mtriple", cl::desc("Override target triple for module"));

cl::opt<bool> NoVerify("disable-verify", cl::Hidden,
                       cl::desc("Do not verify input module"));

static int compileModule(char **, LLVMContext &);

// GetFileNameRoot - Helper function to get the basename of a filename.
static inline std::string GetFileNameRoot(const std::string &InputFilename) {
  std::string IFN = InputFilename;
  std::string outputFilename;
  int Len = IFN.length();
  if ((Len > 2) && IFN[Len - 3] == '.' &&
      ((IFN[Len - 2] == 'b' && IFN[Len - 1] == 'c') ||
       (IFN[Len - 2] == 'l' && IFN[Len - 1] == 'l'))) {
    outputFilename = std::string(IFN.begin(), IFN.end() - 3); // s/.bc/.s/
  } else {
    outputFilename = IFN;
  }
  return outputFilename;
}

static ToolOutputFile *GetOutputStream(const char *TargetName,
                                       Triple::OSType OS,
                                       const char *ProgName) {
  // If we don't yet have an output filename, make one.
  if (OutputFilename.empty()) {
    if (InputFilename == "-")
      OutputFilename = "-";
    else {
      OutputFilename = GetFileNameRoot(InputFilename);

      switch (FileType) {
      case TargetMachine::CGFT_AssemblyFile:
        if (TargetName[0] == 'c') {
          if (TargetName[1] == 0)
            OutputFilename += ".cbe.c";
          else if (TargetName[1] == 'p' && TargetName[2] == 'p')
            OutputFilename += ".cpp";
          else
            OutputFilename += ".s";
        } else
          OutputFilename += ".s";
        break;
      case TargetMachine::CGFT_ObjectFile:
        if (OS == Triple::Win32)
          OutputFilename += ".obj";
        else
          OutputFilename += ".o";
        break;
      case TargetMachine::CGFT_Null:
        OutputFilename += ".null";
        break;
      }
    }
  }

  // Decide if we need "binary" output.
  bool Binary = false;
  switch (FileType) {
  case TargetMachine::CGFT_AssemblyFile:
    break;
  case TargetMachine::CGFT_ObjectFile:
  case TargetMachine::CGFT_Null:
    Binary = true;
    break;
  }

  // Open the file.
  std::error_code error;
  sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
  if (Binary)
    OpenFlags |= sys::fs::F_Text;
  ToolOutputFile *FDOut =
      new ToolOutputFile(OutputFilename.c_str(), error, OpenFlags);
  if (error) {
    errs() << error.message() << '\n';
    delete FDOut;
    return 0;
  }

  return FDOut;
}

static LLVMContext TheContext;

// main - Entry point for the llc compiler.
//
int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);

  // Enable debug stream buffering.
  EnableDebugBuffering = true;

  llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  // Initialize targets first, so that --version shows registered targets.
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  LLVMInitializeCBackendTarget();
  LLVMInitializeCBackendTargetInfo();
  LLVMInitializeCBackendTargetMC();

  // Initialize codegen and IR passes used by llc so that the -print-after,
  // -print-before, and -stop-after options work.
  PassRegistry *Registry = PassRegistry::getPassRegistry();
  initializeCore(*Registry);
  initializeCodeGen(*Registry);
  initializeLoopStrengthReducePass(*Registry);
  initializeLowerIntrinsicsPass(*Registry);
  initializeUnreachableBlockElimLegacyPassPass(*Registry);

  // Register the target printer for --version.
  cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

  cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");

  // Compile the module TimeCompilations times to give better compile time
  // metrics.
  for (unsigned I = TimeCompilations; I; --I)
    if (int RetVal = compileModule(argv, TheContext))
      return RetVal;
  return 0;
}

static int compileModule(char **argv, LLVMContext &Context) {
  // Load the module to be compiled...
  SMDiagnostic Err;

  std::unique_ptr<Module> M;

  Module *mod = 0;
  Triple TheTriple;

  bool SkipModule =
      MCPU == "help" || (!MAttrs.empty() && MAttrs.front() == "help");

  // If user just wants to list available options, skip module loading
  if (!SkipModule) {
    M = parseIRFile(InputFilename, Err, Context);
    mod = M.get();
    if (mod == 0) {
      Err.print(argv[0], errs());
      return 1;
    }

    // If we are supposed to override the target triple, do so now.
    if (!TargetTriple.empty())
      mod->setTargetTriple(Triple::normalize(TargetTriple));
    TheTriple = Triple(mod->getTargetTriple());
  } else {
    TheTriple = Triple(Triple::normalize(TargetTriple));
  }

  if (TheTriple.getTriple().empty())
    TheTriple.setTriple(sys::getDefaultTargetTriple());

  // Get the target specific parser.
  std::string Error;
  // Override MArch
  MArch = "c";
  const Target *TheTarget =
      TargetRegistry::lookupTarget(MArch, TheTriple, Error);
  if (!TheTarget) {
    errs() << argv[0] << ": " << Error << "\n";
    return 1;
  }

  // Package up features to be passed to target/subtarget
  std::string FeaturesStr;
  if (MAttrs.size()) {
    SubtargetFeatures Features;
    for (unsigned i = 0; i != MAttrs.size(); ++i)
      Features.AddFeature(MAttrs[i]);
    FeaturesStr = Features.getString();
  }

  CodeGenOpt::Level OLvl = CodeGenOpt::Default;

  switch (OptLevel) {
  default:
    errs() << argv[0] << ": invalid optimization level.\n";
    return 1;
  case ' ':
    break;
  case '0':
    OLvl = CodeGenOpt::None;
    break;
  case '1':
    OLvl = CodeGenOpt::Less;
    break;
  case '2':
    OLvl = CodeGenOpt::Default;
    break;
  case '3':
    OLvl = CodeGenOpt::Aggressive;
    break;
  }

  TargetOptions Options;
  Options.AllowFPOpFusion = FuseFPOps;
  Options.UnsafeFPMath = EnableUnsafeFPMath;
  Options.NoInfsFPMath = EnableNoInfsFPMath;
  Options.NoNaNsFPMath = EnableNoNaNsFPMath;
  Options.HonorSignDependentRoundingFPMathOption =
      EnableHonorSignDependentRoundingFPMath;
  if (FloatABIForCalls != FloatABI::Default)
    Options.FloatABIType = FloatABIForCalls;
  Options.NoZerosInBSS = DontPlaceZerosInBSS;
  Options.GuaranteedTailCallOpt = EnableGuaranteedTailCallOpt;
  Options.StackAlignmentOverride = OverrideStackAlignment;

  // Jackson Korba 9/30/14
  // OwningPtr<targetMachine>
  std::unique_ptr<TargetMachine> target(TheTarget->createTargetMachine(
      TheTriple.getTriple(), MCPU, FeaturesStr, Options, getRelocModel(),
      getCodeModel(), OLvl));
  assert(target.get() && "Could not allocate target machine!");
  assert(mod && "Should have exited after outputting help!");
  TargetMachine &Target = *target.get();

  // Disable .loc support for older OS X versions.
  if (TheTriple.isMacOSX() && TheTriple.isMacOSXVersionLT(10, 6)) {
  }
  // TODO: Find a replacement to this function
  /* Greg Simpson 6-09-13
  no member named setMCUseLoc
  removed statement
  Target.setMCUseLoc(false);  */

  // Jackson Korba 9/30/14
  std::unique_ptr<ToolOutputFile> Out(
      GetOutputStream(TheTarget->getName(), TheTriple.getOS(), argv[0]));
  if (!Out)
    return 1;

  // Build up all of the passes that we want to do to the module.
  legacy::PassManager PM;

  // Add an appropriate TargetLibraryInfo pass for the module's triple.
  TargetLibraryInfoWrapperPass *TLI =
      new TargetLibraryInfoWrapperPass(TheTriple);
  PM.add(TLI);

  // Add intenal analysis passes from the target machine.
  PM.add(createTargetTransformInfoWrapperPass(Target.getTargetIRAnalysis()));

  if (RelaxAll) {
    if (FileType != TargetMachine::CGFT_ObjectFile)
      errs() << argv[0]
             << ": warning: ignoring -mc-relax-all because filetype != obj\n";
  }

  // Ask the target to add backend passes as necessary.
  if (Target.addPassesToEmitFile(PM, Out->os(),
#if LLVM_VERSION_MAJOR >= 7
                                 nullptr,
#endif
                                 FileType, NoVerify)) {
    errs() << argv[0] << ": target does not support generation of this"
           << " file type!\n";
    return 1;
  }

  // Before executing passes, print the final values of the LLVM options.
  cl::PrintOptionValues();

  PM.run(*mod);

  // Declare success.
  Out->keep();

  return 0;
}
