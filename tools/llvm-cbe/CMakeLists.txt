# Support plugins.
set(LLVM_NO_DEAD_STRIP 1)

if (NOT USE_SYSTEM_LLVM)
  set(LLVM_LINK_COMPONENTS
    ${LLVM_TARGETS_TO_BUILD}
    Analysis
    AsmParser
    AsmPrinter
    BitReader
    CBackendCodeGen
    CBackendInfo
    CodeGen
    Core
    IRReader
    MC
    ScalarOpts
    SelectionDAG
    Support
    Target
    )


  add_llvm_tool(llvm-cbe
    llvm-cbe.cpp
    )
  export_executable_symbols(llvm-cbe)
endif()

if(USE_SYSTEM_LLVM)
  add_executable(llvm-cbe
    llvm-cbe.cpp
    )
  llvm_map_components_to_libnames(llvm_libs
    CBackendCodeGen
    CBackendInfo
    )
  target_link_libraries(llvm-cbe
    LLVMXRay;LLVMLibDriver;LLVMDlltoolDriver;LLVMTelemetry;LLVMTextAPIBinaryReader;LLVMCoverage;LLVMLineEditor;LLVMNVPTXCodeGen;LLVMNVPTXDesc;LLVMNVPTXInfo;LLVMRISCVTargetMCA;LLVMRISCVDisassembler;LLVMRISCVAsmParser;LLVMRISCVCodeGen;LLVMRISCVDesc;LLVMRISCVInfo;LLVMWebAssemblyDisassembler;LLVMWebAssemblyAsmParser;LLVMWebAssemblyCodeGen;LLVMWebAssemblyUtils;LLVMWebAssemblyDesc;LLVMWebAssemblyInfo;LLVMBPFDisassembler;LLVMBPFAsmParser;LLVMBPFCodeGen;LLVMBPFDesc;LLVMBPFInfo;LLVMX86TargetMCA;LLVMX86Disassembler;LLVMX86AsmParser;LLVMX86CodeGen;LLVMX86Desc;LLVMX86Info;LLVMARMDisassembler;LLVMARMAsmParser;LLVMARMCodeGen;LLVMARMDesc;LLVMARMUtils;LLVMARMInfo;LLVMAArch64Disassembler;LLVMAArch64AsmParser;LLVMAArch64CodeGen;LLVMAArch64Desc;LLVMAArch64Utils;LLVMAArch64Info;LLVMOrcDebugging;LLVMOrcJIT;LLVMWindowsDriver;LLVMMCJIT;LLVMJITLink;LLVMInterpreter;LLVMExecutionEngine;LLVMRuntimeDyld;LLVMOrcTargetProcess;LLVMOrcShared;LLVMDWP;LLVMOption;LLVMObjectYAML;LLVMObjCopy;LLVMMCA;LLVMMCDisassembler;LLVMLTO;LLVMPasses;LLVMHipStdPar;LLVMCFGuard;LLVMCoroutines;LLVMipo;LLVMVectorize;LLVMSandboxIR;LLVMLinker;LLVMInstrumentation;LLVMFrontendOpenMP;LLVMFrontendOffloading;LLVMFrontendOpenACC;LLVMFrontendHLSL;LLVMFrontendDriver;LLVMFrontendAtomic;LLVMExtensions;LLVMDWARFLinkerParallel;LLVMDWARFLinkerClassic;LLVMDWARFLinker;LLVMGlobalISel;LLVMMIRParser;LLVMAsmPrinter;LLVMSelectionDAG;LLVMCodeGen;LLVMTarget;LLVMObjCARCOpts;LLVMCodeGenTypes;LLVMCGData;LLVMIRPrinter;LLVMInterfaceStub;LLVMFileCheck;LLVMFuzzMutate;LLVMScalarOpts;LLVMInstCombine;LLVMAggressiveInstCombine;LLVMTransformUtils;LLVMBitWriter;LLVMAnalysis;LLVMProfileData;LLVMSymbolize;LLVMObject;LLVMTextAPI;LLVMMCParser;LLVMIRReader;LLVMAsmParser;LLVMMC;LLVMBitReader;LLVMFuzzerCLI;LLVMCore;LLVMRemarks;LLVMBitstreamReader;LLVMBinaryFormat;LLVMTargetParser;LLVMTableGen;LLVMSupport;LLVMDemangle;
    ${llvm_libs})
endif()
