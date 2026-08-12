# C Backend Clang Target

A Clang plugin that preserves type information in LLVM IR for more accurate C code generation with llvm-cbe.

## Problem

LLVM IR doesn't preserve integer signedness. When llvm-cbe converts IR back to C, it can't distinguish `int` from `unsigned int`, causing incorrect function signatures and compiler warnings.

**Before:**
```llvm
define i32 @add(i32 %a, i32 %b)  ; No signedness info!
```
**After (with this plugin):**
```llvm
define i32 @add(i32 signext %a, i32 zeroext %b)  ; Signedness preserved!
```

## Setup

### 1. Build plugin

**In-tree** (when llvm-cbe is symlinked into `llvm/projects/`):
```bash
ninja -C /path/to/llvm/build CBackendPlugin
```

**Standalone**:
```bash
cmake -S clang-plugin -B clang-plugin/build \
      -DLLVM_DIR=/path/to/llvm/build/lib/cmake/llvm \
      -DClang_DIR=/path/to/llvm/build/lib/cmake/clang
cmake --build clang-plugin/build
# Creates: clang-plugin/build/CBackendPlugin.so
```

### 2. Patch Clang

```bash
cd /path/to/llvm-project
patch -p1 < /path/to/clang-plugin/CodeGenModule.patch
ninja -C build
```

The patch adds a hook to `CodeGenModule::getTargetCodeGenInfo()` that calls `getCBackendABIForTarget` via `DynamicLibrary::SearchForAddressOfSymbol` when the plugin is loaded.

## Usage

```bash
clang -fplugin ./build/CBackendPlugin.so \
      -target llvm64-linux-gnu \
      -S -emit-llvm input.c -o output.ll

llvm-cbe output.ll -o output.cbe.c
```

## How It Works

`CodeGenModule::getTargetCodeGenInfo()` checks via `dlsym` whether `getCBackendABIForTarget` is present. If the plugin is loaded and the target triple has arch `llvm32` or `llvm64`, it returns a custom `TargetCodeGenInfo` that:

- Marks signed integers with `signext`
- Marks unsigned integers with `zeroext`
- Returns structs directly (no `sret`)
- Passes structs directly (no `byval`)

## Target Triple Examples

This doesn't need to exactly match the target environment, but the closer you get, the more likely the ABI will get translated correctly. In particular, 32 vs. 64 pointer size, endianness (currently unsupported), and C++ ABI (OS) are the most likely to be impactful, in that order.

Format: `llvm32-<os>[-<env>]` or `llvm64-<os>[-<env>]`

| Triple | Description |
|--------|-------------|
| `llvm32-linux-gnu` | Linux (glibc) 32-bit |
| `llvm64-linux-gnu` | Linux (glibc) 64-bit |
| `llvm64-darwin` | macOS 64-bit |
| `llvm32-windows-msvc` | Windows (MSVC) 32-bit |
| `llvm64-windows-msvc` | Windows (MSVC) 64-bit |