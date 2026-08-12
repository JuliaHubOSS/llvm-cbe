# LLVM-CBE Build Instructions

## Install CMake (if needed)

```bash
curl -L -O https://github.com/Kitware/CMake/releases/download/v3.30.0/cmake-3.30.0-linux-x86_64.tar.gz
tar -xzf cmake-3.30.0-linux-x86_64.tar.gz
export PATH=$PWD/cmake-3.30.0-linux-x86_64/bin:$PATH
```

## Build LLVM (if needed)

```bash
git clone --single-branch --branch llvmorg-20.1.8 --depth 1 https://github.com/llvm/llvm-project.git
cd llvm-project
cmake -S llvm -B build -G "Ninja" -DCMAKE_BUILD_TYPE=Debug -DLLVM_ENABLE_PROJECTS="clang" -DLLVM_ENABLE_ASSERTIONS=ON -DBUILD_SHARED_LIBS=ON
ninja
```

## Run Tests

### pytest
```bash
cd /home/vtjnash/llvm-cbe
pip install pytest pytest-xdist
pytest -n 4
```

### Unit tests
```bash
cd /home/vtjnash/llvm-project/llvm/build
make CBEUnitTests && projects/llvm-cbe/unittests/CWriterTest
```
