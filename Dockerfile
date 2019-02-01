FROM buildpack-deps:stretch

RUN \
  set -e; \
  apt update && apt install -y clang; \
  wget -qO- "https://cmake.org/files/v3.13/cmake-3.13.1-Linux-x86_64.tar.gz" \
    | tar --strip-components=1 -xz -C /usr/local; \
  cd /root; \
  git clone https://github.com/llvm-mirror/llvm; \
  cd /root/llvm; \
  git checkout release_70; \
  mkdir -p /root/llvm/build; \
  cd /root/llvm/build; \
  cmake ..; \
  make;

COPY . /root/llvm/projects/llvm-cbe/

RUN \
  set -e; \
  cd /root/llvm/projects/llvm-cbe; \
  git reset 45a5de1bb25e0d9bb7fea50100deab829f194c9a --hard; \
  cd /root/llvm/build; \
  cmake ..; \
  make llvm-cbe; \
  make lli; \
  make CBEUnitTests; \
  /root/llvm/build/projects/llvm-cbe/unittests/CWriterTest; \
  ln -s /root/llvm/build/bin/llvm-cbe /bin/llvm-cbe;
