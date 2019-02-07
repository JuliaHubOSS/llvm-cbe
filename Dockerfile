FROM buildpack-deps:stretch

RUN \
  set -e; \
  apt update && apt install -y clang; \
  wget -qO- "https://cmake.org/files/v3.13/cmake-3.13.1-Linux-x86_64.tar.gz" \
    | tar --strip-components=1 -xz -C /usr/local; \
  cd /root; \
  curl -fLO https://github.com/ninja-build/ninja/releases/download/v1.9.0/ninja-linux.zip; \
  unzip /root/ninja-linux.zip; \
  rm /root/ninja-linux.zip; \
  mv /root/ninja /bin/ninja; \
  chmod +x /bin/ninja; \
  curl -fL http://releases.llvm.org/7.0.1/llvm-7.0.1.src.tar.xz \
    | tar xJf -; \
  mv /root/llvm-7.0.1.src /root/llvm; \
  mkdir -p /root/llvm/build;

COPY . /root/llvm/projects/llvm-cbe/

RUN \
  set -e; \
  mkdir -p /root/llvm/build; \
  cd /root/llvm/build; \
  cmake -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..; \
  ninja llvm-cbe; \
  ninja lli; \
  ninja CBEUnitTests; \
  /root/llvm/build/projects/llvm-cbe/unittests/CWriterTest; \
  ln -s /root/llvm/build/bin/llvm-cbe /bin/llvm-cbe;
