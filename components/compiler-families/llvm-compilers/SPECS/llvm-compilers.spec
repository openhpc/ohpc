#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Simple check for other active LLVM versions; they will break this build
%if 0%(which clang llvm-ar >/dev/null 2>&1; echo $?) == 0
%{error: "LLVM found in PATH; cannot build LLVM with with another LLVM available"}
%endif

%include %{_sourcedir}/OHPC_macros

%ifarch aarch64 
%global triple aarch64-pc-linux-gnu
%global build_target AArch64
%else
%global triple x86_64-pc-linux-gnu
%global build_target X86
%endif

# Limit the number of parallel build processes to avoid race conditions
%global _smp_ncpus_max 8

%global major_ver 10
%global pname llvm%{major_ver}-compilers

Summary:   LLVM (An Optimizing Compiler Infrastructure)
Name:      %{pname}%{PROJ_DELIM}
Version:   10.0.1
Release:   1%{?dist}
License:   Apache-2.0 with LLVM exception
Group:     %{PROJ_NAME}/compiler-families
URL:       http://www.llvm.org
Source0:   https://github.com/llvm/llvm-project/releases/download/llvmorg-%{version}/llvm-project-%{version}.tar.xz

BuildRequires: cmake%{PROJ_DELIM}
BuildRequires: gcc
BuildRequires: gcc-c++
BuildRequires: python3
BuildRequires: zlib-devel
BuildRequires: pkgconfig
BuildRequires: binutils-devel
BuildRequires: glibc >= 2.26
BuildRequires: libffi-devel
%if 0%{?sle_version}
BuildRequires: ninja
BuildRequires: libelf-devel
%endif
%if 0%{?rhel}
BuildRequires: ninja-build
BuildRequires: elfutils-libelf-devel
%endif

Requires:      binutils
Requires:      python3
Conflicts:     libunwind-devel


%define install_path %{OHPC_COMPILERS}/llvm/%{version}

%description
LLVM is a compiler infrastructure designed for compile-time, link-time, runtime,
and idle-time optimization of programs from arbitrary programming languages.
LLVM is written in C++ and has been developed since 2000 at the University of
Illinois and Apple. It currently supports compilation of C and C++ programs, 
using front-ends derived from GCC 4.0.1. The compiler infrastructure
includes mirror sets of programming tools as well as libraries with equivalent
functionality.
This package includes: clang, libcxx, libcxxabi, compiler-rt, openmp,
                       libunwind, lld, clang-tools-extra, libclc, and polly 
  

%prep
%setup -q -n llvm-project-%{version} 
%{__mkdir_p} stage1
%{__mkdir_p} stage2

# Replace any ambiguous python shebangs or rpmbuild will fail
find . -type f -exec sed -i '1s@#! */usr/bin/env python\($\| \)@#!/usr/bin/python2@' "{}" \;


%build
module load cmake

MAIN=$(pwd)
BOOTSTRAP=$MAIN/stage1
STAGE2=$MAIN/stage2

# STAGE 1
# Bootstrap llvm with the distro llvm
# Lots of options; the goal is to disable as much as possible
#    to reduce build time, but keep all of the required components
#    needed to rebuild LLVM10 with LLVM10
# ZLib support required for OpenSUSE15
cd $BOOTSTRAP
cmake -DCMAKE_INSTALL_PREFIX="/" \
      -DCMAKE_C_COMPILER=gcc \
      -DCMAKE_CXX_COMPILER=g++ \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$BOOTSTRAP/lib -L$BOOTSTRAP/lib" \
      -DLLVM_DEFAULT_TARGET_TRIPLE=%{triple} \
      -DLLVM_ENABLE_PROJECTS="clang;lld;compiler-rt;libunwind;libcxxabi;libcxx" \
      -DLLVM_BUILD_TOOLS=On \
      -DLLVM_TARGETS_TO_BUILD=%{build_target} \
      -DLLVM_INCLUDE_TESTS=Off \
      -DLLVM_INCLUDE_EXAMPLES=Off \
      -DLLVM_INCLUDE_UTILS=Off \
      -DLLVM_INCLUDE_DOCS=Off \
      -DLLVM_INCLUDE_BENCHMARKS=Off \
      -DLLVM_ENABLE_ZLIB=On \
      -DLLVM_ENABLE_Z3_SOLVER=Off \
      -DLLVM_ENABLE_BACKTRACES=Off \
      -DLLVM_LINK_LLVM_DYLIB=On \
      -DLLVM_ENABLE_LTO=Off \
      -DLLVM_PARALLEL_LINK_JOBS=1 \
      -DLLVM_STATIC_LINK_CXX_STDLIB=On \
      -DCLANG_INCLUDE_TESTS=Off \
      -DCLANG_ENABLE_STATIC_ANALYZER=Off \
      -DCLANG_ENABLE_ARCMT=Off \
      -DCOMPILER_RT_INCLUDE_TESTS=Off \
      -DCOMPILER_RT_BUILD_BUILTINS=On \
      -DCOMPILER_RT_BUILD_SANITIZERS=Off \
      -DCOMPILER_RT_BUILD_XRAY=Off \
      -DCOMPILER_RT_BUILD_LIBFUZZER=Off \
      -DCOMPILER_RT_BUILD_PROFILE=Off \
      -DLIBCXX_INCLUDE_BENCHMARKS=Off \
      -DLIBCXX_INCLUDE_TESTS=Off \
      -DLIBCXX_ENABLE_EXPERIMENTAL_LIBRARY=Off \
      --enable-optimise -Wno-dev -G Ninja ../llvm 

ninja %{?_smp_mflags} -v 
# End Stage 1

# STAGE 2
# Rebuild all components with new clang.
# Swtich to using libc++, compiler-rt, and libunwind only.
# Several settings appear redundant. Configuration options are 
#   inconsistent between projects. Setting all possible variables for each
#   option appears to work.
# To use ninja check-all, reenable all static libraries before building.

%{__mkdir_p} $STAGE2/lib

cd $STAGE2
cmake -DPYTHON_EXECUTABLE=/usr/bin/python3 \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX="%{install_path}" \
      -DCMAKE_C_COMPILER=$BOOTSTRAP/bin/clang \
      -DCMAKE_CXX_COMPILER=$BOOTSTRAP/bin/clang++ \
      -DCMAKE_C_FLAGS="-fuse-ld=lld -fPIC -stdlib=libc++ -Qunused-arguments" \
      -DCMAKE_CXX_FLAGS="-fuse-ld=lld -fPIC -stdlib=libc++ -Qunused-arguments" \
      -DCMAKE_ASM_COMPILER=$BOOTSTRAP/bin/clang \
      -DCMAKE_AR=$BOOTSTRAP/bin/llvm-ar \
      -DCMAKE_RANLIB=$BOOTSTRAP/bin/llvm-ranlib \
      -DCMAKE_LINKER=$BOOTSTRAP/bin/ld.lld \
      -DCMAKE_EXE_LINKER_FLAGS="-rtlib=compiler-rt -L$STAGE2/lib -Wl,-thinlto-jobs=4" \
      -DCMAKE_SHARED_LINKER_FLAGS="-rtlib=compiler-rt -L$STAGE2/lib -Wl,-thinlto-jobs=4" \
      -DLLVM_TABLEGEN=$BOOTSTRAP/bin/llvm-tblgen \
      -DLLVM_OPTIMIZED_TABLEGEN=On \
      -DLLVM_CONFIG_PATH=$BOOTSTRAP/bin/llvm-config \
      -DLLVM_ENABLE_LIBCXX=On \
      -DLLVM_DEFAULT_TARGET_TRIPLE=%{triple} \
      -DLLVM_BINUTILS_INCDIR=/usr/include \
      -DLLVM_ENABLE_LLD=On \
      -DLLVM_ENABLE_PROJECTS="clang;lld;openmp;libclc;clang-tools-extra;polly;compiler-rt;libunwind;libcxxabi;libcxx" \
      -DLLVM_ENABLE_LTO=Thin \
      -DLLVM_TARGETS_TO_BUILD=%{build_target} \
      -DLLVM_LINK_LLVM_DYLIB=On \
      -DLLVM_BUILD_LLVM_DYLIB=On \
      -DLLVM_BUILD_STATIC=Off \
      -DLLVM_DYLIB_COMPONENTS=all\
      -DLLVM_INCLUDE_DOCS=Off \
      -DLLVM_INCLUDE_BENCHMARKS=Off \
      -DLLVM_ENABLE_Z3_SOLVER=Off \
      -DLLVM_INSTALL_UTILS=On \
      -DLLVM_ENABLE_ZLIB=On \
      -DLLVM_ENABLE_MODULES=On \
      -DLLVM_ENABLE_RTTI=On \
      -DLLVM_ENABLE_FFI=On \
      -DLLVM_USE_INTEL_JITEVENTS=On \
      -DLLVM_USE_PERF=On \
      -DLLVM_BUILD_TESTS=Off \
      -DLLVM_ENABLE_PIC=On \
      -DLLVM_INSTALL_TOOLCHAIN_ONLY=On \
      -DLLVM_PARALLEL_LINK_JOBS=1 \
      -DCLANG_DEFAULT_LINKER=lld \
      -DCLANG_PLUGIN_SUPPORT=On \
      -DCLANG_DEFAULT_RTLIB=compiler-rt \
      -DCLANG_DEFAULT_UNWINDLIB=libunwind \
      -DCLANG_DEFAULT_CXX_STDLIB=libc++ \
      -DLIBUNWIND_USE_COMPILER_RT=On \
      -DLIBUNWIND_ENABLE_STATIC=Off \
      -DCOMPILER_RT_USE_LIBCXX=On \
      -DCOMPILER_RT_BUILD_BUILTINS=On \
      -DLIBCXXABI_USE_LLVM_UNWINDER=On \
      -DLIBCXXABI_USE_COMPILER_RT=On \
      -DLIBCXXABI_ENABLE_STATIC=Off \
      -DLIBCXX_CXX_ABI=libcxxabi \
      -DLIBCXX_USE_COMPILER_RT=On \
      -DLIBCXX_ENABLE_SHARED=On \
      -DLIBCXX_ENABLE_STATIC=Off \
      -DLIBCXX_CXX_ABI_INCLUDE_PATHS="$MAIN/libcxxabi/include" \
      -DLIBOMP_ENABLE_SHARED=On \
      -DLIBOMP_ENABLE_STATIC=Off \
      -DLIBOMP_LIBFLAGS="-lm" \
      -DLIBOMP_FORTRAN_MODULES=Off \
      -DLIBOMP_COPY_EXPORTS=Off \
      -DLIBOMP_USE_HWLOC=Off \
      -DLIBOMP_OMPT_SUPPORT=On \
      --enable-optimized -Wno-dev -G Ninja ../llvm

ninja %{?_smp_mflags} -v
# End stage 2


%install
cd stage2
DESTDIR=%{buildroot} INSTALL="%{__install} -p" ninja install

# OpenHPC module files
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/llvm%{major_ver}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/llvm%{major_ver}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the LLVM compiler infrastructure"
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: LLVM Compiler Infrastructure"
module-whatis "Version: %{version}"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: LLVM Compiler Infrastructure"
module-whatis "URL: http://www.llvm.org"

set     version           %{version}

setenv         LLVM%{major_ver}_PATH         %{install_path}
prepend-path   PATH               %{install_path}/bin
prepend-path   MANPATH            %{install_path}/share/man
prepend-path   INCLUDE            %{install_path}/include/c++/v1
prepend-path   LD_LIBRARY_PATH    %{install_path}/lib
prepend-path   MODULEPATH         %{OHPC_MODULEDEPS}/llvm%{major_ver}

family "compiler"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/llvm%{major_ver}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF


%files
%{OHPC_MODULES}/llvm%{major_ver}
%dir %{OHPC_COMPILERS}/llvm
%{install_path}
%doc llvm/CODE_OWNERS.TXT
%doc llvm/CREDITS.TXT
%doc llvm/README.txt
%doc llvm/RELEASE_TESTERS.TXT
%license llvm/LICENSE.TXT
