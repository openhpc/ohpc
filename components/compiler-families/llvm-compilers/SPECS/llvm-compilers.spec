#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

%ifarch aarch64 
%global triple aarch64-pc-linux-gnu
%global build_target AArch64
%else
%global triple x86_64-pc-linux-gnu
%global build_target X86
%endif

%global major_ver 9
%global pname llvm%{major_ver}-compilers

Summary:   LLVM (An Optimizing Compiler Infrastructure)
Name:      %{pname}%{PROJ_DELIM}
Version:   9.0.0
Release:   1%{?dist}
License:   Apache-2.0 with LLVM exception
Group:     %{PROJ_NAME}/compiler-families
URL:       http://www.llvm.org
Source0:   http://releases.llvm.org/%{version}/llvm-%{version}.src.tar.xz
Source1:   http://releases.llvm.org/%{version}/cfe-%{version}.src.tar.xz
Source2:   http://releases.llvm.org/%{version}/compiler-rt-%{version}.src.tar.xz
Source3:   http://releases.llvm.org/%{version}/libcxx-%{version}.src.tar.xz
Source4:   http://releases.llvm.org/%{version}/libcxxabi-%{version}.src.tar.xz
Source5:   http://releases.llvm.org/%{version}/libunwind-%{version}.src.tar.xz
Source6:   http://releases.llvm.org/%{version}/lld-%{version}.src.tar.xz
Source7:   http://releases.llvm.org/%{version}/openmp-%{version}.src.tar.xz
Source8:   http://releases.llvm.org/%{version}/clang-tools-extra-%{version}.src.tar.xz

BuildRequires: cmake%{PROJ_DELIM}
BuildRequires: gnu9-compilers%{PROJ_DELIM}
BuildRequires: python
BuildRequires: zlib-devel
BuildRequires: pkgconfig
BuildRequires: binutils-devel
BuildRequires: perl
BuildRequires: perl(Data::Dumper)
BuildRequires: zlib-devel
BuildRequires: glibc >= 2.26
Requires:      binutils


%define install_path %{OHPC_COMPILERS}/llvm/%{version}

%description
LLVM is a compiler infrastructure designed for compile-time, link-time, runtime,
and idle-time optimization of programs from arbitrary programming languages.
LLVM is written in C++ and has been developed since 2000 at the University of
Illinois and Apple. It currently supports compilation of C and C++ programs, 
using front-ends derived from GCC 4.0.1. The compiler infrastructure
includes mirror sets of programming tools as well as libraries with equivalent
functionality.
This package includes: clang, clang-tools-extra, libcxx, libcxxabi, compiler-rt,
                       openmp, libunwind, and lld
  

%prep
%setup -q -c -a1 -a2 -a3 -a4 -a5 -a6 -a7 -a8 

%{__ln_s} llvm-%{version}.src llvm
%{__ln_s} cfe-%{version}.src clang
%{__ln_s} lld-%{version}.src lld
%{__mv} clang-tools-extra-%{version}.src clang/tools/extra
%{__ln_s} clang/tools/extra clang-tools-extra
%{__ln_s} compiler-rt-%{version}.src compiler-rt
%{__ln_s} libcxx-%{version}.src libcxx
%{__ln_s} libcxxabi-%{version}.src libcxxabi
%{__ln_s} libunwind-%{version}.src libunwind
%{__ln_s} openmp-%{version}.src openmp


%install
module load cmake
module load gnu9

GNU8=$(command -v g++ | sed s#/bin.*##)

MAIN=$(pwd)
%{__mkdir} bootstrap
BOOTSTRAP=$MAIN/bootstrap
%{__mkdir} build

# STAGE 1
# Bootstrap clang with gcc
# Then (re)build all components in stage 2
# This is done manually as the BOOTSTRAP cmake option wouldn't
#    build libc++ in stage 1 when needed for stage 2 build [JCS 13NOV19]
cd build
cmake --enable-optimise -Wno-dev -G"Unix Makefiles" ../llvm \
      -DCMAKE_INSTALL_PREFIX="/" \
      -DCMAKE_C_COMPILER=gcc \
      -DCMAKE_CXX_COMPILER=g++ \
      -DCMAKE_C_FLAGS=-Wl,-rpath,${GNU8}/lib64 \
      -DCMAKE_CXX_FLAGS=-Wl,-rpath,${GNU8}/lib64 \
      -DCMAKE_BUILD_TYPE=Release \
      -DLLVM_DEFAULT_TARGET_TRIPLE=%{triple} \
      -DLLVM_ENABLE_PROJECTS="clang;lld" \
      -DLLVM_ENABLE_RUNTIMES="compiler-rt;libunwind;libcxxabi;libcxx" \
      -DLLVM_BUILD_TOOLS=llvm-config \
      -DLLVM_BINUTILS_INCDIR=/usr/include \
      -DLLVM_TARGETS_TO_BUILD=%{build_target} \
      -DCOMPILER_RT_BUILD_BUILTINS=On \
      -DCOMPILER_RT_BUILD_SANITIZERS=Off \
      -DCOMPILER_RT_BUILD_XRAY=Off \
      -DCOMPILER_RT_BUILD_LIBFUZZER=Off \
      -DCOMPILER_RT_BUILD_PROFILE=Off \
      -DBUILD_SHARED_LIBS=On

# Occasional failure when using make all. Appears to be race condition [JCS 13NOV19]
# Compliation takes a long time if the -j option is removed [JCS 13NOV19]
%{__make} %{?_smp_mflags} VERBOSE=1 clang
%{__make} %{?_smp_mflags} VERBOSE=1 lld
%{__make} %{?_smp_mflags} VERBOSE=1 cxxabi
%{__make} %{?_smp_mflags} VERBOSE=1 cxx
%{__make} %{?_smp_mflags} VERBOSE=1 
%{__make} DESTDIR=$MAIN/bootstrap INSTALL="%{__install} -p" install
cd $MAIN
# End Stage 1

module unload gnu9

# STAGE 2
# Rebuild all components with clang
# Swtich to LLVM libc++, compiler-rt, and libunwind
# The gnu8 stack isn't needed after rebuild

# Rebuild llvm+clang
%{__mkdir_p} llvm/build/lib
%{__cp} $BOOTSTRAP/lib/%{triple}/c++/* $MAIN/llvm/build/lib
cd llvm/build
cmake --enable-optimise -Wno-dev -G"Unix Makefiles" .. \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX="%{install_path}" \
      -DCMAKE_C_COMPILER=$BOOTSTRAP/bin/clang \
      -DCMAKE_CXX_COMPILER=$BOOTSTRAP/bin/clang++ \
      -DCMAKE_ASM_COMPILER=$BOOTSTRAP/bin/clang \
      -DCMAKE_AR=$BOOTSTRAP/bin/llvm-ar \
      -DCMAKE_RANLIB=$BOOTSTRAP/bin/llvm-ranlib \
      -DCMAKE_C_FLAGS="-I$BOOTSTRAP/include/c++/v1" \
      -DCMAKE_CXX_FLAGS="-I$BOOTSTRAP/include/c++/v1" \
      -DCMAKE_EXE_LINKER_FLAGS="-rtlib=compiler-rt -L$MAIN/llvm/build/lib -lunwind" \
      -DCMAKE_SHARED_LINKER_FLAGS="-rtlib=compiler-rt -L$MAIN/llvm/build/lib -lunwind" \
      -DLLVM_ENABLE_LIBCXX=On \
      -DLLVM_ENABLE_LIBCXXABI=On \
      -DLLVM_DEFAULT_TARGET_TRIPLE=%{triple} \
      -DLLVM_BINUTILS_INCDIR=/usr/include \
      -DLLVM_ENABLE_LLD=On \
      -DLLVM_ENABLE_FPIC=On \
      -DLLVM_CONFIG_PATH=$BOOTSTRAP/bin/llvm-config \
      -DLLVM_ENABLE_PROJECTS="clang;lld;clang-tools-extra;openmp" \
      -DLLVM_ENABLE_RUNTIMES="compiler-rt;libcxxabi;libcxx;libunwind" \
      -DLLVM_ENABLE_LTO=Thin \
      -DLLVM_TARGETS_TO_BUILD=%{build_target} \
      -DLLVM_LINK_LLVM_DYLIB=On \
      -DLLVM_DYLIB_COMPONENTS=all \
      -DLLVM_INSTALL_TOOLCHAIN_ONLY=On \
      -DCLANG_DEFAULT_LINKER=lld \
      -DCLANG_DEFAULT_RTLIB=compiler-rt \
      -DCLANG_DEFAULT_UNWINDLIB=libunwind \
      -DCLANG_DEFAULT_CXX_STDLIB=libc++ \
      -DLIBOMP_FORTRAN_MODULES=Off \
      -DLIBOMP_COPY_EXPORTS=Off \
      -DLIBOMP_USE_HWLOC=Off \
      -DLIBOMP_OMPT_SUPPORT=On \
      -DLIBOMP_ENABLE_SHARED=On \
      -DLIBOMP_CFLAGS="-fuse-ld=lld -stdlib=libc++" \
      -DLIBOMP_CPPFLAGS="-fuse-ld=lld -stdlib=libc++" \
      -DLIBOMP_CXXFLAGS="-fuse-ld=lld -stdlib=libc++" \
      -DLIBOMP_LDFLAGS="-rtlib=compiler-rt" \
      -DLIBOMP_LLVM_TOOLS_DIR="$BOOTSTRAP/bin" \
      -DLIBOMP_LIBFLAGS="-lm" \
      -DLIBUNWIND_USE_COMPILER_RT=On \
      -DCOMPILER_RT_USE_LIBCXX=On \
      -DCOMPILER_RT_BUILD_BUILTINS=On \
      -DCOMPILER_RT_USE_BUILTINS_LIBRARY=On \
      -DLIBCXXABI_USE_LLVM_UNWINDER=On \
      -DLIBCXXABI_USE_COMPILER_RT=On \
      -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$MAIN/libcxxabi/include \
      -DLIBCXX_CXX_ABI_LIBRARY_PATH=$MAIN/llvm/build/lib/%{triple}/c++ \
      -DLIBCXX_CXX_ABI=libcxxabi \
      -DLIBCXX_USE_COMPILER_RT=On \
      -DLIBCXX_ENABLE_SHARED=On \
      -DLIBCXX_ENABLE_STATIC=Off 

%{__make} %{?_smp_mflags} VERBOSE=1 clang
%{__make} %{?_smp_mflags} VERBOSE=1 lld
%{__make} %{?_smp_mflags} VERBOSE=1 compiler-rt
%{__make} %{?_smp_mflags} VERBOSE=1 unwind
%{__make} %{?_smp_mflags} VERBOSE=1 cxxabi
%{__make} %{?_smp_mflags} VERBOSE=1 cxx
%{__make} %{?_smp_mflags} VERBOSE=1 omp
# Race condition persists with remaining components; remove -j option
%{__make} VERBOSE=1
%{__make} DESTDIR=%{buildroot} INSTALL="%{__install} -p" install
%{__rm} %{buildroot}/%{install_path}/lib/%{triple}/c++/*.a
cd $MAIN
# End stage 2

# OpenHPC module file
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
prepend-path   LD_LIBRARY_PATH    %{install_path}/lib:%{install_path}/lib/%{triple}/c++
prepend-path   MODULEPATH         %{OHPC_MODULEDEPS}/llvm


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
%doc llvm/LICENSE.TXT
%doc llvm/README.txt
%doc llvm/RELEASE_TESTERS.TXT
