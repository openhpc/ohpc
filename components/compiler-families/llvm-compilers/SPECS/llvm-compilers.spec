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

%global clang_sha 64043d5cec9fb02d1b0fd80c9f2c4e9e4f09cf8f
%global flang_sha 37e86e06f74d9bd91ef6bb511c026753b9124006

%global pname llvm6-compilers
%global major_ver 6

Summary:   The LLVM Compiler Infrastructure
Name:      %{pname}%{PROJ_DELIM}
Version:   6.0.1
Release:   1%{?dist}
License:   UIUC, Apache-2.0
Group:     %{PROJ_NAME}/compiler-families
URL:       http://www.llvm.org
Source0:   http://releases.llvm.org/%{version}/llvm-%{version}.src.tar.xz
Source1:   https://github.com/flang-compiler/clang/archive/%{clang_sha}.tar.gz
Source2:   https://github.com/flang-compiler/flang/archive/flang_20180612.tar.gz
Source3:   http://releases.llvm.org/%{version}/compiler-rt-%{version}.src.tar.xz
Source4:   http://releases.llvm.org/%{version}/libcxx-%{version}.src.tar.xz
Source5:   http://releases.llvm.org/%{version}/libcxxabi-%{version}.src.tar.xz
Source6:   http://releases.llvm.org/%{version}/libunwind-%{version}.src.tar.xz
Source7:   http://releases.llvm.org/%{version}/lld-%{version}.src.tar.xz
Source8:   http://releases.llvm.org/%{version}/openmp-%{version}.src.tar.xz
BuildRequires: cmake%{PROJ_DELIM}
BuildRequires: make
BuildRequires: perl
BuildRequires: python
BuildRequires: pkgconfig
BuildRequires: binutils-devel
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires: gcc-fortran
BuildRequires: libstdc++48-devel
Requires: libstdc++6
%else
BuildRequires: gcc-gfortran
BuildRequires: libstdc++-devel
Requires: libstdc++
%endif
Requires:      binutils
BuildRequires:      gcc-c++
Requires:      gcc-c++
%if 0%{?rhel_version} || 0%{?centos_version} || 0%{?rhel}
BuildRequires: perl-Data-Dumper
%endif

%define install_path %{OHPC_COMPILERS}/llvm/%{version}

%description

The LLVM Compiler Infrastructure.

%prep
%setup -q -c -b1 -b2 -b3 -b4 -b5 -b6 -b7 -b8

cd llvm-%{version}.src/tools
ln -s ../../clang-%{clang_sha} clang
cd ../..

cd llvm-%{version}.src/tools
ln -s ../../lld-%{version}.src lld
cd ../..

cd llvm-%{version}.src/projects
ln -s ../../compiler-rt-%{version}.src compiler-rt
cd ../..

cd llvm-%{version}.src/projects
ln -s ../../libcxx-%{version}.src libcxx
cd ../..

cd llvm-%{version}.src/projects
ln -s ../../libcxxabi-%{version}.src libcxxabi
cd ../..

cd llvm-%{version}.src/projects
ln -s ../../libunwind-%{version}.src libunwind
cd ../..

cd llvm-%{version}.src/projects
ln -s ../../openmp-%{version}.src openmp
cd ../..

ln -s llvm-%{version}.src llvm
ln -s flang-flang_20180612 flang

# Flang code is not ready for -Werror
%{__sed} -i -e 's/-Werror/-Wall/g' flang/CMakeLists.txt

%install
module load cmake

mkdir build-clang
cd build-clang
cmake \
--enable-optimise ../llvm \
-DBUILD_SHARED_LIBS=True \
-DCMAKE_BUILD_TYPE=Release \
-DLIBOMP_FORTRAN_MODULES=False \
-DLIBOMP_COPY_EXPORTS=False \
-DLIBOMP_USE_HWLOC=False \
-DLIBOMP_OMPT_SUPPORT=ON \
-DLLVM_BINUTILS_INCDIR=/usr/include \
-DLLVM_ENABLE_FFI=False \
-DCMAKE_INSTALL_PREFIX=%{install_path}

%{__make} %{?_smp_mflags} VERBOSE=1
%{__make} DESTDIR=$RPM_BUILD_ROOT INSTALL="%{__install} -p" install
cd ..


mkdir build-flang
cd build-flang
CC=$RPM_BUILD_ROOT/%{install_path}/bin/clang \
CXX=$RPM_BUILD_ROOT/%{install_path}/bin/clang++ \
CFLAGS="-fPIC -DPIC" \
CXXFLAGS="-fPIC -DPIC" \
FFLAGS="-fPIC -DPIC" \
FCFLAGS="-fPIC -DPIC" \

cmake \
../flang \
-DCMAKE_SKIP_RPATH=YES \
-DBUILD_SHARED_LIBS=False \
-DCMAKE_BUILD_TYPE=Release \
-DC_INCLUDE_DIRS=$RPM_BUILD_ROOT/%{install_path}/include \
-DLLVM_BINARY_DIR=$RPM_BUILD_ROOT/%{install_path} \
-DLLVM_TOOLS_BINARY_DIR=$RPM_BUILD_ROOT/%{install_path}/bin \
-DLLVM_CONFIG=$RPM_BUILD_ROOT/%{install_path}/bin/llvm-config \
-DLLVM_LIBRARY_DIR=$RPM_BUILD_ROOT/%{install_path}/lib \
-DLLVM_MAIN_INCLUDE_DIR=$RPM_BUILD_ROOT/%{install_path}/include \
-DCMAKE_Fortran_COMPILER=$RPM_BUILD_ROOT/%{install_path}/bin/flang \
-DFLANG_LIBOMP=$RPM_BUILD_ROOT/%{install_path}/lib/libomp.so \
-DCMAKE_INSTALL_PREFIX=%{install_path}


%{__make} VERBOSE=1
%{__make} VERBOSE=1 DESTDIR=$RPM_BUILD_ROOT INSTALL="%{__install} -p" install
cd ..

# Additional step: build OpenMP runtime with compatible Fortran module
mkdir build-openmp
cd build-openmp
FC=$RPM_BUILD_ROOT/%{install_path}/bin/flang \
F77=$RPM_BUILD_ROOT/%{install_path}/bin/flang \
F90=$RPM_BUILD_ROOT/%{install_path}/bin/flang \
F95=$RPM_BUILD_ROOT/%{install_path}/bin/flang \
F03=$RPM_BUILD_ROOT/%{install_path}/bin/flang \
LD_LIBRARY_PATH=$RPM_BUILD_ROOT/%{install_path}/lib:$LD_LIBRARY_PATH \

cmake \
../llvm/projects/openmp \
-DBUILD_SHARED_LIBS=True \
-DCMAKE_BUILD_TYPE=Release \
-DLIBOMP_FORTRAN_MODULES=True \
-DLIBOMP_COPY_EXPORTS=False \
-DLIBOMP_USE_HWLOC=False \
-DLIBOMP_OMPT_SUPPORT=ON \
-DCMAKE_INSTALL_PREFIX=%{install_path}

%{__make} VERBOSE=1
%{__make} DESTDIR=$RPM_BUILD_ROOT INSTALL="%{__install} -p" install
cd ..

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

set     version                            %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path    CMAKE_MODULE_PATH   %{install_path}/lib/cmake/clang:%{install_path}/lib/cmake/llvm
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/llvm


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
%doc flang/LICENSE.txt
%doc llvm/README.txt
%doc flang/README.md
%doc llvm/RELEASE_TESTERS.TXT
