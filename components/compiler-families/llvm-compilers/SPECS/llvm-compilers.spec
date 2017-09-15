#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%define compiler_family gnu7
%include %{_sourcedir}/OHPC_macros

# Remember to drop this gnu-compilers dependency in the future,
# when gcc-5.3+ appear in all the targeted Linux distros.
%global gnuver 7.1.0

%global clang_sha 1210030915d1e1441d62eea54976f4ced7f6ad88
%global flang_sha cab3fc47849d7ba8effef8474b9aae2cc60e8c57

%global pname llvm5-compilers
%global major_ver 5

Summary:   The LLVM Compiler Infrastructure
Name:      %{pname}%{PROJ_DELIM}
Version:   5.0.0
Release:   1%{?dist}
License:   UIUC, Apache-2.0
Group:     %{PROJ_NAME}/compiler-families
URL:       http://www.llvm.org
Source0:   http://releases.llvm.org/%{version}/llvm-%{version}.src.tar.xz
Source1:   https://github.com/flang-compiler/clang/archive/%{clang_sha}.tar.gz
Source2:   https://github.com/flang-compiler/flang/archive/%{flang_sha}.tar.gz
Source3:   http://releases.llvm.org/%{version}/compiler-rt-%{version}.src.tar.xz
Source4:   http://releases.llvm.org/%{version}/libcxx-%{version}.src.tar.xz
Source5:   http://releases.llvm.org/%{version}/libcxxabi-%{version}.src.tar.xz
Source6:   http://releases.llvm.org/%{version}/libunwind-%{version}.src.tar.xz
Source7:   http://releases.llvm.org/%{version}/lld-%{version}.src.tar.xz
Source8:   http://releases.llvm.org/%{version}/openmp-%{version}.src.tar.xz
Source9:   OHPC_macros
Patch1:    0001-GOMP-compatibility-add-missing-OpenMP4.0-task-deps-h.patch
BuildRequires: gnu7-compilers%{PROJ_DELIM}
BuildRequires: cmake%{PROJ_DELIM}
BuildRequires: make
BuildRequires: perl
BuildRequires: python
BuildRequires: pkgconfig
BuildRequires: binutils-devel
BuildRequires: libstdc++-devel
Requires:      binutils
Requires:      gcc-c++
Requires:      gnu7-compilers%{PROJ_DELIM} = %{gnuver}
%if 0%{?rhel_version} || 0%{?centos_version} || 0%{?rhel}
BuildRequires: perl-Data-Dumper
%endif

%define install_path %{OHPC_COMPILERS}/llvm/%{version}
%define gnu_path %{OHPC_COMPILERS}/gcc/%{gnuver}
%if 0%{?sles_version} || 0%{?suse_version}
%define compiler_path `%{OHPC_COMPILERS}/gcc/%{gnuver}/bin/g++ -v --version 2>&1|grep COMPILER_PATH|cut -f 2- -d '='`
%define library_path `%{OHPC_COMPILERS}/gcc/%{gnuver}/bin/g++ -v --version 2>&1|grep LIBRARY_PATH|cut -f 2- -d '='`
%endif

%description

The LLVM Compiler Infrastructure.

%prep
%setup -q -c -b1 -b2 -b3 -b4 -b5 -b6 -b7 -b8
cd openmp-%{version}.src
%patch1 -p1
cd ..

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
ln -s flang-%{flang_sha} flang

# Flang code is not ready for -Werror
%{__sed} -i -e 's/-Werror/-Wall/g' flang/CMakeLists.txt

%install
%ohpc_setup_compiler
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

%if 0%{?sles_version} || 0%{?suse_version}
SUSE_CXXFLAGS="-I%{gnu_path}/include/c++/%{gnuver}"
%ifarch aarch64
SUSE_CXXFLAGS="$SUSE_CXXFLAGS -I%{gnu_path}/include/c++/%{gnuver}/aarch64-unknown-linux-gnu"
%endif
%ifarch x86_64
SUSE_CXXFLAGS="$SUSE_CXXFLAGS -I%{gnu_path}/include/c++/%{gnuver}/x86_64-pc-linux-gnu"
%endif
%endif

mkdir build-flang
cd build-flang
CC=$RPM_BUILD_ROOT/%{install_path}/bin/clang \
CXX=$RPM_BUILD_ROOT/%{install_path}/bin/clang++ \
CFLAGS="-fPIC -DPIC" \
%if 0%{?sles_version} || 0%{?suse_version}
CXXFLAGS="-fPIC -DPIC $SUSE_CXXFLAGS" \
%else
CXXFLAGS="-fPIC -DPIC" \
%endif
FFLAGS="-fPIC -DPIC" \
FCFLAGS="-fPIC -DPIC" \
%if 0%{?sles_version} || 0%{?suse_version}
COMPILER_PATH=%{compiler_path} \
LIBRARY_PATH=%{library_path} \
%endif
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

%if 0%{?sles_version} || 0%{?suse_version}
COMPILER_PATH=%{compiler_path} \
LIBRARY_PATH=%{library_path} \
%endif
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
%if 0%{?sles_version} || 0%{?suse_version}
COMPILER_PATH=%{compiler_path} \
LIBRARY_PATH=%{library_path} \
%endif
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
prepend-path    LD_LIBRARY_PATH     %{gnu_path}/lib64:%{install_path}/lib
prepend-path    CMAKE_MODULE_PATH   %{install_path}/lib/cmake/clang:%{install_path}/lib/cmake/llvm
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/llvm
%if 0%{?sles_version} || 0%{?suse_version}
prepend-path    COMPILER_PATH       %{compiler_path}
prepend-path    LIBRARY_PATH        %{library_path}
%ifarch aarch64
prepend-path    CPLUS_INCLUDE_PATH  %{gnu_path}/include/c++/%{gnuver}:%{gnu_path}/include/c++/%{gnuver}/aarch64-unknown-linux-gnu
%endif
%ifarch x86_64
prepend-path    CPLUS_INCLUDE_PATH  %{gnu_path}/include/c++/%{gnuver}:%{gnu_path}/include/c++/%{gnuver}/x86_64-pc-linux-gnu
%endif
%endif

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
%defattr(-,root,root,-)
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

%changelog
* Thu May 25 2017 Paul Osmialowski <pawel.osmialowski@foss.arm.com> - 4.0.0-1
- add Fortran language frontend and runtime libraries (Flang)
