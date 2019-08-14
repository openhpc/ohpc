#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# OpenBLAS build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname openblas

Name:           %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        0.3.6
Release:        1%{?dist}
Summary:        An optimized BLAS library based on GotoBLAS2
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/serial-libs
Url:            http://www.openblas.net
Source0:        https://github.com/xianyi/OpenBLAS/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Patch1:         openblas-libs.patch
# PATCH-FIX-UPSTREAM openblas-noexecstack.patch
Patch2:         openblas-noexecstack.patch
# PATCH-FIX-UPSTREADM fix-arm64-cpuid-return.patch
Patch3:         fix-arm64-cpuid-return.patch
ExclusiveArch:  %ix86 ia64 ppc ppc64 ppc64le x86_64 aarch64

%description
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%prep
%setup -q -n OpenBLAS-%{version}

%patch1 -p1
%patch2 -p1
%patch3 -p1

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

# Only *86 CPUs support DYNAMIC_ARCH
%ifarch %ix86 x86_64
%define openblas_target DYNAMIC_ARCH=1 NUM_THREADS=256
%endif
# Temporary fix, OpenBLAS does not autodetect aarch64
%ifarch aarch64
%define openblas_target TARGET=ARMV8 NUM_THREADS=256
%define nbjobs_option MAKE_NB_JOBS=4
%endif

make %{?openblas_target} USE_THREAD=1 USE_OPENMP=1 %{?nbjobs_option} \
        PREFIX=%{buildroot}%{install_path}

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

make   %{?openblas_target} PREFIX=%{buildroot}%{install_path} install

# Delete info about host cpu
%ifarch %ix86 x86_64
sed -i '/#define OPENBLAS_NEEDBUNDERSCORE/,/#define OPENBLAS_VERSION/{//!d}' %{buildroot}%{install_path}/include/openblas_config.h
%endif

# Remove buildroot
sed -i 's|%{buildroot}||g' %{buildroot}%{install_path}/lib/cmake/openblas/OpenBLASConfig.cmake
sed -i 's|%{buildroot}||g' %{buildroot}%{install_path}/lib/pkgconfig/openblas.pc

# Remove static lib
rm -f %{buildroot}%{install_path}/lib/*a

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "openblas"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%{OHPC_PUB}
%doc BACKERS.md Changelog.txt CONTRIBUTORS.md GotoBLAS_00License.txt GotoBLAS_01Readme.txt GotoBLAS_02QuickInstall.txt GotoBLAS_03FAQ.txt GotoBLAS_04FAQ.txt GotoBLAS_05LargePage.txt GotoBLAS_06WeirdPerformance.txt LICENSE README.md TargetList.txt
