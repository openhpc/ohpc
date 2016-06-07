#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package openblas
#
# Copyright (c) 2015 SUSE LINUX GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

#-ohpc-header-comp-begin-----------------------------

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency).
%if !0%{?OHPC_BUILD}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
BuildRequires: coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?OHPC_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-ohpc-header-comp-end-------------------------------

# Base package name
%define pname openblas
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        0.2.15
Release:        1
Summary:        An optimized BLAS library based on GotoBLAS2
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/serial-libs
Url:            http://www.openblas.net
Source0:        https://github.com/xianyi/OpenBLAS/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Patch0:         openblas-libs.patch
# PATCH-FIX-UPSTREAM c_xerbla_no-void-return.patch
Patch1:         c_xerbla_no-void-return.patch
# PATCH-FIX-UPSTREAM openblas-noexecstack.patch
Patch2:         openblas-noexecstack.patch
# PATCH-FIX-UPSTREAM openblas-gemv.patch
Patch3:         openblas-gemv.patch
BuildRoot:      %{_tmppath}/%{pname}-%{version}-build
ExclusiveArch:  %ix86 ia64 ppc ppc64 x86_64 aarch64
DocDir:        %{OHPC_PUB}/doc/contrib

%description
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%prep
%setup -q -n OpenBLAS-%{version}

%patch0 -p1
%patch1 -p1
%patch2 -p1
%patch3 -p1

%build
# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

# Only *86 CPUs support DYNAMIC_ARCH
%ifarch %ix86 x86_64
%define openblas_target DYNAMIC_ARCH=1
%endif
# Temporary fix, OpenBLAS does not autodetect aarch64
%ifarch aarch64
%define openblas_target TARGET=ARMV8
%endif

make    %{?openblas_target} USE_THREAD=1 USE_OPENMP=1 \
        PREFIX=%{buildroot}%{install_path}

%install
# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

make   PREFIX=%{buildroot}%{install_path} install 

# Delete info about host cpu
%ifarch %ix86 x86_64
sed -i '/#define OPENBLAS_NEEDBUNDERSCORE/,/#define OPENBLAS_VERSION/{//!d}' %{buildroot}%{install_path}/include/openblas_config.h
%endif

# Remove buildroot
sed -i 's|%{buildroot}||g' %{buildroot}%{install_path}/lib/cmake/openblas/OpenBLASConfig.cmake

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

%post
/sbin/ldconfig || exit 1

%postun -p /sbin/ldconfig

%files
%{OHPC_HOME}
%{OHPC_PUB}
%doc BACKERS.md Changelog.txt CONTRIBUTORS.md GotoBLAS_00License.txt GotoBLAS_01Readme.txt GotoBLAS_02QuickInstall.txt GotoBLAS_03FAQ.txt GotoBLAS_04FAQ.txt GotoBLAS_05LargePage.txt GotoBLAS_06WeirdPerformance.txt LICENSE README.md TargetList.txt

%changelog
