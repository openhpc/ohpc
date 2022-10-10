#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# plasma - Parallel Linear Algebra Software for Multicore Architectures

%global ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

%global pname plasma
%global lapack_ver 3.10.1

Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Version: 21.8.29
Release: 1%{?dist}
Summary: Parallel Linear Algebra Software for Multicore Architectures
License: BSD-3-Clause
Group:   %{PROJ_NAME}/serial-libs
URL:     https://icl.utk.edu/plasma/overview/index.html
Source0: https://github.com/icl-utk-edu/plasma/releases/download/%{version}/plasma-%{version}.tar.gz
Source1: https://github.com/Reference-LAPACK/lapack/archive/refs/tags/v%{lapack_ver}.tar.gz
Source2: make.inc
Patch0:  plasma-21.8.29-configure.patch
Patch1:  plasma-21.8.29-tools.patch
Patch2:  plasma-21.8.29-makefile.patch

#!BuildIgnore: post-build-checks

BuildRequires: python3
BuildRequires: doxygen
BuildRequires: make
BuildRequires: sed
BuildRequires: lua-devel >= 5.3
Requires: lua >= 5.3
Requires: lmod%{PROJ_DELIM} >= 8.7.3
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Default library install path
%global install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version
%global module_path %{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}

%description
PLASMA is a software package for solving problems in dense linear algebra
using OpenMP. PLASMA provides implementations of state-of-the-art
algorithms using cutting-edge task scheduling techniques. PLASMA currently
offers a collection of routines for solving linear systems of equations,
least squares problems, eigenvalue problems, and singular value problems.


%prep
%setup -q -a 1 -n %{pname}-%{version}
cp %{SOURCE2} .
# Convert scripts to Python3; clean up indentation first
# Patches created using 2to3
sed -i "s/\t/    /g;s/^\s*$//;1s|^#!.*env.*python.*$|#!/usr/bin/python3|" \
    configure.py config/*.py tools/*.py
%patch0 -p1
%patch1 -p1
%patch2 -p1

%build
%ohpc_setup_compiler

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
%endif

make prefix=%{install_path} \
%if 0%{?sle_version} || 0%{?suse_version}
     lua_dir=/usr/include/lua%{luaver} \
%else
     lua_dir=/usr/include \
%endif
%if "%{compiler_family}" == "intel"
   CFLAGS="-fPIC -std=c99 -I${MKLROOT}/include -fopenmp \
     -DHAVE_OPENMP_DEPEND -DHAVE_OPENMP_PRIORITY -DHAVE_MKL \
     -DBLAS_RETURN_COMPLEX_AS_ARGUMENT -DHAVE_LAPACKE_DLASCL -DHAVE_LAPACKE_DLANTR" \
   FCFLAGS="-fPIC -std=f2008 -fopenmp" \
   LDFLAGS="-fPIC -L${MKLROOT}/lib/intel64 -L${CMPLR_ROOT}/linux/compiler/lib/intel64_lin -fopenmp" \
   LIBS="-lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lifcore -lm" \
%else
%if "%{compiler_family}" == "arm1"
   CFLAGS="-fPIC -std=c99 -fopenmp \
     -DHAVE_OPENMP_DEPEND -DHAVE_OPENMP_PRIORITY  \
     -DHAVE_LAPACKE_DLASCL -DHAVE_LAPACKE_DLANTR" \
   FCFLAGS="-fPIC -std=f2008 -fopenmp" \
   LDFLAGS="-fPIC -fopenmp" \
   LIBS="-armpl -lm" \
%else
   CFLAGS="-fPIC -std=c99 -I${OPENBLAS_DIR}/include -fopenmp -DHAVE_OPENMP_DEPEND -DHAVE_OPENMP_PRIORITY -DHAVE_OPENBLAS -DHAVE_LAPACKE_DLASCL -DHAVE_LAPACKE_DLANTR -DHAVE_LAPACKE_DLASSQ" \
   FCFLAGS="-fPIC -std=f2008 -fopenmp" \
   LDFLAGS="-fPIC -L${OPENBLAS_DIR}/lib -fopenmp" \
   LIBS="-lopenblas -lm" \
%endif
%endif
     all
make prefix=%{install_path} docs


%install
# Compiler is needed on RHEL for make install
%ohpc_setup_compiler

mkdir -p %{buildroot}%{install_path}
make prefix=%{buildroot}%{install_path} CFLAGS="-fPIC" LDFLAGS="-fPIC" install

# Correct the paths in the config file
sed -i "s|%{buildroot}||" %{buildroot}%{install_path}/lib/pkgconfig/plasma.pc

# Remove static libraries
rm -f %{buildroot}%{install_path}/lib/*.a

# OpenHPC module file
mkdir -p %{buildroot}%{module_path}
cat << EOF > %{buildroot}/%{module_path}/%{version}%{OHPC_CUSTOM_PKG_DELIM}.lua
help([[
This module loads the %{PNAME} library built with the %{compiler_family}
compiler toolchain.
Version %{version}
]])

whatis("Name: %{PNAME} built with %{compiler_family} compiler")
whatis("Version: %{version}")
whatis("Category: runtime library")
whatis("Description: %{summary}")
whatis("URL %{url}")

local version = "%{version}"

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
-- Require openblas for gnu and llvm compiler families
depends_on("openblas")
%endif

prepend_path( "PATH",            "%{install_path}/bin")
prepend_path( "MANPATH",         "%{install_path}/share/man")
prepend_path( "INCLUDE",         "%{install_path}/include")
prepend_path( "LD_LIBRARY_PATH", "%{install_path}/lib")

setenv("%{PNAME}_DIR", "%{install_path}")
setenv("%{PNAME}_LIB", "%{install_path}/lib")
setenv("%{PNAME}_INC", "%{install_path}/include")

EOF

ln -s %{version}%{OHPC_CUSTOM_PKG_DELIM}.lua %{buildroot}%{module_path}/default


%files
%{install_path}
%{module_path}
%license LICENSE
%doc README.md docs/html
