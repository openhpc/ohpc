#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%global ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

%global pname plasma

Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Version: 24.8.7
Release: 1%{?dist}
Summary: Parallel Linear Algebra Software for Multicore Architectures
License: BSD-3-Clause
Group:   %{PROJ_NAME}/serial-libs
URL:     https://github.com/icl-utk-edu/plasma/
Source0: https://github.com/icl-utk-edu/plasma/releases/download/%{version}/plasma-%{version}.tar.gz
Patch0:  https://github.com/icl-utk-edu/plasma/pull/48.patch
# Tell cmake to include the Fortran plasma_mod.o in the shared object
Patch1:  cmake-fortran.patch
# Exclude functions from plasma_mod.o which do not seem to be implemented.
Patch2:  fortran_gen.patch

#!BuildIgnore: post-build-checks

BuildRequires: cmake%{PROJ_DELIM}
BuildRequires: python3
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
%setup -q -n %{pname}-%{version}
%patch -P 0 -p 1
%patch -P 1 -p 1
%patch -P 2 -p 1

%build
%ohpc_setup_compiler

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
%endif

module load cmake

# Generate fortran bindings
python3 tools/codegen.py -p s -p d -p c include/*h
python3 tools/fortran_gen.py --prefix include/ include/plasma*h
# Create plasma.mod
${FC} -fPIC -c -o include/plasma_mod.o include/plasma_mod.f90

mkdir build
cd build
cmake \
	-DPLASMA_DETECT_LUA=1 \
	-DCMAKE_C_FLAGS="$CFLAGS" \
	-DCMAKE_INSTALL_PREFIX=%{install_path} \
%if "%{compiler_family}" == "intel"
	-DCMAKE_EXE_LINKER_FLAGS_INIT="-lifcore" \
%endif
	..

# The build system defines PLASMA_USE_LUA twice. In the header and on the
# command-line. Let's remove one of those.
sed "/PLASMA_USE_LUA/d" -i ../include/plasma_config.h

cmake --build . -v %{?_smp_mflags}

%install
%ohpc_setup_compiler

module load cmake
cd build
DESTDIR="%{buildroot}" cmake --install .
cd ..

# Needed for compatibility with older versions shipped by OpenHPC.
install -m 644 include/core_lapack*h %{buildroot}%{install_path}/include

# Install fortran bindings
install -m 644 plasma.mod %{buildroot}%{install_path}/include

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
prepend_path( "INCLUDE",         "%{install_path}/include")
prepend_path( "LD_LIBRARY_PATH", "%{install_path}/lib64")

setenv("%{PNAME}_DIR", "%{install_path}")
setenv("%{PNAME}_LIB", "%{install_path}/lib64")
setenv("%{PNAME}_INC", "%{install_path}/include")

EOF

ln -s %{version}%{OHPC_CUSTOM_PKG_DELIM}.lua %{buildroot}%{module_path}/default


%files
%{install_path}
%{module_path}
%license LICENSE
%doc README.md
