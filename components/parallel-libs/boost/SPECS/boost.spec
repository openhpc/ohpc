#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Boost C++ library that is is dependent on compiler toolchain and MPI
%global ohpc_compiler_dependent 1
%global ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros
%global pname boost

Summary:        Free peer-reviewed portable C++ source libraries
Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        1.80.0
%define version_exp 1_80_0
Release:        1%{?dist}
License:        Boost
Group:          %{PROJ_NAME}/parallel-libs
Url:            http://www.boost.org
Source0:        https://boostorg.jfrog.io/artifactory/main/release/%{version}/source/boost_%{version_exp}.tar.gz
Patch0:         boost-1.79.0-oneapi_pch.patch

%if 0%{?sle_version} || 0%{?suse_version}
BuildRequires:  libbz2-devel
BuildRequires:  libexpat-devel
BuildRequires:  python3-numpy-devel
%ifarch x86_64
BuildRequires:  libquadmath0
%endif
%else
# Assume RHEL/Fedora/openEuler distro
BuildRequires:  bzip2-devel
BuildRequires:  expat-devel
BuildRequires:  python3-numpy
BuildRequires:  glibc-devel >= 2.28-101
%ifarch x86_64
BuildRequires:  libquadmath-devel
%endif
%endif

BuildRequires:  make
BuildRequires:  fdupes
BuildRequires:  dos2unix
BuildRequires:  gmp-devel
BuildRequires:  python3-devel
BuildRequires:  libstdc++-devel
BuildRequires:  libicu-devel
BuildRequires:  xz-devel
BuildRequires:  zlib-devel

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default install paths
%global install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version
%global module_path %{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}

%description
Boost provides free peer-reviewed portable C++ source libraries.

Boost emphasizes libraries that work well with the C++ Standard Library
and are intended to be widely useful, and usable across a broad spectrum
of applications. The Boost license encourages the use of Boost libraries
for all users with minimal restrictions.


%prep
%setup -q -n %{pname}_%{version_exp}
%patch0 -p1


%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm1"
%global toolset clang
%else
%if "%{compiler_family}" == "intel"
%global toolset intel-linux
%else
%global toolset gcc
%endif
%endif

%if "%{compiler_family}" == "arm1"
which_armclang=$(which armclang)
where_armclang=$(dirname ${which_armclang})
export PATH=${where_armclang}/../llvm-bin:$PATH
%endif

%if "%{mpi_family}" == "impi" && "%{compiler_family}" == "intel"
export CC=mpiicc
export CXX=mpiicpc
%else
export CC=mpicc
export CXX=mpicxx
%endif
export MPICC=$CC
export MPICXX=$CXX

export RPM_OPT_FLAGS="$RPM_OPT_FLAGS -fno-strict-aliasing -Wno-unused-local-typedefs -Wno-deprecated-declarations"
export RPM_LD_FLAGS

cat << "EOF" >> rpm-config.jam
%if 0%{?rhel} >= 9 || 0%{?openEuler}
using python : %{python3_version} : %{__python3} : /usr/include/python%{python3_version} ;
%else
using python : %{python3_version} : %{__python3} : /usr/include/python%{python3_version}m ;
%endif
%if "%{compiler_family}" == "gnu9" || "%{compiler_family}" == "gnu12"
import os ;
local RPM_OPT_FLAGS = [ os.environ RPM_OPT_FLAGS ] ;
local RPM_LD_FLAGS = [ os.environ RPM_LD_FLAGS ] ;
using gcc : : : <compileflags>$(RPM_OPT_FLAGS) <linkflags>$(RPM_LD_FLAGS) ;
%endif
using mpi : $MPICXX ;
EOF

# Generate b2
./bootstrap.sh --with-libraries=all \
               --prefix=%{install_path} \
               --libdir=lib \
               --with-python=python3 \
               --with-toolset=%{toolset} || cat bootstrap.log

# Perform the compilation
./b2 -d2 -q %{?_smp_mflags} --user-config=./rpm-config.jam \
     address-model="64" \
%ifarch aarch64 %{arm}
     architecture="arm" \
%else
     architecture="x86" \
%endif
     threading="multi" \
     link="shared" \
     runtime-link="shared" \
     variant="release" \
     toolset=%{toolset}


%install
%ohpc_setup_compiler

mkdir -p %{buildroot}/%{_docdir}

%if "%{compiler_family}" == "arm1"
which_armclang=$(which armclang)
where_armclang=$(dirname ${which_armclang})
export PATH=${where_armclang}/../llvm-bin:$PATH
%endif

%if "%{mpi_family}" == "impi" && "%{compiler_family}" == "intel"
export CC=mpiicc
export CXX=mpiicpc
%else
export CC=mpicc
export CXX=mpicxx
%endif
export MPICC=$CC
export MPICXX=$CXX

./b2 %{?_smp_mflags} --user-config=./rpm-config.jam \
     --prefix=%{buildroot}/%{install_path} \
     link="shared" \
     runtime-link="shared" \
     variant="release" \
     toolset=%{toolset} install

rm -rf %{buildroot}%{install_path}/lib/cmake

mkdir -p %{buildroot}%{module_path}
cat << EOF > %{buildroot}%{module_path}/%{version}%{OHPC_CUSTOM_PKG_DELIM}.lua
help([[
This module loads the %{pname} library built with the %{compiler_family}
compiler toolchain and the %{mpi_family} MPI stack.

Version %{version}
]])

whatis("Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI")
whatis("Version: %{version}")
whatis("Category: runtime library")
whatis("Description: %{summary}")
whatis("%{url}")

prepend_path("INCLUDE", "%{install_path}/include")
prepend_path("LD_LIBRARY_PATH", "%{install_path}/lib")

setenv("%{PNAME}_DIR", "%{install_path}")
setenv("%{PNAME}_ROOT", "%{install_path}")
setenv("%{PNAME}_LIB", "%{install_path}/lib")
setenv("%{PNAME}_INC", "%{install_path}/include")

family("boost")

EOF

ln -s %{version}%{OHPC_CUSTOM_PKG_DELIM}.lua %{buildroot}%{module_path}/default


%files
%{install_path}
%{module_path}
%doc INSTALL
%license LICENSE_1_0.txt
