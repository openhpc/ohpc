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
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%define _unpackaged_files_terminate_build 0
%define build_mpi 1

# Base package name
%define pname boost

Summary:	Boost free peer-reviewed portable C++ source libraries
Name:		%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        1.70.0

%define version_exp 1_70_0

Release:        1%{?dist}
License:        BSL-1.0
Group:          %{PROJ_NAME}/parallel-libs
Url:            http://www.boost.org
Source0:        https://dl.bintray.com/boostorg/release/%{version}/source/boost_%{version_exp}.tar.gz
Source1:        boost-rpmlintrc
Source2:        mkl_boost_ublas_gemm.hpp
Source3:        mkl_boost_ublas_matrix_prod.hpp
Source100:      baselibs.conf
%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
%if 0%{?sle_version} || 0%{?suse_version}
Patch1:         boost_fenv_suse.patch
%endif
%endif

# https://github.com/boostorg/mpi/pull/81
Patch2:        https://src.fedoraproject.org/rpms/boost/raw/master/f/boost-1.69-mpi-c_data.patch

# optflag patch from Fedora
Patch3: https://src.fedoraproject.org/rpms/boost/raw/master/f/boost-1.66.0-bjam-build-flags.patch
Patch4: https://src.fedoraproject.org/rpms/boost/raw/master/f/boost-1.66.0-build-optflags.patch

%if 0%{?rhel_version} || 0%{?centos_version} || 0%{?rhel}
BuildRequires:  bzip2-devel
BuildRequires:  expat-devel
BuildRequires:  xorg-x11-server-devel
%else
BuildRequires:  libbz2-devel
BuildRequires:  libexpat-devel
BuildRequires:  xorg-x11-devel
%endif
BuildRequires:  libicu-devel >= 4.4
BuildRequires:  python-devel
BuildRequires:  zlib-devel

# (Tron: 3/4/16) Add libicu dependency for SLES12sp1 as the distro does not seem to have it by default and some tests are failing
%if 0%{?suse_version}
Requires: libicu-devel >= 4.4
%endif

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Boost provides free peer-reviewed portable C++ source libraries. The
emphasis is on libraries that work well with the C++ Standard Library.
One goal is to establish "existing practice" and provide reference
implementations so that the Boost libraries are suitable for eventual
standardization. Some of the libraries have already been proposed for
inclusion in the C++ Standards Committee's upcoming C++ Standard
Library Technical Report.

Although Boost was begun by members of the C++ Standards Committee
Library Working Group, membership has expanded to include nearly two
thousand members of the C++ community at large.

This package is mainly needed for updating from a prior version, the
dynamic libraries are found in their respective package. For development
using Boost, you also need the boost-devel package. For documentation,
see the boost-doc package.


%prep
%setup -q -n %{pname}_%{version_exp}

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
%if 0%{?sle_version} || 0%{?suse_version}
%patch1 -p1
%endif
%endif

%patch2 -p2

# optflag patches from Fedora
%patch3 -p1
%patch4 -p1

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
export toolset=clang
%endif

%if "%{compiler_family}" == "arm"
which_armclang=`which armclang`
where_armclang=`dirname ${which_armclang}`
export PATH=${where_armclang}/../llvm-bin:$PATH
%endif

%if %build_mpi
export CC=mpicc
export CXX=mpicxx
export F77=mpif77
export FC=mpif90
export MPICC=mpicc
export MPIFC=mpifc
export MPICXX=mpicxx
%endif

export RPM_OPT_FLAGS="$RPM_OPT_FLAGS -fno-strict-aliasing -Wno-unused-local-typedefs -Wno-deprecated-declarations"
export RPM_LD_FLAGS

cat << "EOF" >> user-config.jam
%if "%{compiler_family}" == "gnu8"
import os ;
local RPM_OPT_FLAGS = [ os.environ RPM_OPT_FLAGS ] ;
local RPM_LD_FLAGS = [ os.environ RPM_LD_FLAGS ] ;
using gcc : : : <compileflags>$(RPM_OPT_FLAGS) <linkflags>$(RPM_LD_FLAGS) ;
%endif
%if %build_mpi
using mpi : mpicxx ;
%endif
EOF

LIBRARIES_FLAGS=--with-libraries=all
./bootstrap.sh $LIBRARIES_FLAGS --prefix=%{install_path} --with-toolset=${toolset} || cat bootstrap.log

# perform the compilation
./b2 -d+2 -q %{?_smp_mflags} threading=multi link=shared variant=release --prefix=%{install_path} --user-config=./user-config.jam

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "arm"
which_armclang=`which armclang`
where_armclang=`dirname ${which_armclang}`
export PATH=${where_armclang}/../llvm-bin:$PATH
%endif

%if %build_mpi
export CC=mpicc
export CXX=mpicxx
export F77=mpif77
export FC=mpif90
export MPICC=mpicc
export MPIFC=mpifc
export MPICXX=mpicxx
%endif

./b2 %{?_smp_mflags} install threading=multi link=shared --prefix=%{buildroot}/%{install_path} --user-config=./user-config.jam


# OpenHPC module file
%if %build_mpi
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
%else
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
%endif
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain"
puts stderr "and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version             %{version}


prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_ROOT       %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "boost"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc INSTALL LICENSE_1_0.txt
