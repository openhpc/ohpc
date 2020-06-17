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
Version:        1.73.0
Release:        1%{?dist}
License:        BSL-1.0
Group:          %{PROJ_NAME}/parallel-libs
Url:            http://www.boost.org
%global tbname boost_%{lua: print((string.gsub(rpm.expand("%{version}"),"%.","_")))}
Source0:        https://dl.bintray.com/boostorg/release/%{version}/source/%{tbname}.tar.gz
Source1:        boost-rpmlintrc
Source2:        mkl_boost_ublas_gemm.hpp
Source3:        mkl_boost_ublas_matrix_prod.hpp
Source100:      baselibs.conf

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
%if 0%{?sle_version} || 0%{?suse_version}
Patch1:         boost-fenv_suse.patch
%define fenv_patch 1
%endif
%endif

%if 0%{?rhel}
BuildRequires:  bzip2-devel
BuildRequires:  expat-devel
BuildRequires:  xorg-x11-server-devel
BuildRequires: libquadmath-devel
Requires: libquadmath-devel
%else
%if 0%{?sle_version}
BuildRequires:  libbz2-devel
BuildRequires:  libexpat-devel
BuildRequires:  xorg-x11-devel
BuildRequires: libquadmath0
Requires: libquadmath0
%endif
%endif

BuildRequires:  python3-devel
BuildRequires:  python3-lxml
BuildRequires:  python3-numpy-devel
BuildRequires:  m4
BuildRequires:  libicu-devel
Requires: libicu-devel
BuildRequires:  libstdc++-devel
BuildRequires:  zlib-devel

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}%{PROJ_DELIM}/%version

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
%setup -q -n %{tbname}

%if 0%{?fenv_patch}
%patch1 -p1
%endif

#sed -i "s#-ip#-gxx-name=/opt/ohpc/pub/compiler/gcc/9.2.0/bin/g++ -gcc-name=/opt/ohpc/pub/compiler/gcc/9.2.0/bin/gcc#g" tools/build/src/tools/intel-linux.jam

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
export toolset=clang
%else
%if "%{compiler_family}" == "intel"
export toolset=intel-linux
%else
export toolset=gcc
%endif
%endif

%if "%{compiler_family}" == "arm"
which_armclang=`which armclang`
where_armclang=`dirname ${which_armclang}`
export PATH=${where_armclang}/../llvm-bin:$PATH
%endif

%if %build_mpi
export CC=%{mpicc}
export CXX=%{mpicxx}
export F77=%{mpif77}
export FC=%{mpif90}
export MPICC=%{mpicc}
export MPIFC=%{mpifc}
export MPICXX=%{mpicxx}
%endif

export RPM_OPT_FLAGS="$RPM_OPT_FLAGS -fno-strict-aliasing -Wno-unused-local-typedefs -Wno-deprecated-declarations"
export RPM_LD_FLAGS

cat << "EOF" >> user-config.jam
using python : %{python3_version} : %{__python3} : /usr/include/python%{python3_version}m ;
%if "%{compiler_family}" == "gnu9"
import os ;
local RPM_OPT_FLAGS = [ os.environ RPM_OPT_FLAGS ] ;
local RPM_LD_FLAGS = [ os.environ RPM_LD_FLAGS ] ;
using gcc : : : <compileflags>$(RPM_OPT_FLAGS) <linkflags>$(RPM_LD_FLAGS) ;
%endif
%if %{build_mpi}
using mpi : %{mpicxx} ;
%endif
EOF

# Generate b2
./bootstrap.sh --with-libraries=all \
               --prefix=%{install_path} \
               --with-python=python3 \
               --with-toolset=${toolset} || cat bootstrap.log

# Perform the compilation
./b2 -d2 -q %{?_smp_mflags} threading=multi link=shared variant=release \
               --prefix=%{install_path} \
               --user-config=./user-config.jam toolset=${toolset}



%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "arm"
which_armclang=`which armclang`
where_armclang=`dirname ${which_armclang}`
export PATH=${where_armclang}/../llvm-bin:$PATH
%endif

%if %build_mpi
export CC=%{mpicc}
export CXX=%{mpicxx}
export F77=%{mpif77}
export FC=%{mpif90}
export MPICC=%{mpicc}
export MPIFC=%{mpifc}
export MPICXX=%{mpicxx}
%endif

./b2 %{?_smp_mflags} install threading=multi link=shared \
               --prefix=%{buildroot}/%{install_path} \
               --user-config=./user-config.jam

%{__rm} -rf %{buildroot}/%{install_path}/lib/cmake

# OpenHPC module file
%if %build_mpi
%global modulename %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}%{PROJ_DELIM}
%else
%global modulename %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}%{PROJ_DELIM}
%endif

%{__mkdir} -p %{dirname: %{modulename}}
%{__cat} << EOF > %{modulename}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler toolchain"
puts stderr "and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{description}"
module-whatis "%{url}"

set             version             %{version}


prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{pname}_DIR        %{install_path}
setenv          %{pname}_ROOT       %{install_path}
setenv          %{pname}_LIB        %{install_path}/lib
setenv          %{pname}_INC        %{install_path}/include

family "boost"
EOF

%{__cat} << EOF > %{dirname: %{modulename}}/.version.%{basename: %{modulename}}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}



%files
%{install_path}
%doc INSTALL
%license LICENSE_1_0.txt
