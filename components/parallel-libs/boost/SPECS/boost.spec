#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#-------------------------------------------------------------------------------
# Copyright (c) 2015 SUSE LINUX GmbH, Nuernberg, Germany.
# Copyright (c) 2015, Intel Corporation
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.
#
#
#-------------------------------------------------------------------------------

# Boost C++ library that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%define _unpackaged_files_terminate_build 0
%define build_mpi 1

# Base package name
%define pname boost
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:	Boost free peer-reviewed portable C++ source libraries
Name:		%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        1.65.1

%define version_exp 1_65_1

Release:        1%{?dist}
License:        BSL-1.0
Group:		%{PROJ_NAME}/parallel-libs
Url:            http://www.boost.org
Source0:        http://sourceforge.net/projects/boost/files/boost/%{version}/boost_%{version_exp}.tar.gz
Source1:        boost-rpmlintrc
Source2:        mkl_boost_ublas_gemm.hpp
Source3:        mkl_boost_ublas_matrix_prod.hpp
Source100:      baselibs.conf
Source101:	OHPC_macros

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

%build
%ohpc_load_modules

%if %build_mpi
export CC=mpicc
export CXX=mpicxx
export F77=mpif77
export FC=mpif90
export MPICC=mpicc
export MPIFC=mpifc
export MPICXX=mpicxx
%endif


LIBRARIES_FLAGS=--with-libraries=all
./bootstrap.sh $LIBRARIES_FLAGS --prefix=%{install_path} --with-toolset=${toolset} || cat bootstrap.log

%if %build_mpi
cat << EOF >>user-config.jam
using mpi : mpicxx ;
EOF
%endif

# perform the compilation
./b2 %{?_smp_mflags} threading=multi link=shared variant=release --prefix=%{install_path} --user-config=./user-config.jam


%install
%ohpc_load_modules

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
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc INSTALL LICENSE_1_0.txt

%changelog
* Tue May 23 2017 Adrian Reber <areber@redhat.com> - 1.63.0-1
- Remove separate mpi setup; it is part of the %%ohpc_compiler macro

* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 1.63.0-0
- switch to use of ohpc_compiler_dependent and ohpc_mpi_dependent flags

* Mon Feb 20 2017 Adrian Reber <areber@redhat.com> - 1.61.0-0
- Switching to %%ohpc_compiler macro
