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

# Build that is dependent on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%define _unpackaged_files_terminate_build 0

# Base package name
%define pname hdf5
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   A general purpose library and file format for storing scientific data
Name:      p%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   1.10.2
Release:   1%{?dist}
License:   Hierarchical Data Format (HDF) Software Library and Utilities License
Group:     %{PROJ_NAME}/io-libs
URL:       http://www.hdfgroup.org/HDF5

Source0:   https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/%{pname}-%{version}/src/%{pname}-%{version}.tar.bz2
Source1:   OHPC_macros
Patch0:    h5cc.patch
Patch1:    h5fc.patch
Patch2:    h5cxx.patch

BuildRequires: zlib-devel

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
HDF5 is a general purpose library and file format for storing scientific data.
HDF5 can store two primary objects: datasets and groups. A dataset is
essentially a multidimensional array of data elements, and a group is a
structure for organizing objects in an HDF5 file. Using these two basic
objects, one can create and store almost any kind of scientific data
structure, such as images, arrays of vectors, and structured and unstructured
grids. You can also mix and match them in HDF5 files according to your needs.

%prep

%setup -q -n %{pname}-%{version}
%patch0 -p0
%patch1 -p0
%patch2 -p0

%build

# override with newer config.guess for aarch64
%ifarch aarch64 || ppc64le
cp /usr/lib/rpm/config.guess bin
%endif

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

export CC=mpicc
export CXX=mpicxx
export F77=mpif77
export FC=mpif90
export MPICC=mpicc
export MPIFC=mpifc
export MPICXX=mpicxx

./configure --prefix=%{install_path} \
	    --enable-fortran         \
            --enable-static=no       \
            --enable-parallel        \
	    --enable-shared          \
	    --enable-fortran2003     || { cat config.log && exit 1; }

%install

# OpenHPC compiler designation
%ohpc_setup_compiler

export NO_BRP_CHECK_RPATH=true

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/p%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/p%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the parallel %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    CPATH               %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include

family "hdf5"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/p%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc COPYING
%doc README.txt
