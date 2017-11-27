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

# Serial HDF5 library build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname hdf5
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   A general purpose library and file format for storing scientific data
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   1.10.1
Release:   1%{?dist}
License:   Hierarchical Data Format (HDF) Software Library and Utilities License
Group:     %{PROJ_NAME}/io-libs
URL:       http://www.hdfgroup.org/HDF5
DocDir:    %{OHPC_PUB}/doc/contrib
Source0:   https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/%{pname}-%{version}/src/%{pname}-%{version}.tar.bz2
Source1:   OHPC_macros

BuildRequires: zlib-devel
Requires:      zlib-devel

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

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

%build

# override with newer config.guess for aarch64
%ifarch aarch64 || ppc64le
cp /usr/lib/rpm/config.guess bin
%endif

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

./configure --prefix=%{install_path} \
	    --enable-fortran         \
            --enable-static=no       \
	    --enable-shared          \
	    --enable-cxx             \
	    --enable-fortran2003    || { cat config.log && exit 1; }

make %{?_smp_mflags}

%install

# OpenHPC compiler designation
%ohpc_setup_compiler

export NO_BRP_CHECK_RPATH=true

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f
find "%buildroot"

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
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

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "hdf5"

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc COPYING
%doc README.txt

%changelog
* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 1.10.0-1
- switch to ohpc_compiler_dependent flag

* Tue Feb 21 2017 Adrian Reber <areber@redhat.com> - 1.8.17-1
- Switching to %%ohpc_compiler macro
