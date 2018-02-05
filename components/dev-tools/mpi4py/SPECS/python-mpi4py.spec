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
# spec file for package mpi4py
#
# Copyright (c) 2017 Kitware, Inc.
#
# mpi4py build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname mpi4py
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%if 0%{?sles_version} || 0%{?suse_version}
%define python3_prefix python3
%else
%define python3_prefix python34
%endif

Name:           %{python3_prefix}-%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        3.0.0
Release:        1%{?dist}
Summary:        Python bindings for the Message Passing Interface (MPI) standard.
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Url:            https://bitbucket.org/mpi4py/mpi4py
Source0:        https://bitbucket.org/mpi4py/mpi4py/downloads/%{pname}-%{version}.tar.gz
Source1:        OHPC_macros
BuildRequires:  %{python3_prefix}-devel
BuildRequires:  %{python3_prefix}-setuptools
#BuildRequires:  python-Cython
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
Requires:       %{python3_prefix}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

#!BuildIgnore: post-build-checks

%description
MPI4Py provides Python bindings for the Message Passing Interface (MPI)
standard. It is implemented on top of the MPI-1/2/3 specification and
exposes an API which grounds on the standard MPI-2 C++ bindings.

%prep
%setup -q -n %{pname}-%{version}
find . -type f -name "*.py" -exec sed -i "s|#!/usr/bin/env python||" {} \;

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%__python3 setup.py build

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%__python3 setup.py install --prefix=%{install_path} --root=%{buildroot}

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{python3_prefix}-%{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI library."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{python3_prefix}-%{pname} built with %{compiler_family} compiler and %{mpi_family}"
module-whatis "Version: %{version}"
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PYTHONPATH          %{install_path}/lib64/python3.4/site-packages

setenv          %{PNAME}_DIR        %{install_path}

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc LICENSE.rst

%changelog
