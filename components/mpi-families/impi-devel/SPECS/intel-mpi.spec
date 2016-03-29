#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define compiler_family intel

Summary:   Intel(R) MPI Library for Linux* OS
Name:      intel-mpi-devel%{PROJ_DELIM}
Version:   5.1.3.181
Source0:   intel-impi-devel%{PROJ_DELIM}-%{version}.tar.gz
Source1:   OHPC_macros
Release:   1
License:   Intel Copyright 1999-2016
URL:       https://software.intel.com/en-us/intel-mpi-library
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no
#AutoReqProv: no

# 09/16/15 karl.w.schulz@intel.com - patch to enable gfortran 5.2.x MPI module support
Source2:   gfortran_support_fix.patch

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

requires: intel-mpi%{PROJ_DELIM}

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%define package_target %{OHPC_COMPILERS}/intel

%description

OpenHPC collection of the Intel(R) MPI toolchain.

%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz %{SOURCE0}

# OpenHPC patches
# %%{__patch} -p1 < %{SOURCE2}

cd -

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%changelog

