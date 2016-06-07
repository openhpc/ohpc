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

%define pname intel-compilers-devel

Summary:   Intel(R) Parallel Studio XE
Name:      %{pname}%{PROJ_DELIM}
Version:   16.2.181
Release:   1
License:   Intel(R)
URL:       https://software.intel.com/en-us/intel-parallel-studio-xe
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
Source0:   intel-compilers-devel%{PROJ_DELIM}-16.0.2-181.tar.gz
Source1:   OHPC_macros
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq: no

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

requires: gcc-c++
requires: intel-compilers%{PROJ_DELIM}

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%define composer_release compilers_and_libraries_20%{version}
%define package_target %{OHPC_COMPILERS}/intel

%define package_version %{version}

%description

OpenHPC collection of development packages for Intel(R) Parallel Studio
compiler suite (including compilers for C,C++, and Fortran).

%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz %{SOURCE0}
cd -

export NO_BRP_CHECK_RPATH true

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%changelog

