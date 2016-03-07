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

%define pname compute-image

Summary:   Baseline compute image for use with OpenHPC
Name:      %{pname}%{PROJ_DELIM}
Version:   1.1
Release:   1
License:   Various
Group:     ohpc/admin
URL:       https://github.com/openhpc/ohpc
Source1:   OHPC_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%define installPath %{OHPC_ADMIN}/images/centos7

BuildRequires: sudo
BuildRequires: yum-utils
BuildRequires: warewulf-cluster%{PROJ_DELIM}
BuildRequires: warewulf-vnfs%{PROJ_DELIM}

%define __spec_install_post %{nil}
%define debug_package %{nil}
%define __os_install_post %{_dbpath}/brp-compress

%description

This package provides a baseline compute image for use with OpenHPC that can be
imported into the Warewulf provisioning system. 

%prep

%build


%install
rm -rf $RPM_BUILD_ROOT
%{__mkdir_p} %{buildroot}/%{installPath}

sudo su - whoami
wwmkchroot centos-7 %{buildroot}/%{installPath}


%clean
rm -rf $RPM_BUILD_ROOT

%post

%postun

%files
%defattr(-,root,root,-)

%{installPath}



