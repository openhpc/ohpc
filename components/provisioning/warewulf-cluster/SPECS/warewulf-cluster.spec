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

%define dname cluster
%define pname warewulf-%{dname}

Name:    %{pname}%{PROJ_DELIM}
Summary: Tools used for clustering with Warewulf
Version: 3.9.0
Release: 0.1%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/development.tar.gz
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM} warewulf-provision%{PROJ_DELIM} ntp
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3.9

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This package contains tools to facilitate management of a Cluster
with Warewulf.

%prep
%setup -n warewulf3-development
cd %{dname}

%build
cd %{dname}
NO_CONFIGURE=1 ./autogen.sh
%configure
%{__make} %{?mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/LICENSE %{dname}/NEWS %{dname}/README %{dname}/TODO
%{_sysconfdir}/profile.d/*
%{_bindir}/*
%{_libexecdir}/warewulf/wwinit/*
%{perl_vendorlib}/Warewulf/Module/Cli/*

