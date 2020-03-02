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
%define wwsrvdir /srv
%define develSHA f5bdc3c9de534472323ef7ebe135c8c2451dc3ca
%define wwextract warewulf3-%{develSHA}

Name:    %{pname}%{PROJ_DELIM}
Version: 3.9.0
Release: 1%{?dist}
Summary: Warewulf - HPC cluster configuration
License: US Dept. of Energy (BSD-like)
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/%{develSHA}.tar.gz
Group:   %{PROJ_NAME}/provisioning
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM}, warewulf-provision%{PROJ_DELIM}
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3

%description
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogenous systems.

Warewulf Cluster contains tools and libraries to facilitate the
configuration of a cluster and its nodes.


%prep
cd %{_builddir}
%{__rm} -rf %{name}-%{version} %{wwextract}
%{__ln_s} %{wwextract}/%{dname} %{name}-%{version}
%setup -q -D


%build
./autogen.sh
%configure --localstatedir=%{wwsrvdir}
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%files
%defattr(-, root, root)
%doc AUTHORS ChangeLog INSTALL NEWS README TODO LICENSE COPYING
%{_sysconfdir}/profile.d/*
%{_bindir}/*
%{_libexecdir}/warewulf/wwinit/*
%{perl_vendorlib}/Warewulf/Module/Cli/*


