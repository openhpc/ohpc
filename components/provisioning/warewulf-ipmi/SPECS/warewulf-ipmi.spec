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

%define pname warewulf-ipmi
%define dname ipmi
%define wwpkgdir /srv/warewulf

Name: %{pname}%{PROJ_DELIM}
Summary: IPMI Module for Warewulf
Version: 3.8.1
Release: 1%{?dist}
License: US Dept. of Energy (BSD-like)
Group: %{PROJ_NAME}/provisioning
URL: http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/%{version}.tar.gz#/warewulf3-%{version}.tar.gz
Requires: warewulf-common%{PROJ_DELIM}
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: openssl-devel
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3
#!BuildIgnore: post-build-checks

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the IPMI module package.  It contains Warewulf modules for
adding IPMI functionality.


%package -n %{pname}-initramfs-%{_arch}%{PROJ_DELIM}
Summary: Warewulf - IPMI Module - Initramfs IPMI Capabilities for %{_arch}
Group: System Environment/Clustering
BuildArch: noarch
%description -n %{pname}-initramfs-%{_arch}%{PROJ_DELIM}
Warewulf Provisioning initramfs IPMI capabilities for %{_arch}.


%prep
%setup -n warewulf3-%{version}

%build
cd %{dname}
if [ ! -f configure ]; then
    ./autogen.sh
fi
%configure --localstatedir=%{wwpkgdir}
%{__make} %{?_smp_mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/NEWS %{dname}/README %{dname}/TODO
%{wwpkgdir}/*
%{_libexecdir}/warewulf/ipmitool
%{perl_vendorlib}/Warewulf/Ipmi.pm
%{perl_vendorlib}/Warewulf/Module/Cli/*

%files -n %{pname}-initramfs-%{_arch}%{PROJ_DELIM}
%{wwpkgdir}/initramfs/%{_arch}/capabilities/setup-ipmi
