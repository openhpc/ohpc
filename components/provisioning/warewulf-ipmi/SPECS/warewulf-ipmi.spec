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

%define dname ipmi
%define pname warewulf-%{dname}
%define wwpkgdir /srv/warewulf

%if 0%{?sle_version} >=150000 || 0%{?rhel_version} >= 800 || 0%{?centos_version} >= 800
%define localipmi 1
%define configopt_ipmi "--with-local-ipmitool"
%else
%define localipmi 0
%endif

Name: %{pname}%{PROJ_DELIM}
Summary: IPMI Module for Warewulf
Version: 3.9.0
Release: .1%{?dist}
License: US Dept. of Energy (BSD-like)
Group: %{PROJ_NAME}/provisioning
URL: http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/development.tar.gz
Requires: warewulf-common%{PROJ_DELIM}
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: openssl-devel
BuildRequires: warewulf-common%{PROJ_DELIM}
%if %{localipmi}
Requires: ipmitool
%endif
Conflicts: warewulf < 3.9
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
%setup -n warewulf3-development

%build
cd %{dname}
NO_CONFIGURE=1 ./autogen.sh
%configure --localstatedir=%{wwpkgdir} %{?configopt_ipmi}

%{__make} %{?_smp_mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/NEWS %{dname}/README %{dname}/TODO
%{wwpkgdir}/*
%if !%{localipmi}
%{_libexecdir}/warewulf/ipmitool
%endif
%{perl_vendorlib}/Warewulf/Ipmi.pm
%{perl_vendorlib}/Warewulf/Module/Cli/*

%files -n %{pname}-initramfs-%{_arch}%{PROJ_DELIM}
%{wwpkgdir}/initramfs/%{_arch}/capabilities/setup-ipmi
