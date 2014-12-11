%{!?_rel:%{expand:%%global _rel 0.r%(test "1686" != "0000" && echo "1686" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}
%include %{_sourcedir}/FSP_macros
%define pname warewulf-ipmi
%define debug_package %{nil}
%define wwpkgdir /srv/warewulf

%if 0%{?PROJ_NAME:1}
%define rpmname %{pname}-%{PROJ_NAME}
%else
%define rpmname %{pname}
%endif

Name: %{rpmname}
Summary: IPMI Module for Warewulf
Version: 3.6
Release: %{_rel}%{?dist}
#Release: 1%{?dist}
License: US Dept. of Energy (BSD-like)
Group: System Environment/Clustering
URL: http://warewulf.lbl.gov/
Source0: %{pname}-%{version}.tar.gz
Source1: FSP_macros
ExclusiveOS: linux
Requires: warewulf-common-fsp
BuildRequires: warewulf-common-fsp
Conflicts: warewulf < 3
BuildConflicts: post-build-checks
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the IPMI module package.  It contains Warewulf modules for
adding IPMI functionality.


%prep
%setup -n %{pname}-%{version}


%build
%configure --localstatedir=%{wwpkgdir}
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root)
%doc AUTHORS COPYING ChangeLog INSTALL NEWS README TODO
%{wwpkgdir}/*
%{_libexecdir}/warewulf/ipmitool
%{perl_vendorlib}/Warewulf/Ipmi.pm
%{perl_vendorlib}/Warewulf/Module/Cli/*


%changelog
