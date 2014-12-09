%{!?_rel:%{expand:%%global _rel 0.r%(test "1723" != "0000" && echo "1723" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}
%include %{_sourcedir}/FSP_macros
%define pname warewulf-nhc
%define debug_package %{nil}


%{!?nhc_script_dir:%global nhc_script_dir %{_sysconfdir}/%{pname}/scripts}
%{!?nhc_helper_dir:%global nhc_helper_dir %{_libexecdir}/%{pname}}

Name: %{pname}
Summary: Warewulf Node Health Check System
Version: 1.4
Release: 1%{?dist}
License: US Dept. of Energy (BSD-like)
Group: Applications/System
URL: http://warewulf.lbl.gov/
Source0: %{pname}-%{version}.tar.gz
Source1: FSP_macros
Packager: %{?_packager}%{!?_packager:Michael Jennings <mej@lbl.gov>}
Vendor: %{?_vendorinfo}%{!?_vendorinfo:Warewulf Project (http://warewulf.lbl.gov/)}
Distribution: %{?_distribution:%{_distribution}}%{!?_distribution:%{_vendor}}
Requires: bash
BuildConflicts: post-build-checks
BuildArch: noarch
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root

%description
This package contains the Warewulf Node Health Check system.

TORQUE (and other resource managers) allow for the execution of a
script to determine if a node is "healthy" or "unhealthy" and
potentially mark unhealthy nodes as unavailable.  The scripts
contained in this package provide a flexible, extensible mechanism for
collecting health checks to be run on your cluster and specifying
which checks should be run on which nodes.


%prep
%setup -n %{pname}-%{version}


%build
%{configure}
%{__make} %{?mflags}


%install
umask 0077
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%check
%{__make} test


%clean
test "$RPM_BUILD_ROOT" != "/" && %{__rm} -rf $RPM_BUILD_ROOT


%files
%defattr(-, root, root)
%doc COPYING ChangeLog LICENSE nhc.conf contrib/nhc.cron
%dir %{_sysconfdir}/%{pname}/
%dir %{_localstatedir}/lib/%{pname}/
%dir %{_localstatedir}/run/%{pname}/
%dir %{nhc_script_dir}/
%dir %{nhc_helper_dir}/
%config(noreplace) %{_sysconfdir}/%{pname}/%{pname}.conf
%config(noreplace) %{_sysconfdir}/logrotate.d/%{pname}
%config(noreplace) %{nhc_script_dir}/*.nhc
%config(noreplace) %{nhc_helper_dir}/*
%config(noreplace) %{_sbindir}/%{pname}
