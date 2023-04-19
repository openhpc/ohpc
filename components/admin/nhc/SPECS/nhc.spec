#---------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname nhc

%{!?nhc_script_dir:%global nhc_script_dir %{_sysconfdir}/%{pname}/scripts}
%{!?nhc_helper_dir:%global nhc_helper_dir %{_libexecdir}/%{pname}}

Summary: LBNL Node Health Check
Name: %{pname}%{PROJ_DELIM}
Version: 1.4.3
Release: 1%{?dist}
License: US Dept. of Energy (BSD-like)
Group: %{PROJ_NAME}/admin
URL: https://github.com/mej/nhc/
Source0: https://github.com/mej/nhc/archive/%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Patch0: nhc-ps-chronyd.patch
Requires: bash
BuildRequires: automake autoconf
BuildRequires: make
BuildArch: noarch

%description
TORQUE (and other resource managers) allow for the execution of a
script to determine if a node is "healthy" or "unhealthy" and
potentially mark unhealthy nodes as unavailable.  The scripts
contained in this package provide a flexible, extensible mechanism for
collecting health checks to be run on your cluster and specifying
which checks should be run on which nodes.


%prep
%setup -q -n %{pname}-%{version}
%patch -P 0 -p1

%build
if [ ! -f configure ]; then
  ./autogen.sh
fi
%{configure}
%{__make} %{?_smp_mflags}


%install
umask 0077
%{__make} install DESTDIR=$RPM_BUILD_ROOT

# tweak ssh check for Leap
%if 0%{?suse_version}
perl -pi -e "s/check_ps_service -u root -S sshd/check_ps_service -m 'sshd:' -u root -S sshd/" $RPM_BUILD_ROOT/etc/nhc/nhc.conf
%endif

%files
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
%config(noreplace) %{_sbindir}/%{pname}-genconf
%config(noreplace) %{_sbindir}/%{pname}-wrapper
