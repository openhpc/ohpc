#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/FSP_macros
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Base package name
%define pname nagios-plugins
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%global _hardened_build 1

Name: %{pname}%{PROJ_DELIM}
Version: 2.0.1
Release: 1%{?dist}
Summary: Host/service/network monitoring program plugins for Nagios

Group: Applications/System
License: GPLv2+
URL: https://www.nagios-plugins.org/
Source0: %{pname}-%{version}.tar.gz
Source1: nagios-plugins.README.Fedora
Patch2: nagios-plugins-0002-Remove-assignment-of-not-parsed-to-jitter.patch
Patch3: nagios-plugins-0003-Fedora-specific-fixes-for-searching-for-diff-and-tai.patch
Patch4: nagios-plugins-0004-Fedora-specific-patch-for-not-to-fixing-fully-qualif.patch
# https://bugzilla.redhat.com/512559
Patch5: nagios-plugins-0005-Prevent-check_swap-from-returning-OK-if-no-swap-acti.patch
Patch7: nagios-plugins-0007-Fix-the-use-lib-statement-and-the-external-ntp-comma.patch

%if 0%{?suse_version}
BuildRequires: openldap2-devel
%else
BuildRequires: openldap-devel
%endif
BuildRequires: mysql-devel
BuildRequires: net-snmp-devel
%if 0%{?suse_version}
BuildRequires: net-snmp
%else
BuildRequires: net-snmp-utils
%endif
BuildRequires: samba-client
BuildRequires: postgresql-devel
BuildRequires: gettext
#BuildRequires: %{_bindir}/ssh
%if 0%{?suse_version}
BuildRequires: openssh
%else
BuildRequires: openssh-clients
%endif
BuildRequires: bind-utils
BuildRequires: ntp
#BuildRequires: %{_bindir}/mailq
%if 0%{?suse_version}
BuildRequires: ssmtp
%else
BuildRequires: postfix
%endif
#BuildRequires: %{_sbindir}/fping
BuildRequires: fping
#BuildRequires: perl(Net::SNMP)
BuildRequires: perl-Net-SNMP
%if 0%{?suse_version}
BuildRequires: freeradius-client-devel
%else
BuildRequires: radiusclient-ng-devel
%endif
BuildRequires: qstat
BuildRequires: libdbi-devel

Requires: %{pname}-common%{PROJ_DELIM} >= 3.3.1-1

# nagios-plugins-1.4.16: the included gnulib files were last updated
# in June/July 2010
# Bundled gnulib exception (https://fedorahosted.org/fpc/ticket/174)
Provides: bundled(gnulib)

%global reqfilt sh -c "%{__perl_requires} | sed -e 's!perl(utils)!nagios-plugins-perl!'"
%global __perl_requires %{reqfilt}


%description
Nagios is a program that will monitor hosts and services on your
network, and to email or page you when a problem arises or is
resolved. Nagios runs on a Unix server as a background or daemon
process, intermittently running checks on various services that you
specify. The actual service checks are performed by separate "plugin"
programs which return the status of the checks to Nagios. This package
contains those plugins.

%package all
Summary: Nagios Plugins - All plugins
Group: Applications/System
Requires: nagios-plugins-breeze, nagios-plugins-by_ssh, nagios-plugins-dhcp, nagios-plugins-dig, nagios-plugins-disk, nagios-plugins-disk_smb, nagios-plugins-dns, nagios-plugins-dummy, nagios-plugins-file_age, nagios-plugins-flexlm, nagios-plugins-fping, nagios-plugins-hpjd, nagios-plugins-http, nagios-plugins-icmp, nagios-plugins-ide_smart, nagios-plugins-ircd, nagios-plugins-ldap, nagios-plugins-load, nagios-plugins-log, nagios-plugins-mailq, nagios-plugins-mrtg, nagios-plugins-mrtgtraf, nagios-plugins-mysql, nagios-plugins-nagios, nagios-plugins-nt, nagios-plugins-ntp, nagios-plugins-ntp-perl, nagios-plugins-nwstat, nagios-plugins-oracle, nagios-plugins-overcr, nagios-plugins-pgsql, nagios-plugins-ping, nagios-plugins-procs, nagios-plugins-game, nagios-plugins-real, nagios-plugins-rpc, nagios-plugins-smtp, nagios-plugins-snmp, nagios-plugins-ssh, nagios-plugins-swap, nagios-plugins-tcp, nagios-plugins-time, nagios-plugins-ups, nagios-plugins-users, nagios-plugins-wave, nagios-plugins-cluster
%ifnarch ppc ppc64 ppc64p7 sparc sparc64
Requires: nagios-plugins-sensors
%endif

%description all
This package provides all Nagios plugins.

%package apt
Summary: Nagios Plugin - check_apt
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description apt
Provides check_apt support for Nagios.

%package breeze
Summary: Nagios Plugin - check_breeze
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description breeze
Provides check_breeze support for Nagios.

%package by_ssh
Summary: Nagios Plugin - check_by_ssh
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
#Requires: %{_bindir}/ssh
Requires: openssh-clients

%description by_ssh
Provides check_by_ssh support for Nagios.

%package cluster
Summary: Nagios Plugin - check_cluster
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description cluster
Provides check_cluster support for Nagios.

%package dbi
Summary: Nagios Plugin - check_dbi
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description dbi
Provides check_dbi support for Nagios.

%package dhcp
Summary: Nagios Plugin - check_dhcp
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: group(nagios)
Requires(pre): group(nagios)

%description dhcp
Provides check_dhcp support for Nagios.

%package dig
Summary: Nagios Plugin - check_dig
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_bindir}/dig

%description dig
Provides check_dig support for Nagios.

%package disk
Summary: Nagios Plugin - check_disk
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description disk
Provides check_disk support for Nagios.

%package disk_smb
Summary: Nagios Plugin - check_disk_smb
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_bindir}/smbclient

%description disk_smb
Provides check_disk_smb support for Nagios.

%package dns
Summary: Nagios Plugin - check_dns
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_bindir}/nslookup

%description dns
Provides check_dns support for Nagios.

%package dummy
Summary: Nagios Plugin - check_dummy
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description dummy
Provides check_dummy support for Nagios.
This plugin does not actually check anything, simply provide it with a flag
0-4 and it will return the corresponding status code to Nagios.

%package file_age
Summary: Nagios Plugin - check_file_age
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description file_age
Provides check_file_age support for Nagios.

%package flexlm
Summary: Nagios Plugin - check_flexlm
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description flexlm
Provides check_flexlm support for Nagios.

%package fping
Summary: Nagios Plugin - check_fping
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_sbindir}/fping
Requires: group(nagios)
Requires(pre): group(nagios)

%description fping
Provides check_fping support for Nagios.

%package game
Summary: Nagios Plugin - check_game
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: qstat

%description game
Provides check_game support for Nagios.

%package hpjd
Summary: Nagios Plugin - check_hpjd
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description hpjd
Provides check_hpjd support for Nagios.

%package http
Summary: Nagios Plugin - check_http
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description http
Provides check_http support for Nagios.

%package icmp
Summary: Nagios Plugin - check_icmp
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: group(nagios)
Requires(pre): group(nagios)

%description icmp
Provides check_icmp support for Nagios.

%package ide_smart
Summary: Nagios Plugin - check_ide_smart
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: group(nagios)
Requires(pre): group(nagios)

%description ide_smart
Provides check_ide_smart support for Nagios.

%package ifoperstatus
Summary: Nagios Plugin - check_ifoperstatus
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description ifoperstatus
Provides check_ifoperstatus support for Nagios to monitor network interfaces.

%package ifstatus
Summary: Nagios Plugin - check_ifstatus
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description ifstatus
Provides check_ifstatus support for Nagios to monitor network interfaces.

%package ircd
Summary: Nagios Plugin - check_ircd
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description ircd
Provides check_ircd support for Nagios.

%package ldap
Summary: Nagios Plugin - check_ldap
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description ldap
Provides check_ldap support for Nagios.

%package load
Summary: Nagios Plugin - check_load
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description load
Provides check_load support for Nagios.

%package log
Summary: Nagios Plugin - check_log
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: /bin/egrep
Requires: /bin/mktemp

%description log
Provides check_log support for Nagios.

%package mailq
Summary: Nagios Plugin - check_mailq
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_bindir}/mailq

%description mailq
Provides check_mailq support for Nagios.

%package mrtg
Summary: Nagios Plugin - check_mrtg
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description mrtg
Provides check_mrtg support for Nagios.

%package mrtgtraf
Summary: Nagios Plugin - check_mrtgtraf
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description mrtgtraf
Provides check_mrtgtraf support for Nagios.

%package mysql
Summary: Nagios Plugin - check_mysql
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description mysql
Provides check_mysql and check_mysql_query support for Nagios.

%package nagios
Summary: Nagios Plugin - check_nagios
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description nagios
Provides check_nagios support for Nagios.

%package nt
Summary: Nagios Plugin - check_nt
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description nt
Provides check_nt support for Nagios.

%package ntp
Summary: Nagios Plugin - check_ntp
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description ntp
Provides check_ntp support for Nagios.

%package ntp-perl
Summary: Nagios Plugin - check_ntp.pl
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_sbindir}/ntpdate
Requires: %{_sbindir}/ntpq

%description ntp-perl
Provides check_ntp.pl support for Nagios.

%package nwstat
Summary: Nagios Plugin - check_nwstat
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description nwstat
Provides check_nwstat support for Nagios.

%package oracle
Summary: Nagios Plugin - check_oracle
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description oracle
Provides check_oracle support for Nagios.

%package overcr
Summary: Nagios Plugin - check_overcr
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description overcr
Provides check_overcr support for Nagios.

%package perl
Summary: Nagios plugins perl dep.
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description perl
Perl dep for nagios plugins.  This is *NOT* an actual plugin it simply provides
utils.pm

%package pgsql
Summary: Nagios Plugin - check_pgsql
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description pgsql
Provides check_pgsql (PostgreSQL)  support for Nagios.

%package ping
Summary: Nagios Plugin - check_ping
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: /bin/ping
Requires: /bin/ping6

%description ping
Provides check_ping support for Nagios.

%package procs
Summary: Nagios Plugin - check_procs
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description procs
Provides check_procs support for Nagios.

%package radius
Summary: Nagios Plugin - check_radius
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description radius
Provides check_radius support for Nagios.

%package real
Summary: Nagios Plugin - check_real
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description real
Provides check_real (rtsp) support for Nagios.

%package rpc
Summary: Nagios Plugin - check_rpc
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_sbindir}/rpcinfo

%description rpc
Provides check_rpc support for Nagios.

%ifnarch ppc ppc64 sparc sparc64
%package sensors
Summary: Nagios Plugin - check_sensors
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: /bin/egrep
Requires: %{_bindir}/sensors

%description sensors
Provides check_sensors support for Nagios.
%endif

%package smtp
Summary: Nagios Plugin - check_smtp
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description smtp
Provides check_smtp support for Nagios.

%package snmp
Summary: Nagios Plugin - check_snmp
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Requires: %{_bindir}/snmpgetnext
Requires: %{_bindir}/snmpget

%description snmp
Provides check_snmp support for Nagios.

%package ssh
Summary: Nagios Plugin - check_ssh
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description ssh
Provides check_ssh support for Nagios.

%package swap
Summary: Nagios Plugin - check_swap
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description swap
Provides check_swap support for Nagios.

%package tcp
Summary: Nagios Plugin - check_tcp
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}
Provides: nagios-plugins-ftp = %{version}-%{release}
Provides: nagios-plugins-imap = %{version}-%{release}
Provides: nagios-plugins-jabber = %{version}-%{release}
Provides: nagios-plugins-nntp = %{version}-%{release}
Provides: nagios-plugins-nntps = %{version}-%{release}
Provides: nagios-plugins-pop = %{version}-%{release}
Provides: nagios-plugins-simap = %{version}-%{release}
Provides: nagios-plugins-spop = %{version}-%{release}
Provides: nagios-plugins-ssmtp = %{version}-%{release}
Provides: nagios-plugins-udp = %{version}-%{release}
Provides: nagios-plugins-udp2 = %{version}-%{release}
Obsoletes: nagios-plugins-udp < 1.4.15-2

%description tcp
Provides check_tcp, check_ftp, check_imap, check_jabber, check_nntp,
check_nntps, check_pop, check_simap, check_spop, check_ssmtp, check_udp
and check_clamd support for Nagios.

%package time
Summary: Nagios Plugin - check_time
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description time
Provides check_time support for Nagios.

%package ups
Summary: Nagios Plugin - check_ups
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description ups
Provides check_ups support for Nagios.

%package uptime
Summary: Nagios Plugin - check_uptime
Group: Applications/Systems
Requires: nagios-plugins = %{version}-%{release}

%description uptime
Provides check_uptime support for Nagios.

%package users
Summary: Nagios Plugin - check_users
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description users
Provides check_users support for Nagios.

%package wave
Summary: Nagios Plugin - check_wave
Group: Applications/System
Requires: nagios-plugins = %{version}-%{release}

%description wave
Provides check_wave support for Nagios.

%prep
%setup -q -n %{pname}-%{version}

%patch2 -p1 -b .not_parsed
%patch3 -p1 -b .proper_paths
%patch4 -p1 -b .no_need_fo_fix_paths
%patch5 -p1 -b .fix_missing_swap
%patch7 -p1 -b .ext_ntp_cmds

%build
%configure \
	--libexecdir=%{_libdir}/nagios/plugins \
	--with-dbi \
	--with-mysql \
	PATH_TO_QSTAT=%{_bindir}/quakestat \
	PATH_TO_FPING=%{_sbindir}/fping \
	PATH_TO_NTPQ=%{_sbindir}/ntpq \
	PATH_TO_NTPDC=%{_sbindir}/ntpdc \
	PATH_TO_NTPDATE=%{_sbindir}/ntpdate \
	PATH_TO_RPCINFO=%{_sbindir}/rpcinfo \
	--with-ps-command="`which ps` -eo 's uid pid ppid vsz rss pcpu etime comm args'" \
	--with-ps-format='%s %d %d %d %d %d %f %s %s %n' \
	--with-ps-cols=10 \
	--enable-extra-opts \
	--with-ps-varlist='procstat,&procuid,&procpid,&procppid,&procvsz,&procrss,&procpcpu,procetime,procprog,&pos'

make %{?_smp_mflags}
cd plugins
make check_ide_smart
make check_ldap
make check_radius
make check_pgsql

cd ..

mv plugins-scripts/check_ntp.pl plugins-scripts/check_ntp.pl.in
gawk -f plugins-scripts/subst plugins-scripts/check_ntp.pl.in > plugins-scripts/check_ntp.pl

cp %{SOURCE1} ./README.Fedora

%install
sed -i 's,^MKINSTALLDIRS.*,MKINSTALLDIRS = ../mkinstalldirs,' po/Makefile
make AM_INSTALL_PROGRAM_FLAGS="" DESTDIR=%{buildroot} install
install -m 0755 plugins-root/check_icmp %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins-root/check_dhcp %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins/check_ide_smart %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins/check_ldap %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins-scripts/check_ntp.pl %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins/check_radius %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins/check_pgsql %{buildroot}/%{_libdir}/nagios/plugins

%ifarch ppc ppc64 ppc64p7 sparc sparc64
rm -f %{buildroot}/%{_libdir}/nagios/plugins/check_sensors
%endif

chmod 644 %{buildroot}/%{_libdir}/nagios/plugins/utils.pm

%find_lang %{pname}

%files -f %{name}.lang
%doc ACKNOWLEDGEMENTS AUTHORS ChangeLog CODING COPYING FAQ LEGAL NEWS README REQUIREMENTS SUPPORT THANKS README.Fedora
%{_libdir}/nagios/plugins/negate
%{_libdir}/nagios/plugins/urlize
%{_libdir}/nagios/plugins/utils.sh

%files all

%files apt
%{_libdir}/nagios/plugins/check_apt

%files breeze
%{_libdir}/nagios/plugins/check_breeze

%files by_ssh
%{_libdir}/nagios/plugins/check_by_ssh

%files cluster
%{_libdir}/nagios/plugins/check_cluster

%files dbi
%{_libdir}/nagios/plugins/check_dbi

%files dhcp
%defattr(4750,root,nagios,-)
%{_libdir}/nagios/plugins/check_dhcp

%files dig
%{_libdir}/nagios/plugins/check_dig

%files disk
%{_libdir}/nagios/plugins/check_disk

%files disk_smb
%{_libdir}/nagios/plugins/check_disk_smb

%files dns
%{_libdir}/nagios/plugins/check_dns

%files dummy
%{_libdir}/nagios/plugins/check_dummy

%files file_age
%{_libdir}/nagios/plugins/check_file_age

%files flexlm
%{_libdir}/nagios/plugins/check_flexlm

%files fping
%defattr(4750,root,nagios,-)
%{_libdir}/nagios/plugins/check_fping

%files game
%{_libdir}/nagios/plugins/check_game

%files hpjd
%{_libdir}/nagios/plugins/check_hpjd

%files http
%{_libdir}/nagios/plugins/check_http

%files icmp
%defattr(4750,root,nagios,-)
%{_libdir}/nagios/plugins/check_icmp

%files ifoperstatus
%{_libdir}/nagios/plugins/check_ifoperstatus

%files ifstatus
%{_libdir}/nagios/plugins/check_ifstatus

%files ide_smart
%defattr(4750,root,nagios,-)
%{_libdir}/nagios/plugins/check_ide_smart

%files ircd
%{_libdir}/nagios/plugins/check_ircd

%files ldap
%{_libdir}/nagios/plugins/check_ldap
%{_libdir}/nagios/plugins/check_ldaps

%files load
%{_libdir}/nagios/plugins/check_load

%files log
%{_libdir}/nagios/plugins/check_log

%files mailq
%{_libdir}/nagios/plugins/check_mailq

%files mrtg
%{_libdir}/nagios/plugins/check_mrtg

%files mrtgtraf
%{_libdir}/nagios/plugins/check_mrtgtraf

%files mysql
%{_libdir}/nagios/plugins/check_mysql
%{_libdir}/nagios/plugins/check_mysql_query

%files nagios
%{_libdir}/nagios/plugins/check_nagios

%files nt
%{_libdir}/nagios/plugins/check_nt

%files ntp
%{_libdir}/nagios/plugins/check_ntp
%{_libdir}/nagios/plugins/check_ntp_peer
%{_libdir}/nagios/plugins/check_ntp_time

%files ntp-perl
%{_libdir}/nagios/plugins/check_ntp.pl

%files nwstat
%{_libdir}/nagios/plugins/check_nwstat

%files oracle
%{_libdir}/nagios/plugins/check_oracle

%files overcr
%{_libdir}/nagios/plugins/check_overcr

%files perl
%{_libdir}/nagios/plugins/utils.pm

%files pgsql
%{_libdir}/nagios/plugins/check_pgsql

%files ping
%{_libdir}/nagios/plugins/check_ping

%files procs
%{_libdir}/nagios/plugins/check_procs

%files radius
%{_libdir}/nagios/plugins/check_radius

%files real
%{_libdir}/nagios/plugins/check_real

%files rpc
%{_libdir}/nagios/plugins/check_rpc

%ifnarch ppc ppc64 ppc64p7 sparc sparc64
%files sensors
%{_libdir}/nagios/plugins/check_sensors
%endif

%files smtp
%{_libdir}/nagios/plugins/check_smtp

%files snmp
%{_libdir}/nagios/plugins/check_snmp

%files ssh
%{_libdir}/nagios/plugins/check_ssh

%files swap
%{_libdir}/nagios/plugins/check_swap

%files tcp
%{_libdir}/nagios/plugins/check_clamd
%{_libdir}/nagios/plugins/check_ftp
%{_libdir}/nagios/plugins/check_imap
%{_libdir}/nagios/plugins/check_jabber
%{_libdir}/nagios/plugins/check_nntp
%{_libdir}/nagios/plugins/check_nntps
%{_libdir}/nagios/plugins/check_pop
%{_libdir}/nagios/plugins/check_simap
%{_libdir}/nagios/plugins/check_spop
%{_libdir}/nagios/plugins/check_ssmtp
%{_libdir}/nagios/plugins/check_tcp
%{_libdir}/nagios/plugins/check_udp

%files time
%{_libdir}/nagios/plugins/check_time

%files ups
%{_libdir}/nagios/plugins/check_ups

%files uptime
%{_libdir}/nagios/plugins/check_uptime

%files users
%{_libdir}/nagios/plugins/check_users

%files wave
%{_libdir}/nagios/plugins/check_wave

%changelog
* Thu May 1 2014 Sam Kottler <skottler@fedoraproject.org> - 2.0.1-1
- Update to 2.0.1
- Moved SSD-specific patch which landed upstream
- Update patch to binary paths in plugins-scripts/check_log.sh so it applies
- Add -uptime subpackage

* Thu Oct 24 2013 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.5-2
- New check_dbi plugin (BR: libdbi-devel; subpackage: nagios-plugins-dbi)

* Wed Oct 23 2013 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.5-1
- Update to version 1.5
- New project homepage and source download locations
- Disabled patches 1, 6, 8, and 9.
- No linux_raid subpackage (the contrib directory was removed)

* Wed Oct 16 2013 Peter Lemenkov <lemenkov@gmail.com> - 1.4.16-10
- Remove EL4 and EL5 support
- Backport patches to fix check_linux_raid in case of resyncing (rhbz #504721)
- Fix smart attribute comparison (rhbz #913085)

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.16-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Sun Jul 21 2013 Petr Pisar <ppisar@redhat.com> - 1.4.16-8
- Perl 5.18 rebuild

* Wed May 22 2013 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.4.16-7
- Build package with PIE flags (#965536)

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.16-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Fri Aug 17 2012 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.4.16-5
- Fix the use lib statement and the external ntp commands paths in check-ntp.pl
  (nagios-plugins-0008-ntpdate-and-ntpq-paths.patch).

* Thu Aug 16 2012 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.4.16-4
- Remove the erroneous requirements of nagios-plugins-ntp (#848830)
- Ship check-ntp.pl in the new nagios-plugins-ntp-perl subpackage (#848830)

* Fri Jul 20 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.16-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Mon Jul  9 2012 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.4.16-2
- Provides bundled(gnulib) (#821779)

* Mon Jul  9 2012 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.4.16-1
- Update to version 1.4.16
- Dropped nagios-plugins-0005-Patch-for-check_linux_raid-with-on-linear-raid0-arra.patch
  (upstream).

* Tue Jun 26 2012 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.4.15-7
- glibc 2.16 no longer defines gets for ISO C11, ISO C++11, and _GNU_SOURCE
  (#835621): nagios-plugins-0007-undef-gets-and-glibc-2.16.patch

* Tue Jun 26 2012 Jose Pedro Oliveira <jpo at di.uminho.pt> - 1.4.15-6
- The nagios-plugins RPM no longer needs to own the /usr/lib{,64}/nagios/plugins
  directory; this directory is now owned by nagios-common (#835621)
- Small updates (clarification) to the file nagios-plugins.README.Fedora

* Fri Jan 13 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.15-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Wed Mar 23 2011 Dan Horák <dan@danny.cz> - 1.4.15-4
- rebuilt for mysql 5.5.10 (soname bump in libmysqlclient)

* Tue Feb 08 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.15-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Thu Oct  7 2010 Peter Lemenkov <lemenkov@gmail.com> - 1.4.15-2
- Dropped check_udp sub-package (see rhbz #634067). Anyway it
  provided just a symlink to check_tcp.
- Fixed weird issue with check_swap returning ok in case of
  missing swap (see rhbz #512559).

* Wed Aug 18 2010 Peter Lemenkov <lemenkov@gmail.com> - 1.4.15-1
- Ver. 1.4.15
- Dropped patch for restoration of behaviour in case of ssl checks

* Tue May 18 2010 Peter Lemenkov <lemenkov@gmail.com> - 1.4.14-4
- Restore ssl behaviour for check_http in case of self-signed
  certificates (see rhbz #584227).

* Sat Apr 24 2010 Peter Lemenkov <lemenkov@gmail.com> - 1.4.14-3
- Removed Requires - nagios (see rhbz #469530).
- Added "Requires,Requires(pre): group(nagios)" where necessary
- Sorted %%files sections
- No need to ship INSTALL file
- Added more doc files to main package

* Mon Apr 12 2010 Peter Lemenkov <lemenkov@gmail.com> - 1.4.14-2
- Added missing Requires - nagios (see rhbz #469530).
- Fixed path to qstat -> quakestat (see rhbz #533777)
- Disable radius plugin for EL4 - there is not radiuscleint-ng for EL-4

* Wed Mar 10 2010 Peter Lemenkov <lemenkov@gmail.com> - 1.4.14-1
- Ver. 1.4.14
- Rebased patches.

* Fri Aug 21 2009 Tomas Mraz <tmraz@redhat.com> - 1.4.13-17
- rebuilt with new openssl

* Sat Jul 25 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.13-16
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Mon Jun 22 2009 Mike McGrath <mmcgrath@redhat.com> - 1.4.13-15
- Added patch from upstream to fix ntp faults (bz #479030)

* Wed Feb 25 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.13-14
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Sat Jan 24 2009 Caolán McNamara <caolanm@redhat.com> 1.4.13-13
- rebuild for dependencies

* Sat Jan 17 2009 Tomas Mraz <tmraz@redhat.com> 1.4.13-12
- rebuild with new openssl

* Mon Oct 20 2008 Robert M. Albrecht <romal@gmx.de> 1.4.13-11
- Enabled --with-extra-opts again

* Mon Oct 20 2008 Robert M. Albrecht <romal@gmx.de> 1.4.13-10
- removed provides perl plugins Bugzilla 457404

* Thu Oct 16 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.13-9
- This is a "CVS is horrible" rebuild

* Thu Oct  9 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.13-8
- Rebuilt with a proper patch

* Wed Oct  8 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.13-7
- Added changed recent permission changes to allow nagios group to execute

* Wed Oct  8 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.13-6
- Fixed up some permission issues

* Mon Oct  6 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.13-5
- Fixing patch, missing semicolon

* Sun Sep 28 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.13-4
- Upstream released new version #464419
- Added patch fix for check_linux_raid #253898
- Upstream releases fix for #451015 - check_ntp_peers
- Upstream released fix for #459309 - check_ntp
- Added Provides Nagios::Plugins for #457404
- Fixed configure line for #458985 check_procs

* Thu Jul 10 2008 Robert M. Albrecht <romal@gmx.de> 1.4.12-3
- Removed --with-extra-opts, does not build in Koji

* Mon Jun 30 2008 Robert M. Albrecht <romal@gmx.de> 1.4.12-2
- Enabled --with-extra-opts

* Sun Jun 29 2008 Robert M. Albrecht <romal@gmx.de> 1.4.12-1
- Upstream released version 1.4.12
- Removed patches ping_timeout.patch and pgsql-fix.patch

* Wed Apr 30 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.11-4
- added patch for check_pgsql

* Wed Apr 09 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.11-2
- Fix for 250588

* Thu Feb 28 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.11-1
- Upstream released version 1.4.11
- Added check_ntp peer and time

* Tue Feb 19 2008 Fedora Release Engineering <rel-eng@fedoraproject.org> - 1.4.10-6
- Autorebuild for GCC 4.3

* Tue Feb 12 2008 Mike McGrath <mmcgrath@redhat.com> 1.4-10-5
- Rebuild for gcc43

* Thu Jan 10 2008 Mike McGrath <mmcgrath@redhat.com> 1.4.10-4
- Fixed check_log plugin #395601

* Thu Dec 06 2007 Release Engineering <rel-eng at fedoraproject dot org> - 1.4.10-2
- Rebuild for deps

* Thu Dec 06 2007 Mike McGrath <mmcgrath@redhat.com> 1.4.10-1
- Upstream released new version
- Removed some patches

* Fri Oct 26 2007 Mike McGrath <mmcgrath@redhat.com> 1.4.8-9
- Fix for Bug 348731 and CVE-2007-5623

* Wed Aug 22 2007 Mike McGrath <mmcgrath@redhat.com> 1.4.8-7
- Rebuild for BuildID
- License change

* Fri Aug 10 2007 Mike McGrath <mmcgrath@redhat.com> 1.4.8-6
- Fix for check_linux_raid - #234416
- Fix for check_ide_disk - #251635

* Tue Aug 07 2007 Mike McGrath <mmcgrath@redhat.com> 1.4.8-2
- Fix for check_smtp - #251049

* Fri Apr 13 2007 Mike McGrath <mmcgrath@redhat.com> 1.4.8-1
- Upstream released new version

* Fri Feb 23 2007 Mike McGrath <mmcgrath@redhat.com> 1.4.6-1
- Upstream released new version

* Sun Dec 17 2006 Mike McGrath <imlinux@gmail.com> 1.4.5-1
- Upstream released new version

* Fri Oct 27 2006 Mike McGrath <imlinux@gmail.com> 1.4.4-2
- Enabled check_smart_ide
- Added patch for linux_raid
- Fixed permissions on check_icmp

* Tue Oct 24 2006 Mike McGrath <imlinux@gmail.com> 1.4.4-1
- Upstream new version
- Disabled check_ide_smart (does not compile cleanly/too lazy to fix right now)
- Added check_apt

* Sun Aug 27 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-18
- Removed utils.pm from the base nagios-plugins package into its own package

* Tue Aug 15 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-17
- Added requires qstat for check_game

* Thu Aug 03 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-16
- Providing path to qstat

* Thu Aug 03 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-15
- Fixed permissions on check_dhcp
- Added check_game
- Added check_radius
- Added patch for ntp

* Sun Jul 23 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-14
- Patched upstream issue: 196356

* Sun Jul 23 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-13
- nagios-plugins-all now includes nagios-plugins-mysql

* Thu Jun 22 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-12
- removed sensors support for sparc and sparc64

* Thu Jun 22 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-11
- Created a README.Fedora explaining how to install other plugins

* Sun Jun 11 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-9
- Removed check_sensors in install section

* Sat Jun 10 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-8
- Inserted conditional blocks for ppc exception.

* Wed Jun 07 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-7
- Removed sensors from all plugins and added excludearch: ppc

* Tue Jun 06 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-6
- For ntp plugins requires s/ntpc/ntpdc/

* Sat Jun 03 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-5
- Fixed a few syntax errors and removed an empty export

* Fri May 19 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-4
- Now using configure macro instead of ./configure
- Added BuildRequest: perl(Net::SNMP)
- For reference, this was bugzilla.redhat.com ticket# 176374

* Fri May 19 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-3
- Added check_ide_smart
- Added some dependencies
- Added support for check_if* (perl-Net-SNMP now in extras)
- nagios-plugins now owns dir %%{_libdir}/nagios

* Sat May 13 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-2
- Added a number of requires that don't get auto-detected

* Sun May 07 2006 Mike McGrath <imlinux@gmail.com> 1.4.3-1
- Upstream remeased 1.4.3

* Tue Apr 18 2006 Mike McGrath <imlinux@gmail.com> 1.4.2-9
- Fixed a typo where nagios-plugins-all required nagios-plugins-httpd

* Mon Mar 27 2006 Mike McGrath <imlinux@gmail.com> 1.4.2-8
- Updated to CVS head for better MySQL support

* Sun Mar 5 2006 Mike McGrath <imlinux@gmail.com> 1.4.2-7
- Added a nagios-plugins-all package

* Wed Feb 1 2006 Mike McGrath <imlinux@gmail.com> 1.4.2-6
- Added provides for check_tcp

* Mon Jan 30 2006 Mike McGrath <imlinux@gmail.com> 1.4.2-5
- Created individual packages for all check_* scripts

* Tue Dec 20 2005 Mike McGrath <imlinux@gmail.com> 1.4.2-4
- Fedora friendly spec file

* Mon May 23 2005 Sean Finney <seanius@seanius.net> - cvs head
- just include the nagios plugins directory, which will automatically include
  all generated plugins (which keeps the build from failing on systems that
  don't have all build-dependencies for every plugin)

* Thu Mar 04 2004 Karl DeBisschop <karl[AT]debisschop.net> - 1.4.0alpha1
- extensive rewrite to facilitate processing into various distro-compatible specs

* Thu Mar 04 2004 Karl DeBisschop <karl[AT]debisschop.net> - 1.4.0alpha1
- extensive rewrite to facilitate processing into various distro-compatible specs

