#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Turn off strip'ng of binaries
# %global __strip /bin/true

%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname nagios-plugins

%global _hardened_build 1

Name: %{pname}%{PROJ_DELIM}
Version: 2.2.1
Release: 1%{?dist}
Summary: Host/service/network monitoring program plugins for Nagios
Group: %{PROJ_NAME}/admin

License: GPLv3
URL: https://www.nagios-plugins.org/
Source0: http://www.nagios-plugins.org/download/nagios-plugins-%{version}.tar.gz
Source1: nagios-plugins.README.Fedora
Patch2: nagios-plugins-0002-Remove-assignment-of-not-parsed-to-jitter.patch
Patch7: nagios-plugins-0007-Fix-the-use-lib-statement-and-the-external-ntp-comma.patch
Patch8: nagios-plugins-0008-Fix-new-mariadb-support.patch

%if 0%{?fedora} || 0%{?rhel}
BuildRequires:  qstat
%endif

%if 0%{?suse_version}
BuildRequires: openldap2-devel
BuildRequires: net-snmp
%else
BuildRequires: openldap-devel
BuildRequires: net-snmp-utils
%endif
BuildRequires: mysql-devel
BuildRequires: net-snmp-devel
BuildRequires: samba-client
BuildRequires: postgresql-devel
BuildRequires: gettext
#BuildRequires: %{_bindir}/ssh
BuildRequires: bind-utils
BuildRequires: ntp
#BuildRequires: %{_bindir}/mailq
#%if 0%{?suse_version}
#BuildRequires: ssmtp
#%else
BuildRequires: postfix
#%endif
#BuildRequires: %{_sbindir}/fping
BuildRequires: fping
BuildRequires: iputils
#BuildRequires: perl(Net::SNMP)
BuildRequires: perl-Net-SNMP
%if 0%{?fedora} || 0%{?rhel}
BuildRequires: radiusclient-ng-devel
%endif

BuildRequires: libdbi-devel

%if 0%{?sle_version} || 0%{?suse_version}
#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks
%endif

Requires: nagios-common%{PROJ_DELIM} >= 3.3.1-1
Provides: %{name}
Provides: %{pname}

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

%package -n %{pname}-all%{PROJ_DELIM}
Summary: Nagios Plugins - All plugins
Group: Applications/System
Requires:  nagios-plugins-by_ssh%{PROJ_DELIM}
Requires:  nagios-plugins-cluster%{PROJ_DELIM}
Requires:  nagios-plugins-dhcp%{PROJ_DELIM}
Requires:  nagios-plugins-dig%{PROJ_DELIM}
Requires:  nagios-plugins-disk%{PROJ_DELIM}
Requires:  nagios-plugins-disk_smb%{PROJ_DELIM}
Requires:  nagios-plugins-dns%{PROJ_DELIM}
Requires:  nagios-plugins-dummy%{PROJ_DELIM}
Requires:  nagios-plugins-file_age%{PROJ_DELIM}
Requires:  nagios-plugins-flexlm%{PROJ_DELIM}
Requires:  nagios-plugins-fping%{PROJ_DELIM}
Requires:  nagios-plugins-hpjd%{PROJ_DELIM}
Requires:  nagios-plugins-http%{PROJ_DELIM}
Requires:  nagios-plugins-icmp%{PROJ_DELIM}
Requires:  nagios-plugins-ide_smart%{PROJ_DELIM}
Requires:  nagios-plugins-ircd%{PROJ_DELIM}
Requires:  nagios-plugins-ldap%{PROJ_DELIM}
Requires:  nagios-plugins-load%{PROJ_DELIM}
Requires:  nagios-plugins-log%{PROJ_DELIM}
Requires:  nagios-plugins-mailq%{PROJ_DELIM}
Requires:  nagios-plugins-mrtg%{PROJ_DELIM}
Requires:  nagios-plugins-mrtgtraf%{PROJ_DELIM}
Requires:  nagios-plugins-mysql%{PROJ_DELIM}
Requires:  nagios-plugins-nagios%{PROJ_DELIM}
# NRPE plugin comes from the nrpe build, but it is a plugin so including it here
Requires:  nagios-plugins-nrpe%{PROJ_DELIM}
# The perl version of these plugins are iffy -- we're not build this one, so don't include
#Requires:  nagios-plugins-ntp-perl%{PROJ_DELIM}
Requires:  nagios-plugins-ntp%{PROJ_DELIM}
Requires:  nagios-plugins-nt%{PROJ_DELIM}
Requires:  nagios-plugins-nwstat%{PROJ_DELIM}
Requires:  nagios-plugins-oracle%{PROJ_DELIM}
Requires:  nagios-plugins-overcr%{PROJ_DELIM}
Requires:  nagios-plugins-pgsql%{PROJ_DELIM}
Requires:  nagios-plugins-ping%{PROJ_DELIM}
Requires:  nagios-plugins-procs%{PROJ_DELIM}
Requires:  nagios-plugins-real%{PROJ_DELIM}
Requires:  nagios-plugins-rpc%{PROJ_DELIM}
Requires:  nagios-plugins-smtp%{PROJ_DELIM}
Requires:  nagios-plugins-snmp%{PROJ_DELIM}
Requires:  nagios-plugins-ssh%{PROJ_DELIM}
Requires:  nagios-plugins-swap%{PROJ_DELIM}
Requires:  nagios-plugins-tcp%{PROJ_DELIM}
Requires:  nagios-plugins-time%{PROJ_DELIM}
Requires:  nagios-plugins-ups%{PROJ_DELIM}
Requires:  nagios-plugins-users%{PROJ_DELIM}
Requires:  nagios-plugins-wave%{PROJ_DELIM}
%if 0%{?fedora} > 14 || 0%{?rhel} > 6 || 0%{?sle_version} >= 150000
%global include_game 1
Requires: nagios-plugins-game
%else
%global include_game 0
%endif

%ifnarch ppc ppc64 ppc64p7 sparc sparc64
Requires: nagios-plugins-sensors
%endif

%description -n %{pname}-all%{PROJ_DELIM}
This package provides all Nagios plugins.

%package -n %{pname}-apt%{PROJ_DELIM}
Summary: Nagios Plugin - check_apt
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-apt

%description -n %{pname}-apt%{PROJ_DELIM}
Provides check_apt support for Nagios.

%package -n %{pname}-breeze%{PROJ_DELIM}
Summary: Nagios Plugin - check_breeze
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-breeze

%description -n %{pname}-breeze%{PROJ_DELIM}
Provides check_breeze support for Nagios.

%package -n %{pname}-by_ssh%{PROJ_DELIM}
Summary: Nagios Plugin - check_by_ssh
Group: Applications/System
Requires: %{name} = %{version}-%{release}
#Requires: %{_bindir}/ssh
%if 0%{?suse_version}
BuildRequires: openssh
%else
BuildRequires: openssh-clients
%endif
Provides: %{pname}-by_ssh

%description -n %{pname}-by_ssh%{PROJ_DELIM}
Provides check_by_ssh support for Nagios.

%package -n %{pname}-cluster%{PROJ_DELIM}
Summary: Nagios Plugin - check_cluster
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-cluster

%description -n %{pname}-cluster%{PROJ_DELIM}
Provides check_cluster support for Nagios.

%package -n %{pname}-dbi%{PROJ_DELIM}
Summary: Nagios Plugin - check_dbi
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-dbi

%description -n %{pname}-dbi%{PROJ_DELIM}
Provides check_dbi support for Nagios.

%package -n %{pname}-dhcp%{PROJ_DELIM}
Summary: Nagios Plugin - check_dhcp
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: nagios-common%{PROJ_DELIM}
Requires(pre): nagios-common%{PROJ_DELIM}
Provides: %{pname}-dhcp

%description -n %{pname}-dhcp%{PROJ_DELIM}
Provides check_dhcp support for Nagios.

%package -n %{pname}-dig%{PROJ_DELIM}
Summary: Nagios Plugin - check_dig
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: %{_bindir}/dig
Provides: %{pname}-dig

%description -n %{pname}-dig%{PROJ_DELIM}
Provides check_dig support for Nagios.

%package -n %{pname}-disk%{PROJ_DELIM}
Summary: Nagios Plugin - check_disk
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-disk

%description -n %{pname}-disk%{PROJ_DELIM}
Provides check_disk support for Nagios.

%package -n %{pname}-disk_smb%{PROJ_DELIM}
Summary: Nagios Plugin - check_disk_smb
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: %{_bindir}/smbclient
Provides: %{pname}-disk_smb

%description -n %{pname}-disk_smb%{PROJ_DELIM}
Provides check_disk_smb support for Nagios.

%package -n %{pname}-dns%{PROJ_DELIM}
Summary: Nagios Plugin - check_dns
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: %{_bindir}/nslookup
Provides: %{pname}-dns

%description -n %{pname}-dns%{PROJ_DELIM}
Provides check_dns support for Nagios.

%package -n %{pname}-dummy%{PROJ_DELIM}
Summary: Nagios Plugin - check_dummy
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-dummy

%description -n %{pname}-dummy%{PROJ_DELIM}
Provides check_dummy support for Nagios.
This plugin does not actually check anything, simply provide it with a flag
0-4 and it will return the corresponding status code to Nagios.

%package -n %{pname}-file_age%{PROJ_DELIM}
Summary: Nagios Plugin - check_file_age
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-file_age

%description -n %{pname}-file_age%{PROJ_DELIM}
Provides check_file_age support for Nagios.

%package -n %{pname}-flexlm%{PROJ_DELIM}
Summary: Nagios Plugin - check_flexlm
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-flexlm
AutoReq: no

%description -n %{pname}-flexlm%{PROJ_DELIM}
Provides check_flexlm support for Nagios.

%package -n %{pname}-fping%{PROJ_DELIM}
Summary: Nagios Plugin - check_fping
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: %{_sbindir}/fping
Requires: nagios-common%{PROJ_DELIM}
Requires(pre): nagios-common%{PROJ_DELIM}
Provides: %{pname}-fping

%description -n %{pname}-fping%{PROJ_DELIM}
Provides check_fping support for Nagios.

%if %{include_game}
%package -n %{pname}-game%{PROJ_DELIM}
Summary: Nagios Plugin - check_game
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: qstat
Provides: %{pname}-qstat
Provides: %{pname}-game

%description -n %{pname}-game%{PROJ_DELIM}
Provides check_game support for Nagios.
%endif

%package -n %{pname}-hpjd%{PROJ_DELIM}
Summary: Nagios Plugin - check_hpjd
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-hpjd

%description -n %{pname}-hpjd%{PROJ_DELIM}
Provides check_hpjd support for Nagios.

%package -n %{pname}-http%{PROJ_DELIM}
Summary: Nagios Plugin - check_http
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-http

%description -n %{pname}-http%{PROJ_DELIM}
Provides check_http support for Nagios.

%package -n %{pname}-icmp%{PROJ_DELIM}
Summary: Nagios Plugin - check_icmp
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: nagios-common%{PROJ_DELIM}
Requires(pre): nagios-common%{PROJ_DELIM} 
Provides: %{pname}-icmp

%description -n %{pname}-icmp%{PROJ_DELIM}
Provides check_icmp support for Nagios.

%package -n %{pname}-ide_smart%{PROJ_DELIM}
Summary: Nagios Plugin - check_ide_smart
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: nagios-common%{PROJ_DELIM}
Requires(pre): nagios-common%{PROJ_DELIM}
Provides: %{pname}-ide_smart

%description -n %{pname}-ide_smart%{PROJ_DELIM}
Provides check_ide_smart support for Nagios.

%package -n %{pname}-ifoperstatus%{PROJ_DELIM}
Summary: Nagios Plugin - check_ifoperstatus
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-ifoperstatus
AutoReq: no

%description -n %{pname}-ifoperstatus%{PROJ_DELIM}
Provides check_ifoperstatus support for Nagios to monitor network interfaces.

%package -n %{pname}-ifstatus%{PROJ_DELIM}
Summary: Nagios Plugin - check_ifstatus
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-ifstatus
AutoReq: no

%description -n %{pname}-ifstatus%{PROJ_DELIM}
Provides check_ifstatus support for Nagios to monitor network interfaces.

%package -n %{pname}-ircd%{PROJ_DELIM}
Summary: Nagios Plugin - check_ircd
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-ircd

%description -n %{pname}-ircd%{PROJ_DELIM}
Provides check_ircd support for Nagios.

%package -n %{pname}-ldap%{PROJ_DELIM}
Summary: Nagios Plugin - check_ldap
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-ldap

%description -n %{pname}-ldap%{PROJ_DELIM}
Provides check_ldap support for Nagios.

%package -n %{pname}-load%{PROJ_DELIM}
Summary: Nagios Plugin - check_load
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-load

%description -n %{pname}-load%{PROJ_DELIM}
Provides check_load support for Nagios.

%package -n %{pname}-log%{PROJ_DELIM}
Summary: Nagios Plugin - check_log
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: /bin/egrep
Requires: /bin/mktemp
Provides: %{pname}-log

%description -n %{pname}-log%{PROJ_DELIM}
Provides check_log support for Nagios.

%package -n %{pname}-mailq%{PROJ_DELIM}
Summary: Nagios Plugin - check_mailq
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: %{_bindir}/mailq
Provides: %{pname}-mailq

%description -n %{pname}-mailq%{PROJ_DELIM}
Provides check_mailq support for Nagios.

%package -n %{pname}-mrtg%{PROJ_DELIM}
Summary: Nagios Plugin - check_mrtg
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-mrtg

%description -n %{pname}-mrtg%{PROJ_DELIM}
Provides check_mrtg support for Nagios.

%package -n %{pname}-mrtgtraf%{PROJ_DELIM}
Summary: Nagios Plugin - check_mrtgtraf
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-mrtgtraf

%description -n %{pname}-mrtgtraf%{PROJ_DELIM}
Provides check_mrtgtraf support for Nagios.

%package -n %{pname}-mysql%{PROJ_DELIM}
Summary: Nagios Plugin - check_mysql
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-mysql

%description -n %{pname}-mysql%{PROJ_DELIM}
Provides check_mysql and check_mysql_query support for Nagios.

%package -n %{pname}-nagios%{PROJ_DELIM}
Summary: Nagios Plugin - check_nagios
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-nagios

%description -n %{pname}-nagios%{PROJ_DELIM}
Provides check_nagios support for Nagios.

%package -n %{pname}-nt%{PROJ_DELIM}
Summary: Nagios Plugin - check_nt
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-nt

%description -n %{pname}-nt%{PROJ_DELIM}
Provides check_nt support for Nagios.

%package -n %{pname}-ntp%{PROJ_DELIM}
Summary: Nagios Plugin - check_ntp
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-ntp

%description -n %{pname}-ntp%{PROJ_DELIM}
Provides check_ntp support for Nagios.

# perl scripts aren't getting substitutions done in 2.1.1, Makefile issue?
#%package -n %{pname}-ntp-perl%{PROJ_DELIM}
#Summary: Nagios Plugin - check_ntp.pl
#Group: Applications/System
#Requires: %{name} = %{version}-%{release}
#Requires: %{_sbindir}/ntpdate
#Requires: %{_sbindir}/ntpq
#Provides: %{pname}-ntp-perl
#
#%description -n %{pname}-ntp-perl%{PROJ_DELIM}
#Provides check_ntp.pl support for Nagios.

%package -n %{pname}-nwstat%{PROJ_DELIM}
Summary: Nagios Plugin - check_nwstat
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-nwstat

%description -n %{pname}-nwstat%{PROJ_DELIM}
Provides check_nwstat support for Nagios.

%package -n %{pname}-oracle%{PROJ_DELIM}
Summary: Nagios Plugin - check_oracle
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-oracle

%description -n %{pname}-oracle%{PROJ_DELIM}
Provides check_oracle support for Nagios.

%package -n %{pname}-overcr%{PROJ_DELIM}
Summary: Nagios Plugin - check_overcr
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-overcr

%description -n %{pname}-overcr%{PROJ_DELIM}
Provides check_overcr support for Nagios.

%package -n %{pname}-perl%{PROJ_DELIM}
Summary: Nagios plugins perl dep.
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-perl

%description -n %{pname}-perl%{PROJ_DELIM}
Perl dep for nagios plugins.  This is *NOT* an actual plugin it simply provides
utils.pm

%package -n %{pname}-pgsql%{PROJ_DELIM}
Summary: Nagios Plugin - check_pgsql
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-pgsql

%description -n %{pname}-pgsql%{PROJ_DELIM}
Provides check_pgsql (PostgreSQL)  support for Nagios.

%package -n %{pname}-ping%{PROJ_DELIM}
Summary: Nagios Plugin - check_ping
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: iputils
Provides: %{pname}-ping

%description -n %{pname}-ping%{PROJ_DELIM}
Provides check_ping support for Nagios.

%package -n %{pname}-procs%{PROJ_DELIM}
Summary: Nagios Plugin - check_procs
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-procs

%description -n %{pname}-procs%{PROJ_DELIM}
Provides check_procs support for Nagios.

%if 0%{?fedora} || 0%{?rhel}
%package -n %{pname}-radius%{PROJ_DELIM}
Summary: Nagios Plugin - check_radius
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-radius

%description -n %{pname}-radius%{PROJ_DELIM}
Provides check_radius support for Nagios.
%endif

%package -n %{pname}-real%{PROJ_DELIM}
Summary: Nagios Plugin - check_real
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-real

%description -n %{pname}-real%{PROJ_DELIM}
Provides check_real (rtsp) support for Nagios.

%package -n %{pname}-rpc%{PROJ_DELIM}
Summary: Nagios Plugin - check_rpc
Group: Applications/System
Requires: %{name} = %{version}-%{release}
#%if 0%{?fedora} || 0%{?rhel}
#Requires: %{_sbindir}/rpcinfo
#%else
#Requires: /sbin/rpcinfo
#%endif
Requires: perl
Requires: rpcbind
Provides: %{pname}-rpc
AutoReq: no

%description -n %{pname}-rpc%{PROJ_DELIM}
Provides check_rpc support for Nagios.

%ifnarch ppc ppc64 sparc sparc64
%package -n %{pname}-sensors%{PROJ_DELIM}
Summary: Nagios Plugin - check_sensors
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: /bin/egrep
Requires: %{_bindir}/sensors
Provides: %{pname}-sensors

%description -n %{pname}-sensors%{PROJ_DELIM}
Provides check_sensors support for Nagios.
%endif

%package -n %{pname}-smtp%{PROJ_DELIM}
Summary: Nagios Plugin - check_smtp
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-smtp

%description -n %{pname}-smtp%{PROJ_DELIM}
Provides check_smtp support for Nagios.

%package -n %{pname}-snmp%{PROJ_DELIM}
Summary: Nagios Plugin - check_snmp
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Requires: %{_bindir}/snmpgetnext
Requires: %{_bindir}/snmpget
Provides: %{pname}-snmp

%description -n %{pname}-snmp%{PROJ_DELIM}
Provides check_snmp support for Nagios.

%package -n %{pname}-ssh%{PROJ_DELIM}
Summary: Nagios Plugin - check_ssh
Group: Applications/System
Requires: %{name} = %{version}-%{release}
%if 0%{?suse_version}
BuildRequires: openssh
%else
BuildRequires: openssh-clients
%endif
Provides: %{pname}-ssh

%description -n %{pname}-ssh%{PROJ_DELIM}
Provides check_ssh support for Nagios.

%package -n %{pname}-swap%{PROJ_DELIM}
Summary: Nagios Plugin - check_swap
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-swap

%description -n %{pname}-swap%{PROJ_DELIM}
Provides check_swap support for Nagios.

%package -n %{pname}-tcp%{PROJ_DELIM}
Summary: Nagios Plugin - check_tcp
Group: Applications/System
Requires: %{name} = %{version}-%{release}
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
Provides: %{pname}-tcp
Obsoletes: nagios-plugins-udp < 1.4.15-2

%description -n %{pname}-tcp%{PROJ_DELIM}
Provides check_tcp, check_ftp, check_imap, check_jabber, check_nntp,
check_nntps, check_pop, check_simap, check_spop, check_ssmtp, check_udp
and check_clamd support for Nagios.

%package -n %{pname}-time%{PROJ_DELIM}
Summary: Nagios Plugin - check_time
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-time

%description -n %{pname}-time%{PROJ_DELIM}
Provides check_time support for Nagios.

%package -n %{pname}-ups%{PROJ_DELIM}
Summary: Nagios Plugin - check_ups
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-ups

%description -n %{pname}-ups%{PROJ_DELIM}
Provides check_ups support for Nagios.

%package -n %{pname}-uptime%{PROJ_DELIM}
Summary: Nagios Plugin - check_uptime
Group: Applications/Systems
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-uptime

%description -n %{pname}-uptime%{PROJ_DELIM}
Provides check_uptime support for Nagios.

%package -n %{pname}-users%{PROJ_DELIM}
Summary: Nagios Plugin - check_users
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-users

%description -n %{pname}-users%{PROJ_DELIM}
Provides check_users support for Nagios.

%package -n %{pname}-wave%{PROJ_DELIM}
Summary: Nagios Plugin - check_wave
Group: Applications/System
Requires: %{name} = %{version}-%{release}
Provides: %{pname}-wave

%description -n %{pname}-wave%{PROJ_DELIM}
Provides check_wave support for Nagios.

%prep
%setup -q -n %{pname}-%{version}

%patch2 -p1 -b .not_parsed
%patch7 -p1 -b .ext_ntp_cmds
%patch8 -p1 


%build
export SUID_CFLAGS=-fPIE
export SUID_LDFLAGS=-pie
%configure \
	--libexecdir=%{_libdir}/nagios/plugins \
	--with-dbi \
	--with-mysql \
%if %{include_game}
	PATH_TO_QSTAT=%{_bindir}/quakestat \
%endif
	PATH_TO_FPING=%{_sbindir}/fping \
	PATH_TO_NTPQ=%{_sbindir}/ntpq \
	PATH_TO_NTPDC=%{_sbindir}/ntpdc \
	PATH_TO_NTPDATE=%{_sbindir}/ntpdate \
	PATH_TO_RPCINFO=%{_sbindir}/rpcinfo \
	--with-ps-command="`which ps` -eo 's uid pid ppid vsz rss pcpu etime comm args'" \
	--with-ps-format='%s %d %d %d %d %d %f %s %s %n' \
	--with-ps-cols=10 \
	--enable-extra-opts \
%if 0%{?suse_version}
        --without-game \
%endif
	--with-ps-varlist='procstat,&procuid,&procpid,&procppid,&procvsz,&procrss,&procpcpu,procetime,procprog,&pos'

make %{?_smp_mflags}
cd plugins
make check_ide_smart
make check_ldap
%if 0%{?fedora} || 0%{?rhel}
make check_radius
%endif
make check_pgsql

cd ..

# perl scripts aren't getting substitutions done in 2.1.1, Makefile issue?
#mv plugins-scripts/check_ntp.pl plugins-scripts/check_ntp.pl.in
#gawk -f plugins-scripts/subst plugins-scripts/check_ntp.pl.in > plugins-scripts/check_ntp.pl

cp %{SOURCE1} ./README.Fedora

%install
sed -i 's,^MKINSTALLDIRS.*,MKINSTALLDIRS = ../mkinstalldirs,' po/Makefile
make AM_INSTALL_PROGRAM_FLAGS="" DESTDIR=%{buildroot} install
install -m 0755 plugins-root/check_icmp %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins-root/check_dhcp %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins/check_ide_smart %{buildroot}/%{_libdir}/nagios/plugins
install -m 0755 plugins/check_ldap %{buildroot}/%{_libdir}/nagios/plugins
# perl scripts aren't getting substitutions done in 2.1.1, Makefile issue?
#install -m 0755 plugins-scripts/check_ntp.pl %{buildroot}/%{_libdir}/nagios/plugins
%if 0%{?fedora} || 0%{?rhel}
install -m 0755 plugins/check_radius %{buildroot}/%{_libdir}/nagios/plugins
%endif
install -m 0755 plugins/check_pgsql %{buildroot}/%{_libdir}/nagios/plugins

%ifarch ppc ppc64 ppc64p7 sparc sparc64
rm -f %{buildroot}/%{_libdir}/nagios/plugins/check_sensors
%endif

chmod 644 %{buildroot}/%{_libdir}/nagios/plugins/utils.pm

%find_lang %{pname}

%files -f %{pname}.lang
%doc ACKNOWLEDGEMENTS AUTHORS ChangeLog CODING COPYING FAQ LEGAL NEWS README REQUIREMENTS SUPPORT THANKS README.Fedora
%{_libdir}/nagios/plugins/negate
%{_libdir}/nagios/plugins/urlize
%{_libdir}/nagios/plugins/utils.sh

%files -n %{pname}-all%{PROJ_DELIM}

%files -n %{pname}-apt%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_apt

%files -n %{pname}-breeze%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_breeze

%files -n %{pname}-by_ssh%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_by_ssh

%files -n %{pname}-cluster%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_cluster

%files -n %{pname}-dbi%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_dbi

%files -n %{pname}-dhcp%{PROJ_DELIM}
%if 0%{?suse_version}
%defattr(0750,root,nagios,-)
%else
%defattr(4750,root,nagios,-)
%endif
%{_libdir}/nagios/plugins/check_dhcp

%files -n %{pname}-dig%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_dig

%files -n %{pname}-disk%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_disk

%files -n %{pname}-disk_smb%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_disk_smb

%files -n %{pname}-dns%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_dns

%files -n %{pname}-dummy%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_dummy

%files -n %{pname}-file_age%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_file_age

%files -n %{pname}-flexlm%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_flexlm

%files -n %{pname}-fping%{PROJ_DELIM}
%if 0%{?suse_version}
%defattr(0750,root,nagios,-)
%else
%defattr(4750,root,nagios,-)
%endif
%{_libdir}/nagios/plugins/check_fping

%if %{include_game}
%files -n %{pname}-game%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_game
%endif

%files -n %{pname}-hpjd%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_hpjd

%files -n %{pname}-http%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_http

%files -n %{pname}-icmp%{PROJ_DELIM}
%if 0%{?suse_version}
%defattr(0750,root,nagios,-)
%else
%defattr(4750,root,nagios,-)
%endif
%{_libdir}/nagios/plugins/check_icmp

%files -n %{pname}-ifoperstatus%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ifoperstatus

%files -n %{pname}-ifstatus%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ifstatus

%files -n %{pname}-ide_smart%{PROJ_DELIM}
%if 0%{?suse_version}
%defattr(0750,root,nagios,-)
%else
%defattr(4750,root,nagios,-)
%endif
%{_libdir}/nagios/plugins/check_ide_smart

%files -n %{pname}-ircd%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ircd

%files -n %{pname}-ldap%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ldap
%{_libdir}/nagios/plugins/check_ldaps

%files -n %{pname}-load%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_load

%files -n %{pname}-log%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_log

%files -n %{pname}-mailq%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_mailq

%files -n %{pname}-mrtg%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_mrtg

%files -n %{pname}-mrtgtraf%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_mrtgtraf

%files -n %{pname}-mysql%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_mysql
%{_libdir}/nagios/plugins/check_mysql_query

%files -n %{pname}-nagios%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_nagios

%files -n %{pname}-nt%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_nt

%files -n %{pname}-ntp%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ntp
%{_libdir}/nagios/plugins/check_ntp_peer
%{_libdir}/nagios/plugins/check_ntp_time

# perl scripts aren't getting substitutions done in 2.1.1, Makefile issue?
#%files -n %{pname}-ntp-perl%{PROJ_DELIM}
#%{_libdir}/nagios/plugins/check_ntp.pl

%files -n %{pname}-nwstat%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_nwstat

%files -n %{pname}-oracle%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_oracle

%files -n %{pname}-overcr%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_overcr

%files -n %{pname}-perl%{PROJ_DELIM}
%{_libdir}/nagios/plugins/utils.pm

%files -n %{pname}-pgsql%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_pgsql

%files -n %{pname}-ping%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ping

%files -n %{pname}-procs%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_procs

%if 0%{?fedora} || 0%{?rhel}
%files -n %{pname}-radius%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_radius
%endif

%files -n %{pname}-real%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_real

%files -n %{pname}-rpc%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_rpc

%ifnarch ppc ppc64 ppc64p7 sparc sparc64
%files -n %{pname}-sensors%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_sensors
%endif

%files -n %{pname}-smtp%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_smtp

%files -n %{pname}-snmp%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_snmp

%files -n %{pname}-ssh%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ssh

%files -n %{pname}-swap%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_swap

%files -n %{pname}-tcp%{PROJ_DELIM}
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

%files -n %{pname}-time%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_time

%files -n %{pname}-ups%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_ups

%files -n %{pname}-uptime%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_uptime

%files -n %{pname}-users%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_users

%files -n %{pname}-wave%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_wave
