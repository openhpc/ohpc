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

# Base package name
%define pname conman


Name:		%{pname}%{PROJ_DELIM}
Version:	0.3.0
Release:	1%{?dist}

Summary:	ConMan: The Console Manager
Group:		%{PROJ_NAME}/admin
License:	GPLv3+
URL:		http://dun.github.io/conman/

Requires:	expect
Requires(post): systemd
Requires(preun): systemd
Requires(postun): systemd


%if 0%{?suse_version}
BuildRequires:	tcpd-devel
%else
%if 0%{?rhel} <= 7
BuildRequires:	tcp_wrappers-devel
%endif
%endif
BuildRequires:	freeipmi-devel
#!BuildIgnore: post-build-checks

Source0:	https://github.com/dun/conman/archive/%pname-%{version}.tar.gz

# 8/15/14 karl.w.schulz@intel.com - include prereq
%if 0%{?sles_version} || 0%{?suse_version}
PreReq: %{insserv_prereq} %{fillup_prereq}
%endif

%description
ConMan is a serial console management program designed to support a large
number of console devices and simultaneous users.  It supports:
  - local serial devices
  - remote terminal servers (via the telnet protocol)
  - IPMI Serial-Over-LAN (via FreeIPMI)
  - Unix domain sockets
  - external processes (eg, using Expect for telnet/ssh/ipmi-sol connections)

Its features include:
  - logging (and optionally timestamping) console device output to file
  - connecting to consoles in monitor (R/O) or interactive (R/W) mode
  - allowing clients to share or steal console write privileges
  - broadcasting client output to multiple consoles

%prep
%setup -q -n %{pname}-%{pname}-%{version}

%build
%configure --with-tcp-wrappers \
%if 0%{?rhel} > 7
	%{nil}
%else
	--with-tcp-wrappers
%endif
make %{?_smp_mflags}

%install
%{__mkdir_p} "%{buildroot}"
make install DESTDIR="%{buildroot}"

rm -rf $RPM_BUILD_ROOT%{_sysconfdir}/init.d

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}


%post
%systemd_post conman.service

if ! grep "^SERVER" /etc/conman.conf > /dev/null; then
    cat <<-HERE >> /etc/conman.conf
SERVER keepalive=ON
SERVER logdir="/var/log/conman"
SERVER logfile="/var/log/conman.log"
SERVER loopback=ON
SERVER pidfile="/var/run/conman.pid"
SERVER resetcmd="powerman -0 %N; sleep 3; powerman -1 %N"
SERVER tcpwrappers=ON
SERVER timestamp=1h
GLOBAL seropts="115200,8n1"
GLOBAL log="console.%N"
GLOBAL logopts="sanitize,timestamp"

HERE
fi

%preun
%systemd_preun conman.service

%postun
%systemd_postun_with_restart conman.service

%if %{?insserv_cleanup:1}0
%insserv_cleanup
%endif

%files
%doc AUTHORS
%doc COPYING
%doc DISCLAIMER*
%doc FAQ
%doc NEWS
%doc README
%doc THANKS
%{OHPC_PUB}
%config(noreplace) %{_sysconfdir}/conman.conf
%config(noreplace) %{_sysconfdir}/[dls]*/conman
%{_unitdir}/%{pname}.service
%{_bindir}/*
%{_sbindir}/*
%{_prefix}/lib/*
%{_mandir}/*/*
