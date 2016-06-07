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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

# Base package name
%define pname conman
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name:		%{pname}%{PROJ_DELIM}
Version:	0.2.7
Release:	1%{?dist}

Summary:	ConMan: The Console Manager
Group:		%{PROJ_NAME}/admin
License:	GPLv3+
URL:		http://dun.github.io/conman/
DocDir:         %{OHPC_PUB}/doc/contrib

Requires:	expect
BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)


%if 0%{?suse_version}
BuildRequires:	tcpd-devel
%else
BuildRequires:	tcp_wrappers-devel
%endif
BuildRequires:	freeipmi-devel
#!BuildIgnore: post-build-checks

Source0:	https://github.com/dun/conman/releases/download/%{pname}-%{version}/%{pname}-%{version}.tar.bz2
Source1:    %{pname}.service
Patch1:         conman.init.patch

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
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build
%configure --with-tcp-wrappers --with-freeipmi
make %{?_smp_mflags}

%install
rm -rf "%{buildroot}"
%{__mkdir_p} "%{buildroot}"
make install DESTDIR="%{buildroot}"

install -D -m 0644 %{SOURCE1} $RPM_BUILD_ROOT%{_unitdir}/%{pname}.service
rm -rf $RPM_BUILD_ROOT%{_sysconfdir}/init.d

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf "%{buildroot}"

%post
if [ $1 -eq 1 ] ; then
    /bin/systemctl daemon-reload >/dev/null 2>&1 || :
fi

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
if [ $1 -eq 0 ] ; then
    /bin/systemctl --no-reload disable conman.service > /dev/null 2>&1 || :
    /bin/systemctl stop conman.service > /dev/null 2>&1 || :
fi

%postun
f [ $1 -ge 1 ] ; then
    /bin/systemctl try-restart conman.service >/dev/null 2>&1 || :
fi

%if %{?insserv_cleanup:1}0
%insserv_cleanup
%endif

%files
%defattr(-,root,root,-)
%doc AUTHORS
%doc ChangeLog
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
