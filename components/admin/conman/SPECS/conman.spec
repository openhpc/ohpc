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
%define pname conman
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name:		%{pname}%{PROJ_DELIM}
Version:	0.2.7
Release:	1%{?dist}

Summary:	ConMan: The Console Manager
Group:		fsp/admin
License:	GPLv3+
URL:		http://conman.googlecode.com/
DocDir:         %{FSP_PUB}/doc/contrib

Requires:	expect
BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)


%if 0%{?suse_version}
BuildRequires:	tcpd
BuildRequires:	OpenIPMI
%else
BuildRequires:	tcp_wrappers-devel
BuildRequires:	freeipmi-devel
%endif
#!BuildIgnore: post-build-checks

Source0:	%{pname}-%{version}.tar.bz2
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
%configure
make %{?_smp_mflags}

%install
rm -rf "%{buildroot}"
mkdir -p "%{buildroot}"
make install DESTDIR="%{buildroot}"
#
%if 0%{?_initrddir:1}
if [ "%{_sysconfdir}/init.d" != "%{_initrddir}" ]; then
  mkdir -p "%{buildroot}%{_initrddir}"
  mv "%{buildroot}%{_sysconfdir}/init.d"/* "%{buildroot}%{_initrddir}/"
fi
%endif

%clean
rm -rf "%{buildroot}"

%post

%if 0%{?suse_version}
%{fillup_and_insserv -f}
%else
if [ -x /sbin/chkconfig ]; then
  /sbin/chkconfig --add conman
elif [ -x /usr/lib/lsb/install initd ]; then
  /usr/lib/lsb/install initd %{_initdir}/conman
fi
%endif


%preun
if [ "$1" = 0 ]; then
  INITRDDIR=%{?_initrddir:%{_initrddir}}%{!?_initrddir:%{_sysconfdir}/init.d}
  $INITRDDIR/conman stop >/dev/null 2>&1 || :
  if [ -x /sbin/chkconfig ]; then
     /sbin/chkconfig --del conman
  elif [ -x /usr/lib/lsb/remove initd ]; then
    /usr/lib/lsb/remove initd %{_initdir}/conman
  fi
fi

%postun
if [ "$1" -ge 1 ]; then
  INITRDDIR=%{?_initrddir:%{_initrddir}}%{!?_initrddir:%{_sysconfdir}/init.d}
  $INITRDDIR/conman condrestart >/dev/null 2>&1 || :
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
%{FSP_PUB}
%config(noreplace) %{_sysconfdir}/conman.conf
%config(noreplace) %{_sysconfdir}/[dls]*/conman
%{?_initrddir:%{_initrddir}}%{!?_initrddir:%{_sysconfdir}/init.d}/conman
%{_bindir}/*
%{_sbindir}/*
%{_prefix}/lib/*
%{_mandir}/*/*
