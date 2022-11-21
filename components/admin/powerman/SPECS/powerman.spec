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

%define pname powerman

Name:  %{pname}%{PROJ_DELIM}
Version: 2.3.26
Release: 1%{?dist}

Summary: PowerMan - centralized power control for clusters
License: GPL
Group: %{PROJ_NAME}/admin
Url: http://code.google.com/p/powerman/
Source0: https://github.com/chaos/%{pname}/releases/download/%{version}/%{pname}-%{version}.tar.gz

%if 0%{?rhel}
%define _with_httppower 1
%define _with_snmppower 1
%define _with_genders 1
%endif

%if 0%{?_with_genders}
BuildRequires: genders%{PROJ_DELIM}
%endif
%if 0%{?_with_httppower}
BuildRequires: curl-devel
%endif
%if 0%{?_with_snmppower}
BuildRequires: net-snmp-devel
%endif
BuildRequires: systemd
BuildRequires: make, gcc

%package -n %{pname}-devel%{PROJ_DELIM}
Requires: %{name} = %{version}-%{release}
Summary: Headers and libraries for developing applications using PowerMan
Group: Development/Libraries

%package -n %{pname}-libs%{PROJ_DELIM}
Requires: %{name} = %{version}-%{release}
Summary: Libraries for applications using PowerMan
Group: System Environment/Libraries

%description
PowerMan is a tool for manipulating remote power control (RPC) devices from a
central location. Several RPC varieties are supported natively by PowerMan and
Expect-like configurability simplifies the addition of new devices.

%description -n %{pname}-devel%{PROJ_DELIM}
A header file and static library for developing applications using PowerMan.

%description -n %{pname}-libs%{PROJ_DELIM}
A shared library for applications using PowerMan.

%prep
%setup -n %{pname}-%{version}

%build
%configure --program-prefix=%{?_program_prefix:%{_program_prefix}} \
  %{?_with_genders: --with-genders} \
  %{?_with_httppower: --with-httppower} \
  %{?_with_snmppower: --with-snmppower}

make

%install
make install DESTDIR=$RPM_BUILD_ROOT

# Put the systemd directory in the right location. -JCSIADAL 3/29/20
%{__mv} $RPM_BUILD_ROOT/usr/usr/lib/systemd $RPM_BUILD_ROOT/usr/lib/
%{__rm} -rd $RPM_BUILD_ROOT/usr/usr

#drop local state dir to avoid making systemd angry when it creates the statedir on start
%{__rm} -rf $RPM_BUILD_ROOT/%{_localstatedir}

%post
/bin/systemctl enable powerman > /dev/null 2>&1 || :

%post -n %{pname}-libs%{PROJ_DELIM}
if [ -x /sbin/ldconfig ]; then /sbin/ldconfig %{_libdir}; fi

%preun
if [ "$1" = 0 ]; then
  systemctl stop powerman >/dev/null 2>&1 || :
  systemctl disable powerman > /dev/null 2>&1 || :
fi

%postun
if [ "$1" -ge 1 ]; then
  systemctl try-restart powerman >/dev/null 2>&1 || :
fi

%postun -n %{pname}-libs%{PROJ_DELIM}
if [ -x /sbin/ldconfig ]; then /sbin/ldconfig %{_libdir}; fi

%files
%doc DISCLAIMER
%license COPYING
%doc NEWS
%doc TODO
%{_bindir}/powerman
%{_bindir}/pm
%{_sbindir}/powermand
%{_sbindir}/vpcd
%if 0%{?_with_httppower}
%{_sbindir}/httppower
%endif
%if 0%{?_with_snmppower}
%{_sbindir}/snmppower
%endif
%{_sbindir}/plmpower
%dir %config %{_sysconfdir}/powerman
%{_sysconfdir}/powerman/*.dev
%{_sysconfdir}/powerman/powerman.conf.example
%{_mandir}/*1/*
%{_mandir}/*5/*
%{_mandir}/*8/*
%{_libdir}/stonith/plugins/external/powerman
%dir %attr(0755,daemon,root) %{_libdir}/stonith
%dir %attr(0755,daemon,root) %{_libdir}/stonith/plugins
%dir %attr(0755,daemon,root) %{_libdir}/stonith/plugins/external
%attr(0644,root,root) %{_unitdir}/powerman.service
%config %attr(0644,root,root) %{_tmpfilesdir}/powerman.conf

%files -n %{pname}-devel%{PROJ_DELIM}
%{_includedir}/*
%{_libdir}/*.la
%{_mandir}/*3/*
%ifnos aix5.3 aix5.2 aix5.1 aix5.0 aix4.3
%{_libdir}/*.a
%{_libdir}/*.so
%{_libdir}/pkgconfig/*
%endif

%files -n %{pname}-libs%{PROJ_DELIM}
%ifnos aix5.3 aix5.2 aix5.1 aix5.0 aix4.3
%{_libdir}/*.so.*
%else
%{_libdir}/*.a
%endif
