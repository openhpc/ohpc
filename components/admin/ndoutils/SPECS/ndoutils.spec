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
%define pname ndoutils
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:               %{pname}%{PROJ_DELIM}
Version:            2.0.0
Release:            7%{?dist}
DocDir:             %{OHPC_PUB}/doc/contrib

Summary:            Stores all configuration and event data from Nagios in a database
Group:              Applications/System
License:            GPLv2 and BSD
# Bundled libpqueue header. It has been relicensed to BSD:
# https://github.com/vy/libpqueue/commit/de6480009c60afff22d4c7edf4353ef87797e497
URL:                http://www.nagios.org/download/addons/
BuildRoot:          %{_tmppath}/%{pname}-%{version}-%{release}-root-%(%{__id_u} -n)

Source0:            http://downloads.sourceforge.net/nagios/ndoutils-%{version}.tar.gz
Source1:            README.Fedora
Source2:            ndo2db.service
Source3:            ndo2db.init
Source4:            ndoutils.conf
Source5:            gpl-2.0.txt
# Fedora 21+: https://fedoraproject.org/wiki/Format-Security-FAQ
Patch0:             %{pname}-2.0.0-format-security.patch
# Better align with Fedora/Nagios places for temporary files
Patch1:             %{pname}-2.0.0-var-files.patch
# Set user/group in files section, fix permissions on install
Patch2:             %{pname}-2.0.0-install.patch

BuildRequires:      mysql-devel
Provides:           %{pname}

# Nagios is required also for user and group
%if 0%{?rhel} == 5
Requires:           nagios%{PROJ_DELIM} < 3
%else
Requires:           nagios%{PROJ_DELIM} >= 3
%endif

%if 0%{?fedora} || 0%{?rhel} >= 7
BuildRequires:      systemd
Requires(post):     systemd
Requires(preun):    systemd
Requires(postun):   systemd
%endif

%if 0%{?rhel} == 5 || 0%{?rhel} == 6
Requires(post):     /sbin/chkconfig
Requires(preun):    /sbin/chkconfig
Requires(preun):    /sbin/service
Requires(postun):   /sbin/service
%endif

%if 0%{?sles_version} || 0%{?suse_version}
#!BuildIgnore: brp-check-suse
BuildRequires: -post-build-checks
%endif

%description
The NDOUtils add on is designed to store all configuration and event data from
Nagios in a MySQL database. Storing information from Nagios in a database will
allow for quicker retrieval and processing of that data.

%prep
%setup -q -n %{pname}-%{version}
%patch0 -p1
%patch1 -p1
%patch2 -p1

# Remove executable bits from the database scripts in the documentation.
chmod 644 db/installdb db/prepsql db/upgradedb

%build
%configure \
    --bindir=%{_sbindir} \
    --sysconfdir=%{_sysconfdir}/nagios
make %{?_smp_mflags}

%install
rm -rf %{buildroot}
cp %{SOURCE1} %{SOURCE5} .
mkdir -p %{buildroot}%{_localstatedir}/log/nagios
mkdir -p %{buildroot}%{_localstatedir}/cache/ndoutils
mkdir -p %{buildroot}%{_libdir}/nagios/brokers

# Nagios 4 support + common components
%make_install

# Nagios 2 support (override)
%if 0%{?rhel} == 5
    pushd src
    make install-2x DESTDIR=%{?buildroot}
    popd
%endif

# Nagios 3 support (override)
%if 0%{?rhel} >= 6 || 0%{?fedora}
    pushd src
    make install-3x DESTDIR=%{?buildroot}
    popd
%endif

mv %{buildroot}%{_sbindir}/ndomod.o \
    %{buildroot}%{_libdir}/nagios/brokers/ndomod.so

# Configuration files
make install-config DESTDIR=%{?buildroot}
mv %{buildroot}%{_sysconfdir}/nagios/ndo2db.cfg-sample \
    %{buildroot}%{_sysconfdir}/nagios/ndo2db.cfg
mv %{buildroot}%{_sysconfdir}/nagios/ndomod.cfg-sample \
    %{buildroot}%{_sysconfdir}/nagios/ndomod.cfg

# Remove some spurious permissions from docs
find docs/html -name "*.*" -exec chmod 644 {} \;

%if 0%{?fedora} || 0%{?rhel} >= 7

# Systemd unit files
mkdir -p %{buildroot}%{_unitdir}
install -p -m 644 -D %{SOURCE2} %{buildroot}%{_unitdir}/ndo2db.service

# Runtime files (tmpfs)
#mkdir -p %{buildroot}/run/%{pname}
install -p -m 644 -D %{SOURCE4} %{buildroot}%{_tmpfilesdir}/ndoutils.conf

%else

# Initscripts
mkdir -p %{buildroot}%{_initrddir}
install -p -m 755 -D %{SOURCE3} %{buildroot}%{_initrddir}/ndo2db

# Runtime files
#mkdir -p %{buildroot}%{_localstatedir}/run/ndoutils

%endif
mkdir -p %{buildroot}%{_localstatedir}/run/ndoutils

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
%doc db README.Fedora gpl-2.0.txt
%doc docs/html README REQUIREMENTS TODO UPGRADING
%config(noreplace) %{_sysconfdir}/nagios/ndo2db.cfg
%config(noreplace) %{_sysconfdir}/nagios/ndomod.cfg
%dir %attr(-,nagios,root) %{_localstatedir}/cache/ndoutils
# Currently the one and only broker, but probably the brokers directory
# should belong to nagios-common.
%{_libdir}/nagios/brokers
%{_sbindir}/file2sock
%{_sbindir}/log2ndo
%{_sbindir}/ndo2db
%{_sbindir}/sockdebug

%if 0%{?fedora} || 0%{?rhel} >= 7

#%dir %attr(-,nagios,root) /run/%{pname}/
%{_tmpfilesdir}/ndoutils.conf
%{_unitdir}/ndo2db.service

%else

#%dir %attr(-,nagios,root) %{_localstatedir}/run/ndoutils
%{_initrddir}/ndo2db

%endif
%dir %attr(-,nagios,root) %{_localstatedir}/run/ndoutils

%if 0%{?fedora} || 0%{?rhel} >= 7

%post
%systemd_post ndo2db.service

%preun
%systemd_preun ndo2db.service

%postun
%systemd_postun_with_restart ndo2db.service

%endif

%if 0%{?rhel} == 6

%post
/sbin/chkconfig --add ndo2db

%preun
if [ "$1" = 0 ]; then
    /sbin/service ndo2db stop >/dev/null 2>&1 || :
    /sbin/chkconfig --del ndo2db
fi

%postun
if [ "$1" -ge "1" ]; then
    /sbin/service ndo2db condrestart >/dev/null 2>&1 || :
fi

%endif

%changelog
* Fri Jun 27 2014 Simone Caronni <negativo17@gmail.com> - 2.0.0-7
- Fix runtime directory packaging.

* Fri Jun 27 2014 Simone Caronni <negativo17@gmail.com> - 2.0.0-6
- Install Nagios 3 binaries on RHEL 7 (#1113757).
- Fix Fedora and RHEL 7 temporary directories (#1113767).
- Adjust service file for RHEL 7 / Fedora (#1113767).

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.0.0-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Thu May 15 2014 Simone Caronni <negativo17@gmail.com> - 2.0.0-4
- Add licensing notes.

* Fri Apr 18 2014 Simone Caronni <negativo17@gmail.com> - 2.0.0-3
- Fix executable permissions.
- Add GPL2 license file.

* Wed Apr 16 2014 Simone Caronni <negativo17@gmail.com> - 2.0.0-2
- Add patch for GCC format-security.
- Change layout of temporary files to be more aligned with Nagios.

* Mon Mar 10 2014 Simone Caronni <negativo17@gmail.com> - 2.0.0-1
- Update to 2.0.0.
- Remove multiple database support, it never happened.

* Mon Feb 24 2014 Simone Caronni <negativo17@gmail.com> - 1.5.2-1
- Updated to 1.5.2.
- Updated SPEC file for current packaging guidelines.
- Use only generated docs for binary package.

* Wed Nov 11 2009 Steve Traylen <tmraz@redhat.com> - 1.4-0.7.b9
- New upstream version. 1.4b9

* Fri Aug 21 2009 Tomas Mraz <tmraz@redhat.com> - 1.4-0.7.b8
- rebuilt with new openssl

* Tue Jul 21 2009 Steve Traylen <steve.traylen@cern.ch> - 1.4-0.6.b8
- A requires nagios = 2 will not work.

* Tue Jul 21 2009 Steve Traylen <steve.traylen@cern.ch> - 1.4-0.5.b8
- Updated to nodutils 1.4b8.
- mysql lib path no longer needs to be set explicitly.
- Use dist tags to install for nagios2 on el4 and el5.

* Mon Jul 20 2009 Steve Traylen <steve.traylen@cern.ch> - 1.4-0.4.b7
- Patch ndomod.o to be ndomod.so since it is a shared object.
- Move ndomod.so from /usr/lib to /usr/lib/nagios/brokers
- Change URL to better one.
- Change SourceURL to fedora package guideline for sourceforge.
- Completly removed postgres support. The documents clearly state
  it is not supported.

* Sun Jun 14 2009 Steve Traylen <steve@traylen.net> - 1.4-0.3.b7
- Move ndo2db.cfg and ndomod.cfg to /etc/nagios as per install
  guide.
- Remove executable bits from documentation examples.

* Thu Jun 11 2009 Steve Traylen <steve@traylen.net> - 1.4-0.2.b7
- Split package to create postgres and mysql binary in different
  sub packages.

* Wed Jun 10 2009 Steve Traylen <steve@traylen.net> - 1.4-0.1.b7
- Add full URL location to Source0:
- Use special Version/Release tag since a beta. i.e 1.4-0.1.b7

* Sat Apr 25 2009 Steve Traylen <steve@traylen.net> 
- First Build
