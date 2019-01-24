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
%define pname ndoutils

Name:               %{pname}%{PROJ_DELIM}
Version:            2.1.3
Release:            7%{?dist}

Summary:            Stores all configuration and event data from Nagios in a database
Group:              %{PROJ_NAME}/admin
License:            GPLv2 and BSD
# Bundled libpqueue header. It has been relicensed to BSD:
# https://github.com/vy/libpqueue/commit/de6480009c60afff22d4c7edf4353ef87797e497
URL:                http://www.nagios.org/download/addons/

Source0:            http://downloads.sourceforge.net/nagios/ndoutils-%{version}.tar.gz
Source1:            README.Fedora
Source2:            ndo2db.service
Source3:            ndo2db.init
Source4:            ndoutils.conf
Source5:            gpl-2.0.txt
# Fedora 21+: https://fedoraproject.org/wiki/Format-Security-FAQ
Patch0:             %{pname}-2.1.2-format-security.patch
# Better align with Fedora/Nagios places for temporary files
Patch1:             %{pname}-2.1.2-var-files.patch
# Set user/group in files section, fix permissions on install
Patch2:             %{pname}-2.1.3-install.patch

BuildRequires:      mysql-devel
Provides:           %{pname}

# Nagios is required also for user and group
Requires:           nagios >= 4

%if 0%{?fedora} || 0%{?rhel} >= 7
BuildRequires:      systemd
Requires(post):     systemd
Requires(preun):    systemd
Requires(postun):   systemd
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
make %{?_smp_mflags} all

%install
cp %{SOURCE1} %{SOURCE5} .
mkdir -p %{buildroot}%{_localstatedir}/log/nagios
mkdir -p %{buildroot}%{_localstatedir}/cache/ndoutils
mkdir -p %{buildroot}%{_libdir}/nagios/brokers

# Nagios 4 support + common components
%make_install

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

%files
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
%{_tmpfilesdir}/ndoutils.conf
%{_unitdir}/ndo2db.service
%else
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
