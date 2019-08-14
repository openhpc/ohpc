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
%define pname nrpe

%define nsport 5666

Name: %{pname}%{PROJ_DELIM}
Version: 3.2.0
Release: 2%{?dist}
Summary: Host/service/network monitoring agent for Nagios

Group: %{PROJ_NAME}/admin
License: GPLv2+
URL: http://www.nagios.org
Source0: https://github.com/NagiosEnterprises/nrpe/releases/download/%{pname}-%{version}/%{pname}-%{version}.tar.gz
Source1: nrpe.sysconfig
Source2: nrpe-tmpfiles.conf
Source3: nrpe.service
Source4: commands.cfg
Source5: hosts.cfg.example
Source6: services.cfg.example
Patch1: nrpe-0003-Include-etc-npre.d-config-directory.patch

BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: openssl-devel
# OpenSSL package was split into openssl and openssl-libs in F18+
BuildRequires: openssl
BuildRequires:  systemd

%if 0%{?sles_version} || 0%{?suse_version}
#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks
%endif

%if 0%{?suse_version}
BuildRequires: tcpd-devel
%else
BuildRequires: tcp_wrappers-devel
%endif

Requires(pre): %{_sbindir}/useradd
Requires(post): systemd
Requires(preun): systemd
Requires(postun): systemd

# owns /etc/nagios
Requires: nagios-common%{PROJ_DELIM}
Provides: nagios-nrpe%{PROJ_DELIM} = %{version}-%{release}
Provides: nagios-nrpe = %{version}-%{release}

%description
Nrpe is a system daemon that will execute various Nagios plugins
locally on behalf of a remote (monitoring) host that uses the
check_nrpe plugin.  Various plugins that can be executed by the
daemon are available at:
http://sourceforge.net/projects/nagiosplug

This package provides the core agent.

%package -n nagios-plugins-nrpe%{PROJ_DELIM}
Group: Applications/System
Summary: Provides nrpe plugin for Nagios
Requires: nagios-plugins%{PROJ_DELIM}
Provides: check_nrpe = %{version}-%{release}

%description -n nagios-plugins-nrpe%{PROJ_DELIM}
Nrpe is a system daemon that will execute various Nagios plugins
locally on behalf of a remote (monitoring) host that uses the
check_nrpe plugin.  Various plugins that can be executed by the
daemon are available at:
http://sourceforge.net/projects/nagiosplug

This package provides the nrpe plugin for Nagios-related applications.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1 -b .include_etc_npre_d

# Allow building for aarch64
# https://bugzilla.redhat.com/926244
%if 0%{?rhel}
pushd build-aux
mv config.sub config.sub.old
mv config.guess config.guess.old
cp /usr/share/libtool/config/config.guess .
cp /usr/share/libtool/config/config.sub .
popd
%endif

%build
CFLAGS="$RPM_OPT_FLAGS" CXXFLAGS="$RPM_OPT_FLAGS" LDFLAGS="%{?__global_ldflags}" \
./configure \
	--with-init-dir=%{_initrddir} \
	--with-nrpe-port=%{nsport} \
	--with-nrpe-user=nrpe \
	--with-nrpe-group=nrpe \
	--bindir=%{_sbindir} \
	--libdir=/doesnt/matter/ \
	--libexecdir=%{_libdir}/nagios/plugins \
	--datadir=%{_datadir}/nagios \
	--sysconfdir=%{_sysconfdir}/nagios \
	--localstatedir=%{_localstatedir}/log/nagios \
	--enable-command-args
make %{?_smp_mflags} all

%install
install -D -m 0644 -p %{SOURCE3} %{buildroot}%{_unitdir}/%{pname}.service
install -D -p -m 0644 sample-config/nrpe.cfg %{buildroot}/%{_sysconfdir}/nagios/%{pname}.cfg
mkdir -p %{buildroot}/%{_sysconfdir}/nagios/conf.d # install -D... this should be your job
install -D -p -m 0640 %{SOURCE4} %{buildroot}/%{_sysconfdir}/nagios/conf.d/commands.cfg
install -D -p -m 0640 %{SOURCE5} %{buildroot}/%{_sysconfdir}/nagios/conf.d/hosts.cfg.example
install -D -p -m 0640 %{SOURCE6} %{buildroot}/%{_sysconfdir}/nagios/conf.d/services.cfg.example
install -D -p -m 0755 src/nrpe %{buildroot}/%{_sbindir}/nrpe
install -D -p -m 0755 src/check_nrpe %{buildroot}/%{_libdir}/nagios/plugins/check_nrpe
install -D -p -m 0644 %{SOURCE1} %{buildroot}/%{_sysconfdir}/sysconfig/%{pname}
install -d %{buildroot}%{_sysconfdir}/nrpe.d
install -d %{buildroot}%{_localstatedir}/run/%{pname}
%if 0%{?rhel} || 0%{?sle_version} >= 150000
install -D -p -m 0644 %{SOURCE2} %{buildroot}%{_tmpfilesdir}/%{pname}.conf
%endif


%pre
getent group %{pname} >/dev/null || groupadd -r %{pname}
getent passwd %{pname} >/dev/null || \
%{_sbindir}/useradd -c "NRPE user for the NRPE service" -d %{_localstatedir}/run/%{pname} -r -g %{pname} -s /sbin/nologin %{pname} 2> /dev/null || :

%preun
%systemd_preun nrpe.service

%post
/usr/bin/systemctl enable nrpe.service > /dev/null 2>&1 || :
/usr/bin/systemctl start nrpe.service  > /dev/null 2>&1 || :

%postun
if [ "$1" -ge "1" ]; then
	/usr/bin/systemctl reload-or-try-restart nrpe.service
else
	/usr/bin/systemctl disable nrpe.service
fi

%files
%{_unitdir}/%{pname}.service
%{_sbindir}/nrpe
%dir %{_sysconfdir}/nrpe.d
%config(noreplace) %{_sysconfdir}/nagios/nrpe.cfg
%attr( 640, root, nagios ) %config(noreplace) %{_sysconfdir}/nagios/conf.d/commands.cfg
%attr( 640, root, nagios ) %{_sysconfdir}/nagios/conf.d/hosts.cfg.example
%attr( 640, root, nagios ) %{_sysconfdir}/nagios/conf.d/services.cfg.example
%config(noreplace) %{_sysconfdir}/sysconfig/%{pname}
%config(noreplace) %{_tmpfilesdir}/%{pname}.conf
%doc CHANGELOG.md LEGAL README.md README.SSL.md SECURITY.md docs/NRPE.pdf
%dir %attr(775, %{pname}, %{pname}) %{_localstatedir}/run/%{pname}

%files -n nagios-plugins-nrpe%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_nrpe
%doc CHANGELOG.md LEGAL README.md
