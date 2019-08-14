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
%define pname nagios

%global _hardened_build 1

Name:    %{pname}%{PROJ_DELIM}
Version: 4.4.3
Release: 1%{?dist}
Summary: Host/service/network monitoring program
Group:   %{PROJ_NAME}/admin
License: GPLv2
URL: http://www.nagios.org/
Source0: https://assets.nagios.com/downloads/nagioscore/releases/nagios-%{version}.tar.gz
Source1: nagios.logrotate
Source2: nagios.htaccess
Source3: nagios.internet.cfg
Source7: nagios.service
# PNG files from the old nagios-0010-Added-several-images-to-the-sample-config.patch
Source10: printer.png
Source11: router.png
Source12: switch.png

# looks fixed in 4.1.1
#Patch1: nagios-0001-from-rpm.patch
Patch2: nagios-0002-SELinux-relabeling.patch
# Sent upstream
Patch3: nagios-0003-Fix-etc-init.d-nagios-status.patch
# Sent upstream
Patch4: nagios-0004-Fix-installation-of-httpd-conf.d-config-file.patch
Patch5: nagios-0005-Install-config-files-too.patch
Patch6: nagios-0006-Do-not-start-service-by-default.patch
# Sent upstream
Patch7: nagios-0007-The-init-script-should-return-2-in-case-of-unknown-c.patch
Patch8: nagios-0008-Fix-path-to-CGI-executables.patch
Patch10: nagios-0010-Added-several-images-to-the-sample-config-revb.patch
Patch11: nagios-0011-Fixed-strange-permissions.patch
Patch12: nagios-3.4.3-httpd-2.4-and-2.2.patch
Patch14: nagios-3.5.0-conf.d-configuration-directory.patch
Patch15: nagios-0012-add-aarch64-support.patch

BuildRequires: gd-devel > 1.8, mailx, libjpeg-devel, libpng-devel
BuildRequires: perl(CPAN)
BuildRequires: perl(ExtUtils::MakeMaker)
BuildRequires: perl(ExtUtils::Embed)
BuildRequires: perl(Test::Harness)
#%if 0%{?rhel} > 6 || 0%{?fedora}
#BuildRequires: perl(Test::HTML::Lint)
#%endif
BuildRequires: perl(Test::More)
BuildRequires: perl(Test::Simple)
BuildRequires: unzip

%if 0%{?sle_version} || 0%{?suse_version}
#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks
BuildRequires: procps
%else
BuildRequires: procps-ng
%endif

Requires: httpd
Requires: perl(:MODULE_COMPAT_%(eval "`%{__perl} -V:version`"; echo $version))
Requires: mailx

%if 0%{?suse_version} >= 1210
Requires:           apache2-mod_php5
%endif

%if 0%{?fedora} || 0%{?rhel}
Requires: php
%else
Requires(preun): wicked-service,aaa_base
Requires(post): wicked-service,aaa_base
Requires(postun): wicked-service
Requires: php5
%endif

%{?systemd_requires}
BuildRequires: systemd

Requires: %{pname}-common%{PROJ_DELIM}
# OBS, if you're going to parse Requires you need to match what RPM does or you'll just cause problems
#Requires: user(nagios)
#Requires(pre): user(nagios)
#Requires: group(nagios)
#Requires(pre): group(nagios)


%description
Nagios is a program that will monitor hosts and services on your
network.  It has the ability to send email or page alerts when a
problem arises and when a problem is resolved.  Nagios is written
in C and is designed to run under Linux (and some other *NIX
variants) as a background process, intermittently running checks
on various services that you specify.

The actual service checks are performed by separate "plugin" programs
which return the status of the checks to Nagios. The plugins are
available at http://sourceforge.net/projects/nagiosplug.

This package provides the core program, web interface, and documentation
files for Nagios. Development files are built as a separate package.

%package -n %{pname}-common%{PROJ_DELIM}
Group: Applications/System
Summary: Provides common directories, uid and gid among nagios-related packages.
%if 0%{?fedora} || 0%{?rhel}
Requires(pre): shadow-utils
Requires(post): shadow-utils
%else
Requires(pre): shadow
Requires(post): shadow
%endif
Provides: user(nagios)
Provides: group(nagios)
Provides: %{pname}-common


%description -n %{pname}-common%{PROJ_DELIM}
Provides common directories, uid and gid among nagios-related packages.


%package -n %{pname}-devel%{PROJ_DELIM}
Group: Applications/System
Summary: Provides include files that Nagios-related applications may compile against
Requires: %{name} = %{version}-%{release}


%description -n %{pname}-devel%{PROJ_DELIM}
Nagios is a program that will monitor hosts and services on your
network. It has the ability to email or page you when a problem arises
and when a problem is resolved. Nagios is written in C and is
designed to run under Linux (and some other *NIX variants) as a
background process, intermittently running checks on various services
that you specify.

This package provides include files that Nagios-related applications
may compile against.


%prep
%setup -q -n %{pname}-%{version}
# appears to be corrected in 4.1.1
#%patch1 -p1 -b .fedora
%patch2 -p1 -b .selinux_relabel
%patch3 -p1 -b .fix_status_retcode
%patch4 -p1 -b .fix_httpd_conf_d
%patch5 -p1 -b .install_config
%patch6 -p1 -b .dont_start_by_default
%patch7 -p1 -b .return_2
%patch8 -p1 -b .passwd
%patch10 -p1 -b .more_images
%patch11 -p1 -b .fix_perms
%patch12 -p1 -b .httpd_conf
%patch14 -p1 -b .conf_d
%patch15 -p1

install -p -m 0644 %{SOURCE10} %{SOURCE11} %{SOURCE12} html/images/logos/


%build
%configure \
    --prefix=%{_datadir}/%{pname} \
    --exec-prefix=%{_localstatedir}/lib/%{pname} \
    --with-init-dir=/etc/init.d \
    --with-cgiurl=/%{pname}/cgi-bin/ \
    --with-htmlurl=/%{pname} \
    --with-lockfile=%{_localstatedir}/run/%{pname}/%{pname}.pid \
    --libdir=%{_libdir}/%{pname} \
    --with-nagios-user=nagios \
    --with-nagios-grp=nagios \
    --bindir=%{_sbindir} \
    --sbindir=%{_libdir}/%{pname}/cgi-bin \
    --libexecdir=%{_libdir}/%{pname}/plugins \
    --sysconfdir=%{_sysconfdir}/%{pname} \
    --localstatedir=%{_localstatedir}/log/%{pname} \
    --datadir=%{_datadir}/%{pname}/html \
    --with-gd-lib=%{_libdir} \
    --with-gd-inc=%{_includedir} \
    --enable-embedded-perl \
    --with-perlcache \
    --with-template-objects \
    --with-template-extinfo \
    %ifarch aarch64
    --with-initdir=%{_unitdir} \
    %endif
    %if 0%{?sle_version} || 0%{?suse_version}
    --with-httpd-conf=/etc/apache2/conf.d \
    %else
    --with-httpd-conf=/etc/httpd/conf.d \
    %endif
    STRIP=/bin/true
make %{?_smp_mflags} all

#sed -i -e "s| package Embed::Persistent;|#\!%{_bindir}/perl\npackage Embed::Persistent;|" p1.pl
sed -i -e "s|NagiosCmd=/var/log/nagios/rw/nagios.cmd|NagiosCmd=%{_localstatedir}/spool/%{pname}/cmd/nagios.cmd|" startup/default-init
sed -i -e "s|resource.cfg|private/resource.cfg|" \
     -e "s|#query_socket=/var/log/nagios/rw/nagios.qh|query_socket=%{_localstatedir}/log/%{pname}/nagios.qh|" \
     -e "s|command_file=/var/log/nagios/rw/nagios.cmd|command_file=%{_localstatedir}/spool/%{pname}/cmd/nagios.cmd|" sample-config/nagios.cfg 
sed -e "s|/usr/lib/|%{_libdir}/|" %{SOURCE2} > %{pname}.htaccess
cp -f %{SOURCE3} internet.cfg
echo >> html/stylesheets/common.css


%install
make DESTDIR=%{buildroot} INIT_OPTS="" INSTALL_OPTS="" COMMAND_OPTS="" CGIDIR="%{_libdir}/%{pname}/cgi-bin" CFGDIR="%{_sysconfdir}/%{pname}" fullinstall

# relocated to sbin (Fedora-specific)
install -d -m 0755 %{buildroot}%{_bindir}
mv %{buildroot}%{_sbindir}/nagiostats %{buildroot}%{_bindir}/nagiostats

install -d -m 0755 %{buildroot}%{_sysconfdir}/%{pname}/private
mv %{buildroot}%{_sysconfdir}/%{pname}/resource.cfg %{buildroot}%{_sysconfdir}/%{pname}/private/resource.cfg

install -d -m 0755 %{buildroot}%{_sysconfdir}/%{pname}/conf.d/
touch %{buildroot}%{_sysconfdir}/%{pname}/passwd

# Install header-file
install -D -m 0644 include/locations.h %{buildroot}%{_includedir}/%{pname}/locations.h

# Install logrotate rule
install -D -m 0644 %{SOURCE1} %{buildroot}%{_sysconfdir}/logrotate.d/%{pname}

# Make room for event-handlers
install -d -m 0755 %{buildroot}%{_libdir}/%{pname}/plugins/eventhandlers

install -d -m 0775 %{buildroot}%{_localstatedir}/spool/%{pname}/cmd
install -d -m 0775 %{buildroot}%{_localstatedir}/run/%{pname}

# Install systemd entry
install -D -m 0644 -p %{SOURCE7} %{buildroot}%{_unitdir}/%{pname}.service

# remove static library that is build in 4.1.1
rm -v    %{buildroot}%{_libdir}/%{pname}/libnagios.a


%pre -n %{pname}-common%{PROJ_DELIM}
getent group nagios >/dev/null || groupadd -r nagios
getent passwd nagios >/dev/null || useradd -r -g nagios -d %{_localstatedir}/spool/%{pname} -s /sbin/nologin nagios
exit 0


%preun
%systemd_preun %{pname}.service

%post
%if 0%{?sle_version} || 0%{?suse_version}
/usr/sbin/a2enmod php5
/usr/sbin/a2enmod version
%{_sbindir}/usermod -a -G %{pname} wwwrun || :
/usr/bin/systemctl try-restart apache2 > /dev/null 2>&1 || :
%else
%{_sbindir}/usermod -a -G %{pname} apache || :
/usr/bin/systemctl try-restart httpd > /dev/null 2>&1 || :
%endif
%systemd_post %{pname}.service


%postun
%if 0%{?sle_version} || 0%{?suse_version}
/usr/bin/systemctl try-restart apache2 > /dev/null 2>&1 || :
%else
/usr/bin/systemctl try-restart httpd > /dev/null 2>&1 || :
%endif


%files
%dir %{_libdir}/%{pname}/plugins/eventhandlers
%dir %{_libdir}/%{pname}/cgi-bin
%dir %{_datadir}/%{pname}
%dir %{_datadir}/%{pname}/html
%doc %{_datadir}/%{pname}/html/docs
%doc Changelog INSTALLING LICENSE README.md UPGRADING
%doc internet.cfg
%{_datadir}/%{pname}/html/[^d]*
# d3 javascript appears to be new for 4.1.1, include directory and files
%dir %{_datadir}/%{pname}/html/d3
%{_datadir}/%{pname}/html/d3/*
%{_sbindir}/*
%{_bindir}/*
%{_libdir}/%{pname}/cgi-bin/*cgi
%{_unitdir}/%{pname}.service

%ifarch aarch64
%if 0%{?sle_version} || 0%{?suse_version}
/usr/lib/systemd/system/nagios.service
%endif
%else  # Arch is not aarch64
%if 0%{?sle_version} >= 150000
/usr/lib/systemd/system/nagios.service
%else # Arch is not aarch64 and OS is not SLE15 or later
/etc/init.d/nagios
%endif 
%endif # ifarch

%if 0%{?sle_version} || 0%{?suse_version}
%config(noreplace) %{_sysconfdir}/apache2/conf.d/nagios.conf
%else
%config(noreplace) %{_sysconfdir}/httpd/conf.d/nagios.conf
%endif

%config(noreplace) %{_sysconfdir}/logrotate.d/%{pname}
%config(noreplace) %{_sysconfdir}/%{pname}/*cfg
%config(noreplace) %{_sysconfdir}/%{pname}/objects/*cfg
%attr(0750,root,nagios) %dir %{_sysconfdir}/%{pname}/private
%attr(0750,root,nagios) %dir %{_sysconfdir}/%{pname}/objects
%attr(0750,root,nagios) %dir %{_sysconfdir}/%{pname}/conf.d
%attr(0640,root,nagios) %config(noreplace) %{_sysconfdir}/%{pname}/private/resource.cfg
%if 0%{?sle_version} || 0%{?suse_version}
%attr(0640,root,www) %config(noreplace) %{_sysconfdir}/%{pname}/passwd
%attr(0640,root,www) %config(noreplace) %{_datadir}/%{pname}/html/config.inc.php
%else
%attr(0640,root,apache) %config(noreplace) %{_sysconfdir}/%{pname}/passwd
%attr(0640,root,apache) %config(noreplace) %{_datadir}/%{pname}/html/config.inc.php
%endif
%attr(2775,nagios,nagios) %dir %{_localstatedir}/spool/%{pname}/cmd
%attr(0750,nagios,nagios) %dir %{_localstatedir}/run/%{pname}
%attr(0750,nagios,nagios) %dir %{_localstatedir}/log/%{pname}
%attr(0750,nagios,nagios) %dir %{_localstatedir}/log/%{pname}/archives
%attr(0750,nagios,nagios) %dir %{_localstatedir}/log/%{pname}/spool/
%attr(0750,nagios,nagios) %dir %{_localstatedir}/log/%{pname}/spool/checkresults


%files -n %{pname}-common%{PROJ_DELIM}
%dir %{_sysconfdir}/%{pname}
%dir %{_libdir}/%{pname}
%dir %{_libdir}/%{pname}/plugins
%attr(0755,nagios,nagios) %dir %{_localstatedir}/spool/%{pname}


%files -n %{pname}-devel%{PROJ_DELIM}
%{_includedir}/%{pname}

