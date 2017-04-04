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
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

# Base package name
%define pname nrpe
%define PNAME %(tr [a-z] [A-Z] <<< %{pname})

%if 0%{?fedora} > 19
%global _hardened_build 1
%endif
%define nsport 5666

Name: %{pname}%{PROJ_DELIM}
Version: 3.0.1
Release: 2%{?dist}
Summary: Host/service/network monitoring agent for Nagios

Group: %{PROJ_NAME}/admin
License: GPLv2+
URL: http://www.nagios.org
DocDir: %{OHPC_PUB}/doc/contrib
Source0: https://github.com/NagiosEnterprises/nrpe/archive/%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Source1: nrpe.sysconfig
Source2: nrpe-tmpfiles.conf
Source3: nrpe.service
Source4: commands.cfg
Source5: hosts.cfg.example
Source6: services.cfg.example
Source7: OHPC_macros
Patch1: nrpe-0001-Add-reload-target-to-the-init-script.patch
Patch2: nrpe-0002-Read-extra-configuration-from-etc-sysconfig-nrpe.patch
Patch3: nrpe-0003-Include-etc-npre.d-config-directory.patch
Patch4: nrpe-0004-Fix-initscript-return-codes.patch
Patch5: nrpe-0005-Do-not-start-by-default.patch
Patch6: nrpe-0006-Relocate-pid-file.patch
Patch7: nrpe-0007-Add-condrestart-try-restart-target-to-initscript.patch
Patch8:	nrpe-0008-Allow-user-to-override-all-defaults-even-command-def.patch
# This should get removed whenever 2.16 is released, assuming it has the fix
# included. http://seclists.org/oss-sec/2014/q2/129. There's not upstream
# concensus that quoting arguments in a mode which is widely agreed upon to be
# risky so track upstream discussions here, too.
Patch9: nrpe-0009-CVE-2014-2913-nasty-metacharacters.patch

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

# For reconfiguration
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: openssl-devel
# OpenSSL package was split into openssl and openssl-libs in F18+
BuildRequires: openssl
#%if 0%{?fedora} > 17 || 0%{?rhel} > 6
BuildRequires:  systemd
#%endif

%if 0%{?sles_version} || 0%{?suse_version}
#!BuildIgnore: brp-check-suse
BuildRequires: -post-build-checks
%endif

%if 0%{?el4}%{?el5}
BuildRequires: tcp_wrappers
%else
%if 0%{?suse_version}
BuildRequires: tcpd-devel
%else
BuildRequires: tcp_wrappers-devel
%endif
%endif

Requires(pre): %{_sbindir}/useradd

%if 0%{?el4}%{?el5}%{?el6}
Requires(preun): /sbin/service, /sbin/chkconfig
Requires(post): /sbin/chkconfig, /sbin/service
Requires(postun): /sbin/service
Requires: initscripts
%else
Requires(post): systemd
Requires(preun): systemd
Requires(postun): systemd
%endif

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
%patch1 -p1 -b .reload
%patch2 -p1 -b .extra_config
%patch3 -p1 -b .include_etc_npre_d
%patch4 -p1 -b .initscript_return_codes
%patch5 -p1 -b .do_not_start_by_default
%patch6 -p1 -b .relocate_pid
%patch7 -p1 -b .condrestart
%patch8 -p1 -b .allow_override
%patch9 -p1

# Allow building for aarch64
# https://bugzilla.redhat.com/926244
%if 0%{?fedora} > 17 || 0%{?rhel} > 6
mv config.sub config.sub.old
mv config.guess config.guess.old
cp /usr/share/libtool/config/config.guess .
cp /usr/share/libtool/config/config.sub .
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
rm -rf %{buildroot}
%if 0%{?el4}%{?el5}%{?el6}
install -D -p -m 0755 init-script %{buildroot}/%{_initrddir}/nrpe
%else
install -D -m 0644 -p %{SOURCE3} %{buildroot}%{_unitdir}/%{pname}.service
%endif
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
%if 0%{?fedora} > 14 || 0%{?rhel} > 6
install -D -p -m 0644 %{SOURCE2} %{buildroot}%{_tmpfilesdir}/%{pname}.conf
%endif


%clean
rm -rf %{buildroot}

%pre
getent group %{pname} >/dev/null || groupadd -r %{pname}
getent passwd %{pname} >/dev/null || \
%{_sbindir}/useradd -c "NRPE user for the NRPE service" -d %{_localstatedir}/run/%{pname} -r -g %{pname} -s /sbin/nologin %{pname} 2> /dev/null || :

%preun
%if 0%{?el4}%{?el5}%{?el6}
if [ $1 = 0 ]; then
	/sbin/service %{pname} stop > /dev/null 2>&1 || :
	/sbin/chkconfig --del %{pname} || :
fi
%else
%systemd_preun nrpe.service
%endif

%post
%if 0%{?el4}%{?el5}%{?el6}
/sbin/chkconfig --add %{pname} || :
%else
#%systemd_post nrpe.service
/usr/bin/systemctl enable nrpe.service > /dev/null 2>&1 || :
/usr/bin/systemctl start nrpe.service  > /dev/null 2>&1 || :
%endif

%postun
%if 0%{?el4}%{?el5}%{?el6}
if [ "$1" -ge "1" ]; then
	/sbin/service %{pname} condrestart > /dev/null 2>&1 || :
fi
%else
#%systemd_postun_with_restart nrpe.service
if [ "$1" -ge "1" ]; then
	/usr/bin/systemctl reload-or-try-restart nrpe.service
else
	/usr/bin/systemctl disable nrpe.service
fi
%endif

%files
%if 0%{?el4}%{?el5}%{?el6}
%{_initrddir}/nrpe
%else
%{_unitdir}/%{pname}.service
%endif
%{_sbindir}/nrpe
%dir %{_sysconfdir}/nrpe.d
%config(noreplace) %{_sysconfdir}/nagios/nrpe.cfg
%attr( 640, root, nagios ) %config(noreplace) %{_sysconfdir}/nagios/conf.d/commands.cfg
%attr( 640, root, nagios ) %{_sysconfdir}/nagios/conf.d/hosts.cfg.example
%attr( 640, root, nagios ) %{_sysconfdir}/nagios/conf.d/services.cfg.example
%config(noreplace) %{_sysconfdir}/sysconfig/%{pname}
%if 0%{?fedora} > 14 || 0%{?rhel} > 6
%config(noreplace) %{_tmpfilesdir}/%{pname}.conf
%endif
%doc Changelog LEGAL README README.SSL SECURITY docs/NRPE.pdf
%dir %attr(775, %{pname}, %{pname}) %{_localstatedir}/run/%{pname}

%files -n nagios-plugins-nrpe%{PROJ_DELIM}
%{_libdir}/nagios/plugins/check_nrpe
%doc Changelog LEGAL README

%changelog
* Thu May 1 2014 Sam Kottler <skottler@fedoraproject.org> - 2.15.2
- Add patch to mitigate CVE-2014-2913

* Mon Jan 27 2014 Sam Kottler <skottler@fedoraproject.org> - 2.15.1
- Update to 2.15

* Wed Oct 16 2013 Peter Lemenkov <lemenkov@gmail.com> - 2.14-5
- Allow building for aarch64 (rhbz #926244)
- Allow user to redefine default commands (rhbz #963703)

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.14-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Wed May 22 2013 Kevin Fenzi <kevin@scrye.com> 2.14-3
- Apply patch from bug 860988 to handle RHEL versions and systemd
- Apply patch from bug 957567 to fix condrestart so nrpe restarts on upgrade.
- Rework systemd and service scriptlets and requires.
- Harden Fedora 19+ builds

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.14-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Mon Jan 14 2013 Mark Chappell <tremble@tremble.org.uk> - 2.14
- Version 2.14

* Mon Jan 14 2013 Mark Chappell <tremble@tremble.org.uk> - 2.13-2
- #860982 Mistake in service file
- #860985 nrpe shouldn't own /etc/nagios (from nagios-common)

* Mon Sep 17 2012 Peter Lemenkov <lemenkov@gmail.com> - 2.13-1
- Ver. 2.13

* Fri Jul 20 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.12-21
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Fri Jan 13 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.12-20
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Thu Sep 22 2011 Peter Lemenkov <lemenkov@gmail.com> - 2.12-19
- Disable systemd stuff in EPEL

* Sat Sep 17 2011 Ruben Kerkhof <ruben@rubenkerkhof.com> - 2.12-18
- Let systemd create /var/run/nrpe. Fixes rhbz #656641

* Tue Feb 08 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.12-17
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Mon Oct 25 2010 Peter Lemenkov <lemenkov@gmail.com> - 2.12-16
- Issue with SELinux was resolved (see rhbz #565220#c25). 2nd try.

* Wed Sep 29 2010 jkeating - 2.12-15
- Rebuilt for gcc bug 634757

* Sat Sep 11 2010 Peter Lemenkov <lemenkov@gmail.com> - 2.12-14
- Issue with SELinux was resolved (see rhbz #565220).

* Fri Jun 18 2010 Peter Lemenkov <lemenkov@gmail.com> - 2.12-13
- Init-script enhancements (see rhbz #247001, #567141 and #575544)

* Mon Oct 26 2009 Peter Lemenkov <lemenkov@gmail.com> - 2.12-12
- Do not own %%{_libdir}/nagios/plugins ( bz# 528974 )
- Fixed building against tcp_wrappers in Fedora ( bz# 528974 )

* Thu Sep 24 2009 Peter Lemenkov <lemenkov@gmail.com> - 2.12-11
- Fixed BZ# 515324

* Fri Aug 21 2009 Tomas Mraz <tmraz@redhat.com> - 2.12-10
- rebuilt with new openssl

* Sat Jul 25 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.12-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Wed Feb 25 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.12-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Sat Feb 21 2009 Mike McGrath <mmcgrath@redhat.com> - 2.12-7
- Re-fix for 477527

* Mon Feb  2 2009 Peter Lemenkov <lemenkov@gmail.com> - 2.12-6
- Fixed BZ# 449174
- Clean up (in order to disable rpmlint warnings)

* Sat Jan 17 2009 Tomas Mraz <tmraz@redhat.com> - 2.12-5
- rebuild with new openssl

* Sun Dec 21 2008 Mike McGrath <mmcgrath@redhat.com> - 2.12-4
- Added some doc lines for ticket 477527

* Fri Dec 19 2008 Mike McGrath <mmcgrath@redhat.com> - 2.12-3
- Added Provides: nagios-nrpe

* Fri Dec 19 2008 Mike McGrath <mmcgrath@redhat.com> - 2.12-2
- Upstreamreleased new version

* Tue Feb 12 2008 Mike McGrath <mmcgrath@redhat.com> - 2.7-6
- Rebuild for gcc43

* Wed Dec 05 2007 Release Engineering <rel-eng at fedoraproject dot org> - 2.7-5
 - Rebuild for deps

* Wed Aug 22 2007 Mike McGrath <mmcgrath@redhat.com> 2.7-4
- License Change
- Rebuild for BuildID

* Fri Feb 23 2007 Mike McGrath <mmcgrath@redhat.com> 2.7-1
- Upstream released new version

* Sun Jul 23 2006 Mike McGrath <imlinux@gmail.com> 2.5.2-3
- no longer owns libdir/nagios
- buildrequires tcp_wrappers

* Sun Jul 23 2006 Mike McGrath <imlinux@gmail.com> 2.5.2-2
- Specify bogus libdir so rpmlint won't complain

* Mon Jul 03 2006 Mike McGrath <imlinux@gmail.com> 2.5.2-1
- Upstream released new version

* Sun Mar 12 2006 Mike McGrath <imlinux@gmail.com> 2.4-3
- Added description to useradd statement

* Sun Mar 05 2006 Mike McGrath <imlinux@gmail.com> 2.4-2
- Added proper SMP build flags
- Added %{?dist} tag
- Added reload to nrpe script
- Updated to 2.4, changes include:
- Added option to allow week random seed (Gerhard Lausser)
- Added optional command line prefix (Sean Finney)
- Added ability to reload config file with SIGHUP
- Fixed bug with location of dh.h include file
- Fixed bug with disconnect message in debug mode

* Sat Feb 04 2006 Mike McGrath <imlinux@gmail.com> 2.3-1
- Created a Fedora friendly spec file

* Mon Jan 23 2006 Andreas Kasenides ank<@>cs.ucy.ac.cy
- fixed nrpe.cfg relocation to sample-config
- replaced Copyright label with License
- added --enable-command-args to enable remote arg passing (if desired can be disabled by commenting out)

* Wed Nov 12 2003 Ingimar Robertsson <iar@skyrr.is>
- Added adding of nagios group if it does not exist.

* Tue Jan 07 2003 James 'Showkilr' Peterson <showkilr@showkilr.com>
- Removed the lines which removed the nagios user and group from the system
- changed the patch release version from 3 to 1

* Mon Jan 06 2003 James 'Showkilr' Peterson <showkilr@showkilr.com>
- Removed patch files required for nrpe 1.5
- Update spec file for version 1.6 (1.6-1)

* Sat Dec 28 2002 James 'Showkilr' Peterson <showkilr@showkilr.com>
- First RPM build (1.5-1)
