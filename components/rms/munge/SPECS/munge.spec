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

%define pname munge


Name:           %{pname}%{PROJ_DELIM}
Version:	0.5.13
Release:	1%{?dist}

Summary:	MUNGE authentication service
Group:		%{PROJ_NAME}/rms
License:	GPLv3+ and LGPLv3+
URL:		http://dun.github.io/munge/
Requires:	%{pname}-libs%{PROJ_DELIM} = %{version}-%{release}

%if 0%{?suse_version} >= 1100
BuildRequires:	libbz2-devel
BuildRequires:	libopenssl-devel
BuildRequires:	zlib-devel
%if 0%{?suse_version} >= 1230
BuildRequires:	systemd
%endif
%else
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:	bzip2
BuildRequires:	openssl-devel
BuildRequires:	zlib-devel
%else
BuildRequires:	bzip2-devel
BuildRequires:	openssl-devel
BuildRequires:	zlib-devel
BuildRequires:	systemd
%endif
%endif
#!BuildIgnore: post-build-checks

Conflicts: munge 

Source0:   https://github.com/dun/munge/archive/munge-%{version}.tar.gz
# 6/12/14 karl.w.schulz@intel.com - logdir patch for use with Warewulf
Patch1:     %{pname}.logdir.patch
# 6/12/14 karl.w.schulz@intel.com - define default runlevel
Patch2:     %{pname}.initd.patch
# 11/10/14 karl.w.schulz@intel.com - enable systemd-based startup
Patch3:     %{pname}.service.patch
# 2019-03-11 janne.blomqvist@aalto.fi - Enable syslog
Patch4:     %{pname}.syslog.patch

%if 0%{?suse_version} >= 1230
Requires(pre):	shadow
%else
%if 0%{?suse_version}
Requires(pre):	pwdutils
%else
Requires(pre):	shadow-utils
%endif
%endif

%package -n %{pname}-devel%{PROJ_DELIM}
Summary:	Headers and libraries for developing applications using MUNGE
Group:		Development/Libraries
Requires:	%{pname}-libs%{PROJ_DELIM} = %{version}-%{release}
%if 0%{?suse_version}
BuildRequires:	pkg-config
%else
BuildRequires:	pkgconfig
%endif
Conflicts: %{pname}-devel

%package -n %{pname}-libs%{PROJ_DELIM}
Summary:	Libraries for applications using MUNGE
Group:		System Environment/Libraries
Requires:	%{pname}%{PROJ_DELIM} = %{version}-%{release}
Conflicts: %{pname}-libs

%description
MUNGE (MUNGE Uid 'N' Gid Emporium) is an authentication service for creating
and validating credentials.  It is designed to be highly scalable for use
in an HPC cluster environment.  It allows a process to authenticate the
UID and GID of another local or remote process within a group of hosts
having common users and groups.  These hosts form a security realm that is
defined by a shared cryptographic key.  Clients within this security realm
can create and validate credentials without the use of root privileges,
reserved ports, or platform-specific methods.

%description -n %{pname}-devel%{PROJ_DELIM}
A header file and static library for developing applications using MUNGE.

%description -n %{pname}-libs%{PROJ_DELIM}
A shared library for applications using MUNGE.

%prep
%setup -n %{pname}-%{pname}-%{version}

# OpenHPC patches
%patch1
%patch2
%patch3
%patch4

%build
##
# Add the following to the rpm command line to specify 32-bit/64-bit builds:
#   --with arch32               (build 32-bit executables & library)
#   --with arch64               (build 64-bit executables & library)
##
%configure \
  %{?_with_arch32: --enable-arch=32} \
  %{?_with_arch64: --enable-arch=64} \
  --program-prefix=%{?_program_prefix:%{_program_prefix}}
make %{?_smp_mflags}

%install
DESTDIR="$RPM_BUILD_ROOT" make install
touch "$RPM_BUILD_ROOT"/%{_sysconfdir}/munge/munge.key

touch "$RPM_BUILD_ROOT"/%{_localstatedir}/lib/munge/munge.seed
touch "$RPM_BUILD_ROOT"/%{_localstatedir}/log/munge/munged.log
touch "$RPM_BUILD_ROOT"/%{_localstatedir}/run/munge/munged.pid

# karl.w.schulz@intel.com (11/10/14) - remove chkconfig service for newer distros
%if 0%{?suse_version} >= 1230
rm "$RPM_BUILD_ROOT"/etc/init.d/munge
%endif
%if 0%{?rhel_version} > 600 || 0%{?centos_version} > 600 || 0%{?rhel}
rm "$RPM_BUILD_ROOT"/etc/rc.d/init.d/munge
%endif

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%pre
# karl.w.schulz@intel.com (9/10/18) - provide specific uid/gid to deal with 
# possibility of getting alternate ownership within Warewulf
/usr/bin/getent group munge >/dev/null 2>&1 || \
  /usr/sbin/groupadd -r munge -o -g 201
/usr/bin/getent passwd munge >/dev/null 2>&1 || \
  /usr/sbin/useradd -c "MUNGE authentication service" \
  -d "%{_sysconfdir}/munge" -g munge -s /bin/false -o -r munge -u 201

%post
if [ ! -e %{_sysconfdir}/munge/munge.key -a -c /dev/urandom ]; then
  /bin/dd if=/dev/urandom bs=1 count=1024 \
    >%{_sysconfdir}/munge/munge.key 2>/dev/null
  /bin/chown munge:munge %{_sysconfdir}/munge/munge.key
  /bin/chmod 0400 %{_sysconfdir}/munge/munge.key
fi
##
# Fix files for munge user when upgrading to 0.5.11.
if ! /bin/egrep '^[ 	]*USER=' %{_sysconfdir}/sysconfig/munge \
    >/dev/null 2>&1; then
  /bin/chown munge:munge %{_sysconfdir}/munge/* %{_localstatedir}/*/munge/* \
    %{_localstatedir}/run/munge >/dev/null 2>&1
fi
##
# Fix subsys lockfile name when upgrading to 0.5.11.
if [ -f /var/lock/subsys/munged ]; then
  /bin/mv /var/lock/subsys/munged /var/lock/subsys/munge
fi
##
%if 0%{?suse_version} >= 1230 || 0%{?rhel_version} > 600 || 0%{?centos_version} > 600 || 0%{?rhel}
   /bin/systemctl enable munge.service >/dev/null 2>&1 || :
%else
   if [ -x /sbin/chkconfig ]; then /sbin/chkconfig --add munge; fi
%endif

%post -n %{pname}-libs%{PROJ_DELIM}
/sbin/ldconfig %{_libdir}

%preun
if [ $1 -eq 0 ]; then
   %if 0%{?suse_version} >= 1230 || 0%{?rhel_version} > 600 || 0%{?centos_version} > 600 || 0%{?rhel}
   /bin/systemctl disable munge.service >/dev/null 2>&1 || :
   /bin/systemctl stop munge.service >/dev/null 2>&1 || :
   %else
     %{_sysconfdir}/init.d/munge stop >/dev/null 2>&1 || :
     if [ -x /sbin/chkconfig ]; then /sbin/chkconfig --del munge; fi
   %endif  
fi

%postun
if [ $1 -ge 1 ]; then
   %if 0%{?suse_version} >= 1230 || 0%{?rhel_version} > 600 || 0%{?centos_version} > 600 || 0%{?rhel}
      service munge condrestart  >/dev/null 2>&1 || :
   %else
      %{_sysconfdir}/init.d/munge try-restart >/dev/null 2>&1 || :
   %endif
fi

%postun -n %{pname}-libs%{PROJ_DELIM}
/sbin/ldconfig %{_libdir}

%files
%doc AUTHORS
%doc COPYING
%doc DISCLAIMER*
%doc HISTORY
%doc INSTALL
%doc JARGON
%doc NEWS
%doc PLATFORMS
%doc QUICKSTART
%doc README*
%doc TODO
%doc doc/*
%dir %attr(0700,munge,munge) %{_sysconfdir}/munge
%attr(0600,munge,munge) %config(noreplace) %ghost %{_sysconfdir}/munge/munge.key
%config(noreplace) %{_sysconfdir}/sysconfig/munge

# OpenHPC mods - systemd 
%if 0%{?suse_version} >= 1230 || 0%{?rhel_version} > 600 || 0%{?centos_version} > 600 || 0%{?rhel}
%{_prefix}/lib/systemd/system/munge.service
%else
%{?_initddir:%{_initddir}}%{!?_initddir:%{_initrddir}}/munge
%endif

%dir %attr(0711,munge,munge) %{_localstatedir}/lib/munge
%attr(0600,munge,munge) %ghost %{_localstatedir}/lib/munge/munge.seed
%dir %attr(0700,munge,munge) %{_localstatedir}/log/munge
%attr(0640,munge,munge) %ghost %{_localstatedir}/log/munge/munged.log
%dir %attr(0755,munge,munge) %ghost %{_localstatedir}/run/munge
%attr(0644,munge,munge) %ghost %{_localstatedir}/run/munge/munged.pid
%{_bindir}/*
%{_sbindir}/*
%{_mandir}/*[^3]/*


%if 0%{?suse_version} >= 1230 || 0%{?rhel_version} > 600 || 0%{?centos_version} > 600 || 0%{?rhel}
%{_prefix}/lib/tmpfiles.d/munge.conf
%endif

%files -n %{pname}-devel%{PROJ_DELIM}
%{_includedir}/*
%{_libdir}/*.la
%{_libdir}/pkgconfig/*.pc
%{_mandir}/*3/*
%{_libdir}/*.a
%{_libdir}/*.so

%files -n %{pname}-libs%{PROJ_DELIM}
%{_libdir}/*.so.*

