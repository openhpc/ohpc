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

%define dname common
%define pname warewulf-%{dname}
%define wwsrvdir /srv
%define develSHA c6de604fc76eabfaef2cb99f4c6ae5ed44eff1e0
%define wwextract warewulf3-%{develSHA}

Name:    %{pname}%{PROJ_DELIM}
Version: 3.10.0
Release: 1%{?dist}
Summary: Scalable systems management suite for high performance clusters
License: US Dept. of Energy (BSD-like)
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/%{develSHA}.tar.gz
Patch0:  warewulf-common.mysql_r1978.patch
Patch1:  warewulf-common.rhel_service.patch
Patch2:  warewulf-common.openEuler_22.03.patch
Group:   %{PROJ_NAME}/provisioning
ExclusiveOS: linux
Conflicts: warewulf < 3
BuildArch: noarch
BuildRequires: autoconf, automake, make
Requires: perl(DBD::mysql), perl(DBD::Pg), perl(DBD::SQLite), perl(JSON::PP)

%if 0%{?rhel:1} || 0%{?sle_version} > 150000
%global sql_name mariadb
%global daemon_name mariadb
%else
%global sql_name mysql
%global daemon_name mysqld
%endif

%if 0%{?rhel} >= 8 || 0%{?openEuler}
BuildRequires: perl-generators
BuildRequires: systemd
Requires: perl-Sys-Syslog
%endif
%if 0%{?suse_version:1}
BuildRequires: systemd-rpm-macros
Requires: perl-Unix-Syslog
%endif

Requires: %sql_name

%description
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

Warewulf Common contains the core functionality of Warewulf. It provides
the base libraries that are shared and utilized by the other Warewulf
modules as well as the backend data store interface, event and trigger
handlers and a basic command line interface.


%prep
cd %{_builddir}
%{__rm} -rf %{name}-%{version} %{wwextract}
%{__ln_s} %{wwextract}/%{dname} %{name}-%{version}
%setup -q -D
%patch0 -p1
%patch1 -p1
%patch2 -p1


%build
./autogen.sh
WAREWULF_STATEDIR=%{wwsrvdir}
%configure --sharedstatedir=%{wwsrvdir}
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%pre
/usr/sbin/groupadd -r warewulf >/dev/null 2>&1 || :


%post
# Canonicalize on upgrade, before old package is removed
if [ $1 -gt 1 ] ; then
    %{_bindir}/wwsh object canonicalize -t node >/dev/null 2>&1 || :
    %{_bindir}/wwsh object canonicalize -t file >/dev/null 2>&1 || :
fi


%files
%defattr(-, root, root)
%doc AUTHORS ChangeLog INSTALL NEWS README TODO COPYING LICENSE
%attr(0755, root, warewulf) %dir %{_sysconfdir}/warewulf/
%attr(0755, root, warewulf) %dir %{_sysconfdir}/warewulf/defaults/
%attr(0444, root, warewulf) %{_libexecdir}/warewulf/wwinit/functions
%attr(0644, root, root) %{_sysconfdir}/bash_completion.d/warewulf_completion
%attr(0644, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/database.conf
%attr(0640, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/database-root.conf
%attr(0644, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/defaults/node.conf
%{_mandir}/*
%{_bindir}/*
%{_datadir}/warewulf/
%{_libexecdir}/warewulf/wwinit
%{perl_vendorlib}/*

# 06/14/14 karl.w.schulz@intel.com - include required dir for SUSE
%if 0%{?suse_version}
%dir %{_libexecdir}/warewulf/
%endif


# ====================
%package localdb
Summary: Warewulf - Install local database server
Requires: %{sql_name}-server

%description localdb
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

This metapackage installs a local MySQL or MariaDB instance. It is not
required for installs that will use an external database server.

By default, removing this package will not remove or disable the installed
SQL server.

%post localdb
# Start services on install.
# For upgrades or removal, restart after the old package is removed.
if [ $1 -eq 1 ] ; then
%if 0%{?sle_version:1} || 0%{?rhel} >= 8 || 0%{?openEuler}
%systemd_post %{daemon_name}.service
%else
/usr/bin/systemctl --no-reload preset %{daemon_name}.service  &> /dev/null || :
%endif

/usr/bin/systemctl reenable %{daemon_name}.service &> /dev/null || :
/usr/bin/systemctl restart %{daemon_name}.service  &> /dev/null || :
fi

%postun localdb
%if 0%{?sle_version:1} || 0%{?rhel} >= 8 || 0%{?openEuler}
%systemd_postun_with_restart %{daemon_name}.service
%else
/usr/bin/systemctl try-restart %{daemon_name}.service  &> /dev/null || :
%endif

%files localdb
%defattr(-, root, root)


# ====================
