%{!?_rel:%{expand:%%global _rel 0.r%(test "1686" != "0000" && echo "1686" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}

%define wwpkgdir /srv/

%include %{_sourcedir}/FSP_macros

%define pname warewulf-common
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Name:    %{pname}%{PROJ_DELIM}
Summary: A suite of tools for clustering
Version: 3.6
Release: %{_rel}%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   fsp/provisioning
Source0: %{pname}-%{version}.tar.gz
Source1: FSP_macros
ExclusiveOS: linux
Conflicts: warewulf <= 2.9
# 06/14/14 karl.w.schulz@intel.com - SUSE does not allow files in /usr/lib64 for noarch package
%if 0%{?sles_version} || 0%{?suse_version}
BuildArch: x86_64
%else
BuildArch: noarch
%endif
BuildRoot: %{?_tmppath}/%{pname}-%{version}-%{release}-root
# 09/10/14 charles.r.baird@intel.com - patch to add SuSE as a system type
Patch1: warewulf-common.system.patch
# 09/10/14 charles.r.baird@intel.com - patch to add mariadb as a datastore
Patch2: warewulf-common.mariadb.patch
# 05/23/14 charles.r.baird@intel.com - alternate package names for SuSE
%if 0%{?suse_version}
Requires: mysql perl-DBD-mysql
%else
# 07/23/14 travis.post@intel.com - alternate package names for RHEL7
%if 0%{?rhel_version} > 600 || 0%{?centos_version} > 600
Requires: mariadb-server perl-DBD-MySQL
%else
%if 0%{?rhel_version} < 700 || 0%{?centos_version} < 700
Requires: mysql-server perl-DBD-mysql
BuildRequires: db4-utils
%endif
%endif
%endif

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the main package which includes the main daemon and
supporting libs.


%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1
%patch2 -p1


%build
%configure --localstatedir=%{wwpkgdir}
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%pre
groupadd -r warewulf >/dev/null 2>&1 || :


%post
if [ $1 -eq 2 ] ; then
    %{_bindir}/wwsh object canonicalize >/dev/null 2>&1 || :
fi
service mysqld start >/dev/null 2>&1 || :
chkconfig mysqld on >/dev/null 2>&1 || :


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-, root, root)
%doc AUTHORS COPYING ChangeLog INSTALL NEWS README TODO LICENSE
%attr(0755, root, warewulf) %dir %{_sysconfdir}/warewulf/
%attr(0755, root, warewulf) %dir %{_sysconfdir}/warewulf/defaults/
%attr(0444, root, warewulf) %{_sysconfdir}/warewulf/functions
%attr(0644, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/database.conf
%attr(0640, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/database-root.conf
%attr(0644, root, warewulf) %config(noreplace) %{_sysconfdir}/warewulf/defaults/node.conf
%{_bindir}/*
%{_datadir}/warewulf/
%{_libexecdir}/warewulf/wwinit
%{perl_vendorlib}/*

# 06/14/14 karl.w.schulz@intel.com - include required dir for SUSE
%if 0%{?sles_version} || 0%{?suse_version}
%dir %{_libexecdir}/warewulf/
%endif

%changelog
