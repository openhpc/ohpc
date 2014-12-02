%{!?_rel:%{expand:%%global _rel 0.r%(test "1686" != "0000" && echo "1686" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}
%define debug_package %{nil}
%define wwpkgdir /srv/warewulf

Name: warewulf-provision
Summary: Warewulf - Provisioning Module
Version: 3.6
Release: %{_rel}%{?dist}
#Release: 1.%{?_dist}
License: US Dept. of Energy (BSD-like)
Group: System Environment/Clustering
Source: %{name}-%{version}.tar.gz
ExclusiveOS: linux
Requires: warewulf-common
BuildRequires: warewulf-common
BuildRequires: libselinux-devel
Conflicts: warewulf < 3
BuildConflicts: post-build-checks
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{name}-%{version}-%{release}-root
Patch1: %{name}.busybox.patch.bz2
Patch2: %{name}.httpdconfdir.patch

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.  The
provision module provides functionality for provisioning, configuring,
and booting systems.

This package contains the core provisioning components and
administrative tools.  To actually provision systems, the
%{name}-server package is also required.


%package server
Summary: Warewulf - Provisioning Module - Server
Group: System Environment/Clustering
Requires: %{name} = %{version}-%{release}

# 07/22/14 karl.w.schulz@intel.com - differentiate requirements per Base OS
%if 0%{?sles_version} || 0%{?suse_version}
Requires: apache2 apache2-mod_perl tftp dhcp-server
%else
Requires: mod_perl httpd tftp-server dhcp
%endif

# charles.r.baird@intel.com - required to determine where to stick warewulf-httpd.conf
%if 0%{?sles_version} || 0%{?suse_version} == 1315
BuildRequires: sles-release
%endif

%description server
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.  The
provision module provides functionality for provisioning, configuring,
and booting systems.

This package contains the CGI scripts and event components to actually
provision systems.  Systems used solely for administration of Warewulf
do not require this package.


%package gpl_sources
Summary: This package contains the GPL sources used in Warewulf
Group: Development/System
Requires: %{name} = %{version}-%{release}

%description gpl_sources
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.  The
provision module provides functionality for provisioning, configuring,
and booting systems.

Warewulf is distributed with some third-party software as a
convenience to our users.  While Warewulf itself is licensed under a
DOE license (a derivative of the BSD license), the third-party
software may have different licensing terms, including the GNU General
Public License (GPL).

In order to be 100% compatible with the GPL this package makes
available the included GPL software.


%prep
%setup -q
%patch1 -p1
%patch2 -p1


%build
%configure --localstatedir=%{wwpkgdir}
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%post server
# 07/22/14 karl.w.schulz@intel.com - specify alternate group per Base OS
%if 0%{?sles_version} || 0%{?suse_version}
usermod -a -G warewulf www >/dev/null 2>&1 || :
%else
usermod -a -G warewulf apache >/dev/null 2>&1 || :
%endif
service httpd restart >/dev/null 2>&1 || :
chkconfig httpd on >/dev/null 2>&1 || :
chkconfig tftp on >/dev/null 2>&1 || :
chkconfig xinetd on >/dev/null 2>&1 || :
service xinetd restart >/dev/null 2>&1 || :


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-, root, root)
%doc AUTHORS COPYING ChangeLog INSTALL NEWS README TODO LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/provision.conf
%config(noreplace) %{_sysconfdir}/warewulf/livesync.conf
%config(noreplace) %{_sysconfdir}/warewulf/defaults/provision.conf
%{_bindir}/*
%{wwpkgdir}/*
%{_datadir}/warewulf/*
%{perl_vendorlib}/Warewulf/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Provision.pm
%{perl_vendorlib}/Warewulf/Vnfs.pm
%{perl_vendorlib}/Warewulf/DSO/*
%{perl_vendorlib}/Warewulf/Provision
%{perl_vendorlib}/Warewulf/Event/DynamicHosts.pm
%{perl_vendorlib}/Warewulf/Event/DefaultProvisionNode.pm
%{perl_vendorlib}/Warewulf/Event/ProvisionFileDelete.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Provision.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Vnfs.pm

%files server
%defattr(-, root, root)
%config(noreplace) %{_sysconfdir}/warewulf/dhcpd-template.conf
%if 0%{?sles_version} || 0%{?suse_version}
%config(noreplace) %{_sysconfdir}/apache2/conf.d/warewulf-httpd.conf
%else
%config(noreplace) %{_sysconfdir}/httpd/conf.d/warewulf-httpd.conf
%endif

# 07/22/14 karl.w.schulz@intel.com - specify alternate group per Base OS
%if 0%{?sles_version} || 0%{?suse_version}
%attr(0750, root, www) %{_libexecdir}/warewulf/cgi-bin/
%else
%attr(0750, root, apache) %{_libexecdir}/warewulf/cgi-bin/
%endif

%{perl_vendorlib}/Warewulf/Event/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Event/Dhcp.pm
%{perl_vendorlib}/Warewulf/Event/Pxelinux.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Pxe.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Dhcp.pm

%files gpl_sources
%defattr(-, root, root)
%{_prefix}/src/warewulf/3rd_party/GPL/


%changelog
