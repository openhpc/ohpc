#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%{!?_rel:%{expand:%%global _rel 0.r%(test "1547" != "0000" && echo "1547" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || echo 0000)}}

%include %{_sourcedir}/FSP_macros

%define pname warewulf-cluster
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Name:    %{pname}%{PROJ_DELIM}
Summary: Tools used for clustering with Warewulf
Version: 3.6
Release: %{_rel}
License: US Dept. of Energy (BSD-like)
Group:   fsp/provisioning
URL:     http://warewulf.lbl.gov/
Source0: %{pname}-%{version}.tar.gz
Source1: FSP_macros
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM} warewulf-provision%{PROJ_DELIM} ntp
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root
#%if 0%{?rhel_version} < 700 || 0%{?centos_version} < 700
#%if ! 0%{?suse_version}
#BuildRequires: libdb4-utils
#%endif
#%endif

%define debug_package %{nil}

# 06/13/14 charles.r.baird@intel.com - wwinit patch for SLES
Patch1: warewulf-cluster.wwinit.patch
# 06/14/14 karl.w.schulz@intel.com - FSP flag used to disable inclusion of node package
%define fsp_disable 1
# 07/21/14 karl.w.schulz@intel.com - excplictly document libcom32 and libutil as being provided
provides: libcom32.c32
provides: libutil.c32

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This package contains tools to facilitate management of a Cluster
with Warewulf.

# 06/14/14 karl.w.schulz@intel.com - disable warewulf-cluster-node package
%if %{fsp_disable}
%package -n %{pname}-node%{PROJ_DELIM}
Summary: Tools used for clustering with Warewulf
Group: System Environment/Clustering
Requires: /sbin/sfdisk

%description -n %{pname}-node%{PROJ_DELIM}
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the cluster-node module, that is installed onto the
provisioned nodes.
%endif

%prep
%setup -n %{pname}-%{version}

%patch1 -p1


%build
%configure
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}
cp -r $RPM_BUILD_ROOT/etc/warewulf/vnfs/include/* $RPM_BUILD_ROOT
rm -rf $RPM_BUILD_ROOT/etc/warewulf/vnfs
rmdir $RPM_BUILD_ROOT/etc/warewulf >/dev/null 2>&1 || :

%if 0%{?suse_version}
mv $RPM_BUILD_ROOT/etc/rc.d/init.d  $RPM_BUILD_ROOT/etc/init.d
%endif

# 06/14/14 karl.w.schulz@intel.com - disable warewulf-cluster-node package
%if !%{fsp_disable}
rm -rf $RPM_BUILD_ROOT/etc/sysconfig/wwfirstboot.conf
rm -rf $RPM_BUILD_ROOT/etc/rc.d/init.d/wwfirstboot
rm -rf $RPM_BUILD_ROOT/%{_libexecdir}/warewulf/wwfirstboot/*
%endif


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
%doc AUTHORS COPYING ChangeLog INSTALL LICENSE NEWS README README.node TODO
%{_sysconfdir}/profile.d/*
%{_bindir}/*
%{_libexecdir}/warewulf/wwinit/*
%{perl_vendorlib}/Warewulf/Module/Cli/*

# 06/14/14 karl.w.schulz@intel.com - disable warewulf-cluster-node package
%if %{fsp_disable}

%files -n %{pname}-node%{PROJ_DELIM}
%defattr(-, root, root)
%doc AUTHORS COPYING LICENSE README.node
%config(noreplace) %{_sysconfdir}/sysconfig/wwfirstboot.conf
%if 0%{?suse_version}
%{_sysconfdir}/init.d/wwfirstboot
%else
%{_sysconfdir}/rc.d/init.d/wwfirstboot
%endif
%defattr(0755, root, root)
%{_libexecdir}/warewulf/wwfirstboot/*

%if 0%{?sles_version} || 0%{?suse_version}
%dir %{_libexecdir}/warewulf/
%endif

%post -n %{pname}-node%{PROJ_DELIM}
chkconfig --add wwfirstboot

%endif


%changelog
