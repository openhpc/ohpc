%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary: Open Resiliency Cluster Management implementation
Name: orcm
Version: 0.5.0
Release: 1
License: See COPYING
Group: System Environment/Libraries
Vendor: Intel Corporation
URL: https://bitbucket.org/rhc/orcm/
Prefix: %{_prefix}
Prefix: %{_sysconfdir}
#Source0: %{name}-%{version}.tar.gz
Source0:  orcm-0.5+git+1412090826+3594823.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: flex >= 2.5.35
%if 0%{?rhel_version} > 600
BuildRequires: libtool-ltdl-devel
%endif

# Disable dependencies for non-OBS builds since users need to be able to rebuild
# using the source RPM and may not want to include some or all of these dependencies
%if 0%{?FSP_BUILD}
BuildRequires: autoconf >= 2.69
BuildRequires: automake >= 1.12.2
BuildRequires: libtool >= 2.4.2
%if 0%{?suse_version} || 0%{?rhel_version} || 0%{?centos_version} > 600
BuildRequires: libsigar%{PROJ_DELIM}
BuildRequires: libsigar-devel%{PROJ_DELIM}
%else
BuildRequires: sigar%{PROJ_DELIM}
BuildRequires: sigar-devel%{PROJ_DELIM}
%endif

BuildRequires: unixODBC
BuildRequires: unixODBC-devel
BuildRequires: ipmiutil >= 2.9.3
BuildRequires: ipmiutil-devel >= 2.9.3
%if 0%{?sles_version}
BuildRequires: libopenssl-devel
%else
BuildRequires: openssl-devel
%endif

Requires:      ipmiutil
Requires:      unixODBC
%endif

%{!?configure_flags: %define configure_flags ""}

%description
orcm is an opensource resiliency cluster management software implementation.

%prep
%setup -q -c -T -a 0 -n orcm-0.5+git+1412090826+3594823

%build
#pushd %{name}-%{version}
pushd orcm-0.5+git+1412090826+3594823
./autogen.pl
mkdir -p obj
pushd obj
../configure %{configure_flags} --prefix=%{_prefix} --sysconfdir=%{_sysconfdir} --libdir=%{_libdir} --datadir=%{_datadir} --bindir=%{_bindir} \
--with-platform=../contrib/platform/intel/hillsboro/orcm-linux
make %{?_smp_mflags}
popd
popd

%install
#pushd %{name}-%{version}
pushd orcm-0.5+git+1412090826+3594823
pushd obj
make install DESTDIR=%{buildroot}
%if 0%{?suse_version} <= 1200
rm -rf %{buildroot}%{_libdir}/pkgconfig
%endif
popd
popd

%clean

%files
%defattr(-,root,root,-)
%config %{_sysconfdir}/openmpi-default-hostfile
%config %{_sysconfdir}/openmpi-mca-params.conf
%config %{_sysconfdir}/orcm-site.xml
%{_bindir}/*
%doc %{_datadir}/openmpi
%doc %{_mandir}/man1/*
%doc %{_mandir}/man7/*


%package devel
Summary:       Development libraries for ORCM
Group:         Development/Libraries
BuildRequires: pkg-config

%description devel
An open source resiliency cluster management software implementation.

%files devel
%defattr(-,root,root,-)
%{_includedir}/openmpi
%dir %{_libdir}/openmpi
%{_libdir}/*.so
%{_libdir}/openmpi/*.so
%{_libdir}/*.la
%{_libdir}/openmpi/*.la
%if 0%{?suse_version} > 1220
%{_libdir}/pkgconfig/*.pc
%endif

%post -n %{name}-devel -p /sbin/ldconfig

%postun -n %{name}-devel -p /sbin/ldconfig


%package -n liborcm
Summary:       Dynamic libraries for ORCM
BuildRequires: pkg-config
Group:         System Environment/Libraries

%description -n liborcm
An open source resiliency cluster management software implementation.

%files -n liborcm
%defattr(-,root,root,-)
%{_libdir}/*.so.*

%post -n liborcm -p /sbin/ldconfig

%postun -n liborcm -p /sbin/ldconfig


%changelog
