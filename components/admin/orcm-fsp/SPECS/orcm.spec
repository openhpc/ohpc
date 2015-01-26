%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}
%define pname orcm

Summary: Open Resiliency Cluster Management implementation
Name:    %{pname}%{PROJ_DELIM}
Version: 0.5.0
Release: 1
License: See COPYING
Group:   System Environment/Libraries
Vendor:  Intel Corporation
URL:     https://github.com/open-mpi/orcm
Prefix:  %{_prefix}
Prefix:  %{_sysconfdir}
Source0: openrcm-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
BuildRequires: flex >= 2.5.35
%if 0%{?rhel_version} > 600
BuildRequires: libtool-ltdl-devel
%endif
BuildRequires:  pkgconfig(systemd)
%{?systemd_requires}

# 01/20/2015 karl.w.schulz@intel.com - include systemd files from newer orcm
Source1: orcmd.service
Source2: orcmd.sysconfig
Patch1:  bmc.patch


# Disable dependencies for non-OBS builds since users need to be able to rebuild
# using the source RPM and may not want to include some or all of these dependencies
%if 0%{?FSP_BUILD}
BuildRequires: autoconf >= 2.69
BuildRequires: automake >= 1.12.2
BuildRequires: libtool >= 2.4.2
BuildRequires: sigar%{PROJ_DELIM}
BuildRequires: sigar-devel%{PROJ_DELIM}

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
%setup -q -n open-rcm-%{version}

%patch1 -p0

%build
./autogen.pl

./configure %{configure_flags}           \
             --prefix=%{_prefix}         \
             --sysconfdir=%{_sysconfdir} \
             --libdir=%{_libdir}         \
             --datadir=%{_datadir}       \
             --bindir=%{_bindir}         \
             --with-platform=./contrib/platform/intel/hillsboro/orcm-linux

make %{?_smp_mflags}

%install
make install DESTDIR=%{buildroot}
%if 0%{?suse_version} <= 1200
rm -rf %{buildroot}%{_libdir}/pkgconfig
%endif

# 01/20/2015 karl.w.schulz@intel.com - include systemd files from newer orcm

install -D -m 0644 %SOURCE1 %{buildroot}%{_unitdir}/orcmd.service
install -D -m 0644 %SOURCE2 %{buildroot}/etc/sysconfig/orcmd

%clean

%files
%defattr(-,root,root,-)
%config %{_sysconfdir}/openmpi-default-hostfile
%config %{_sysconfdir}/openmpi-mca-params.conf
%config %{_sysconfdir}/orcm-site.xml
%config /etc/sysconfig/*
%{_bindir}/*
%doc %{_datadir}/openmpi
%doc %{_mandir}/man1/*
%doc %{_mandir}/man7/*
%{_unitdir}/*

%pre 
%service_add_pre orcmd.service

%post
%service_add_post orcmd.service

%preun
%service_del_preun orcmd.service

%postun
%service_del_postun orcmd.service


%package -n %{pname}-devel%{PROJ_DELIM}
Summary:       Development libraries for ORCM
Group:         Development/Libraries
BuildRequires: pkg-config

%description -n %{pname}-devel%{PROJ_DELIM}
An open source resiliency cluster management software implementation.

%files -n %{pname}-devel%{PROJ_DELIM}
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

%post -n %{pname}-devel%{PROJ_DELIM} -p /sbin/ldconfig

%postun -n %{pname}-devel%{PROJ_DELIM} -p /sbin/ldconfig


%package -n liborcm%{PROJ_DELIM}
Summary:       Dynamic libraries for ORCM
BuildRequires: pkg-config
Group:         System Environment/Libraries

%description -n liborcm%{PROJ_DELIM}
An open source resiliency cluster management software implementation.

%files -n liborcm%{PROJ_DELIM}
%defattr(-,root,root,-)
%{_libdir}/*.so.*

%post -n liborcm%{PROJ_DELIM} -p /sbin/ldconfig

%postun -n liborcm%{PROJ_DELIM} -p /sbin/ldconfig


%changelog
