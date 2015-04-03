%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}
%define pname orcm

Summary: Open Resiliency Cluster Management implementation
Name:    %{pname}%{PROJ_DELIM}
Version: 0.7.0
Release: 1
License: See COPYING
Group:   fsp/admin
Vendor:  Intel Corporation
URL:     https://github.com/open-mpi/orcm
#Prefix:  /opt/open-rcm
#Prefix:  %{_sysconfdir}
Source0: orcm-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
BuildRequires: flex >= 2.5.35
%if 0%{?rhel_version} > 600 || 0%{?centos_version} > 600
BuildRequires: libtool-ltdl-devel
%endif
BuildRequires:  pkgconfig(systemd)
%{?systemd_requires}

# Define prefix
%define install_prefix /opt/open-rcm

# 01/20/2015 karl.w.schulz@intel.com - include systemd files from newer orcm
Source1: orcmd.service
Source2: orcmd.sysconfig
# 01/30/2015 karl.w.schulz@intel.com - include db files from newer orcm
Source3: psql_odbc_driver.ini
Source4: orcmdb_psql.ini
Source5: orcmdb_psql.sql
# 01/30/2015 karl.w.schulz@intel.com - use vanilla passwords, relax sensor frequency and alter aggregator hostname
Patch1:  bmc.patch
Patch2:  site-xml.patch
# 02/03/2015 karl.w.schulz@intel.com - include updated postgres config files and db-oriented orcmd.sysconfig
Source6: pg_hba.conf
Source7: postgresql.conf
Source8: orcmd.db.sysconfig

# Disable dependencies for non-OBS builds since users need to be able to rebuild
# using the source RPM and may not want to include some or all of these dependencies
%if 0%{?FSP_BUILD}
BuildRequires: autoconf >= 2.69
BuildRequires: automake >= 1.12.2
BuildRequires: libtool >= 2.4.2
BuildRequires: sigar%{PROJ_DELIM}
BuildRequires: sigar-devel%{PROJ_DELIM}
BuildRequires: psqlODBC%{PROJ_DELIM}

BuildRequires: unixODBC
BuildRequires: unixODBC-devel
BuildRequires: ipmiutil >= 2.9.5
BuildRequires: ipmiutil-devel >= 2.9.5
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
%setup -q -n orcm-%{version}

%patch1 -p0
%patch2 -p0

%build

# 03/30/15 karl.w.schulz@intel.pl - allow use fsp provided autotools
#export PATH=/opt/fsp/pub/autotools/bin/:$PATH

./autogen.pl

### ./configure %{configure_flags}           \
###              --prefix=%{_prefix}         \
###              --sysconfdir=%{_sysconfdir} \
###              --libdir=%{_libdir}         \
###              --datadir=%{_datadir}       \
###              --bindir=%{_bindir}         \
###              --with-platform=./contrib/platform/intel/hillsboro/orcm-linux
### 
./configure --prefix=%{install_prefix} \
             --with-platform=./contrib/platform/intel/hillsboro/orcm-linux

make %{?_smp_mflags}

%install
make install DESTDIR=%{buildroot}
%if 0%{?suse_version} <= 1200
rm -rf %{buildroot}%{_libdir}/pkgconfig
%endif

install -D -m 0644 contrib/dist/linux/orcmd.service %{buildroot}%{_unitdir}/orcmd.service
install -D -m 0644 contrib/dist/linux/orcmsched.service %{buildroot}%{_unitdir}/orcmsched.service

install -D -m 0644 contrib/dist/linux/orcmd.sysconfig %{buildroot}/etc/sysconfig/orcmd
install -D -m 0644 contrib/dist/linux/orcmsched.sysconfig %{buildroot}/etc/sysconfig/orcmsched
install -D -m 0644 contrib/database/orcmdb_psql.ini %{buildroot}%{_sysconfdir}/orcmdb_psql.ini
install -D -m 0644 contrib/database/orcmdb_psql.sql %{buildroot}%{_sysconfdir}/orcmdb_psql.sql
install -D -m 0644 contrib/database/psql_odbc_driver.ini %{buildroot}%{_sysconfdir}/psql_odbc_driver.ini

### # 01/20/2015 karl.w.schulz@intel.com - include systemd files from newer orcm

### install -D -m 0644 %SOURCE1 %{buildroot}%{_unitdir}/orcmd.service
### install -D -m 0644 %SOURCE2 %{buildroot}/etc/sysconfig/orcmd
### 
### # 01/30/2015 karl.w.schulz@intel.com - include db ini files from newer orcm
### 
### install -D -m 0644 %SOURCE3 %{buildroot}%{_sysconfdir}/psql_odbc_driver.ini
### install -D -m 0644 %SOURCE4 %{buildroot}%{_sysconfdir}/orcmdb_psql.ini
### install -D -m 0644 %SOURCE5 %{buildroot}%{_sysconfdir}/orcmdb_psql.sql
### 
# 02/03/2015 karl.w.schulz@intel.com - include copy of updated postgres config files

install -D -m 0644 %SOURCE6 %{buildroot}%{_sysconfdir}/pg_hba.orcm.conf
install -D -m 0644 %SOURCE7 %{buildroot}%{_sysconfdir}/postgresql.orcm.conf
install -D -m 0644 %SOURCE8 %{buildroot}/etc/sysconfig/orcmd.db

%clean

%files
%defattr(-,root,root,-)
%config %{_sysconfdir}/*
%config /etc/sysconfig/*
#%{_bindir}/*
%{install_prefix}/*
#%doc %{_datadir}/openmpi
%doc %{install_prefix}/share/man/man1/*
%doc %{install_prefix}/share/man/man7/*
#%doc %{_mandir}/man1/*
#%doc %{_mandir}/man7/*
%{_unitdir}/*

%pre 
%service_add_pre orcmd.service

%post
%service_add_post orcmd.service

%preun
%service_del_preun orcmd.service

%postun
%service_del_postun orcmd.service


<<<<<<< HEAD
### %package -n %{pname}-devel%{PROJ_DELIM}
### Summary:       Development libraries for ORCM
### Group:         Development/Libraries
### BuildRequires: pkg-config
### 
### %description -n %{pname}-devel%{PROJ_DELIM}
### An open source resiliency cluster management software implementation.
### 
### %files -n %{pname}-devel%{PROJ_DELIM}
### %defattr(-,root,root,-)
### ##%{_includedir}/openmpi
### ##%dir %{_libdir}/openmpi
### %{_libdir}/*.so
### ##%{_libdir}/openmpi/*.so
### %{_libdir}/*.la
### ##%{_libdir}/openmpi/*.la
### %if 0%{?suse_version} > 1220
### %{_libdir}/pkgconfig/*.pc
### %endif
=======
%package -n %{pname}-devel%{PROJ_DELIM}
Summary:       Development libraries for ORCM
Group:         Development/Libraries
BuildRequires: pkg-config

%description -n %{pname}-devel%{PROJ_DELIM}
An open source resiliency cluster management software implementation.

%files -n %{pname}-devel%{PROJ_DELIM}
%defattr(-,root,root,-)
#%{_includedir}/openmpi
#%dir %{_libdir}/openmpi
%{_libdir}/*.so
%{_libdir}/openmpi/*.so
%{_libdir}/*.la
%{_libdir}/openmpi/*.la
%if 0%{?suse_version} > 1220
%{_libdir}/pkgconfig/*.pc
%endif
>>>>>>> acc859ce8a02b281e8bc607476891b181db767f8

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
