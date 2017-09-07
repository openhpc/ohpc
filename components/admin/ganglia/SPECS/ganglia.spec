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

%define pname ganglia

%global gangver     3.7.2
%global webver      3.7.2

%global systemd         1
%global _hardened_build 1

Name:               %{pname}%{PROJ_DELIM}
Version:            %{gangver}
Release:            1%{?dist}
Summary:            Distributed Monitoring System
Group:              %{PROJ_NAME}/admin
License:            BSD-3-Clause
URL:                http://ganglia.sourceforge.net/
DocDir:             %{OHPC_PUB}/doc/contrib
Source0:            http://downloads.sourceforge.net/ganglia/ganglia-%{version}.tar.gz
Source1:            http://downloads.sourceforge.net/ganglia/ganglia-web-%{webver}.tar.gz
Source2:            gmond.service
Source3:            gmetad.service
Source4:            ganglia-httpd24.conf.d
Source6:            conf.php
Source7:            OHPC_macros
Patch0:             ganglia-web-3.5.7-statedir.patch
#Patch1:             ganglia-3.7.1-py-syntax.patch
Patch2:             ganglia-no-private-apr.patch
Patch3:             ganglia-3.7.2-apache.patch
%if 0%{?systemd}
BuildRequires:      systemd
%endif
BuildRequires:      rrdtool-devel
BuildRequires:      libpng-devel
BuildRequires:      libart_lgpl-devel
BuildRequires:      libconfuse-devel
BuildRequires:      python-devel
BuildRequires:      freetype-devel
BuildRequires:      pcre-devel
BuildRequires:      perl
%if 0%{?sles_version} || 0%{?suse_version}
# define fdupes, clean up rpmlint errors
BuildRequires: fdupes
BuildRequires: libapr1-devel
BuildRequires:      libexpat-devel
# Can't find memcached built for SLES
#BuildRequires:      php5-memcached
%else
BuildRequires:      apr-devel >= 1
BuildRequires:      expat-devel
BuildRequires:      libmemcached-devel
%endif
%if 0%{?suse_version} >= 1210
BuildRequires: systemd-rpm-macros
%endif

#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks

%description
Ganglia is a scalable, real-time monitoring and execution environment
with all execution requests and statistics expressed in an open
well-defined XML format.

%package -n %{pname}-web%{PROJ_DELIM}
Summary:            Ganglia Web Frontend
Group:              Applications/Internet
Version:            %{webver}
Requires:           rrdtool
Requires:           php
Requires:           php-gd
Requires:           php-ZendFramework
Requires:           %{pname}-gmetad%{PROJ_DELIM} = %{gangver}-%{release}
%if 0%{?suse_version} >= 1210
Requires:           apache2-mod_php5
%endif

%description -n %{pname}-web%{PROJ_DELIM}
This package provides a web frontend to display the XML tree published by
ganglia, and to provide historical graphs of collected metrics. This website is
written in the PHP4 language.

%package -n %{pname}-gmetad%{PROJ_DELIM}
Summary:            Ganglia Metadata collection daemon
Group:              Applications/Internet
Requires:           %{name} = %{gangver}-%{release}
%if 0%{?systemd}
Requires(post):     systemd
Requires(preun):    systemd
Requires(postun):   systemd
%else
Requires(post):     /sbin/chkconfig
Requires(preun):    /sbin/chkconfig
Requires(preun):    /sbin/service
%endif #systemd

%description -n %{pname}-gmetad%{PROJ_DELIM}
Ganglia is a scalable, real-time monitoring and execution environment
with all execution requests and statistics expressed in an open
well-defined XML format.

This gmetad daemon aggregates monitoring data from several clusters
to form a monitoring grid. It also keeps metric history using rrdtool.

%package -n %{pname}-gmond%{PROJ_DELIM}
Summary:            Ganglia Monitoring daemon
Group:              Applications/Internet
Requires:           %{name} = %{gangver}-%{release}
%if 0%{?systemd}
Requires(post):     systemd
Requires(preun):    systemd
Requires(postun):   systemd
%else
Requires(post):     /sbin/chkconfig
Requires(preun):    /sbin/chkconfig
Requires(preun):    /sbin/service
%endif #systemd

%description -n %{pname}-gmond%{PROJ_DELIM}
Ganglia is a scalable, real-time monitoring and execution environment
with all execution requests and statistics expressed in an open
well-defined XML format.

This gmond daemon provides the ganglia service within a single cluster or
Multicast domain.

%package -n %{pname}-gmond-python%{PROJ_DELIM}
Summary:            Ganglia Monitor daemon python DSO and metric modules
Group:              Applications/Internet
Requires:           %{pname}-gmond%{PROJ_DELIM}
Requires:           python

%description -n %{pname}-gmond-python%{PROJ_DELIM}
Ganglia is a scalable, real-time monitoring and execution environment
with all execution requests and statistics expressed in an open
well-defined XML format.

This package provides the gmond python DSO and python gmond modules, which
can be loaded via the DSO at gmond daemon start time.

%package -n %{pname}-devel%{PROJ_DELIM}
Summary:            Ganglia Library
Group:              Applications/Internet
Requires:           %{name} = %{gangver}-%{release}

%description -n %{pname}-devel%{PROJ_DELIM}
The Ganglia Monitoring Core library provides a set of functions that
programmers can use to build scalable cluster or grid applications

%prep
%setup -q -n ganglia-%{gangver}
%patch2 -p1
# fix broken systemd support
install -m 0644 %{SOURCE2} gmond/gmond.service.in
install -m 0644 %{SOURCE3} gmetad/gmetad.service.in

%patch3 -p0

# web part
%setup -n ganglia-%{gangver} -q -T -D -a 1
#%patch1 -p1
mv ganglia-web-%{webver} web
cd web
%patch0 -p1

%build
%configure \
    --enable-setuid=ganglia \
    --enable-setgid=ganglia \
    --with-gmetad \
%if 0%{?rhel_version} > 600 || 0%{?centos_version} > 600
    --with-memcached \
%endif
    --disable-static \
    --enable-shared \
    --sysconfdir=%{_sysconfdir}/%{pname} \
    --localstatedir=%{_localstatedir}/lib/%{pname}

# Remove rpaths
%{__sed} -i 's|^hardcode_libdir_flag_spec=.*|hardcode_libdir_flag_spec=""|g' libtool
%{__sed} -i 's|^runpath_var=LD_RUN_PATH|runpath_var=DIE_RPATH_DIE|g' libtool

## Default to run as user ganglia instead of nobody
%{__perl} -pi.orig -e 's|nobody|ganglia|g' \
    gmond/gmond.conf.html ganglia.html gmond/conf.pod

%{__perl} -pi.orig -e 's|.*setuid_username.*|setuid_username ganglia|' \
    gmetad/gmetad.conf.in

## Don't have initscripts turn daemons on by default
%{__perl} -pi.orig -e 's|2345|-|g' gmond/gmond.init gmetad/gmetad.init

make %{?_smp_mflags}

%install
export NO_BRP_STALE_LINK_ERROR=yes
make install DESTDIR=$RPM_BUILD_ROOT

## Create directory structures
mkdir -p $RPM_BUILD_ROOT%{_libdir}/ganglia/python_modules
mkdir -p $RPM_BUILD_ROOT%{_localstatedir}/lib/%{pname}/{rrds,conf}
mkdir -p $RPM_BUILD_ROOT%{_localstatedir}/lib/%{pname}/dwoo/{cache,compiled}

## Install services
%if 0%{?systemd}
install -Dp -m 0644 %{SOURCE2} %{buildroot}%{_unitdir}/gmond.service
install -Dp -m 0644 %{SOURCE3} %{buildroot}%{_unitdir}/gmetad.service
%else
install -Dp -m 0755 gmond/gmond.init $RPM_BUILD_ROOT%{_sysconfdir}/init.d/gmond
install -Dp -m 0755 gmetad/gmetad.init $RPM_BUILD_ROOT%{_sysconfdir}/init.d/gmetad
%endif # systemd

## Build default gmond.conf from gmond using the '-t' flag
LD_LIBRARY_PATH=lib/.libs gmond/gmond -t | %{__perl} -pe 's|nobody|ganglia|g' \
    > $RPM_BUILD_ROOT%{_sysconfdir}/ganglia/gmond.conf

## Python bits
# Copy the python metric modules and .conf files
cp -p gmond/python_modules/conf.d/*.pyconf $RPM_BUILD_ROOT%{_sysconfdir}/ganglia/conf.d/
cp -p gmond/modules/conf.d/*.conf $RPM_BUILD_ROOT%{_sysconfdir}/ganglia/conf.d/
cp -p gmond/python_modules/*/*.py $RPM_BUILD_ROOT%{_libdir}/ganglia/python_modules/

## Web bits
mkdir -p $RPM_BUILD_ROOT/%{_datadir}/%{name}
cp -rp web/* $RPM_BUILD_ROOT%{_datadir}/%{name}/
install -p -m 0644 %{SOURCE6} %{buildroot}%{_sysconfdir}/ganglia/conf.php
ln -s ../../..%{_sysconfdir}/%{pname}/conf.php \
    $RPM_BUILD_ROOT%{_datadir}/%{name}/conf.php

%if 0%{?sles_version} || 0%{?suse_version}
install -Dp -m 0644 %{SOURCE4} %{buildroot}%{_sysconfdir}/apache2/conf.d/%{name}.conf
%else
install -Dp -m 0644 %{SOURCE4} %{buildroot}%{_sysconfdir}/httpd/conf.d/%{name}.conf
%endif

## Various clean up after install:

## Don't install the status modules and example.conf
rm -f $RPM_BUILD_ROOT%{_sysconfdir}/ganglia/conf.d/{modgstatus,example}.conf

## Disable the diskusage module until it is configured properly
## mv $RPM_BUILD_ROOT%{_sysconfdir}/ganglia/conf.d/diskusage.pyconf \
##   $RPM_BUILD_ROOT%{_sysconfdir}/ganglia/conf.d/diskusage.pyconf.off

## Remove unwanted files from web dir
rm -rf $RPM_BUILD_ROOT%{_datadir}/%{name}/{Makefile*,debian,ganglia-web.spec*,ganglia-web}
rm -rf $RPM_BUILD_ROOT%{_datadir}/%{name}/{conf_default.php.in,version.php.in}

## Included as doc
rm -rf $RPM_BUILD_ROOT%{_datadir}/%{name}/{README,TODO,AUTHORS,COPYING}

## House cleaning
rm -f $RPM_BUILD_ROOT%{_libdir}/*.la

## Use system php-ZendFramework
rm -rf $RPM_BUILD_ROOT/usr/share/%{name}/lib/Zend

# Remove execute bit
chmod 0644 $RPM_BUILD_ROOT%{_datadir}/%{name}/header.php
chmod 0644 $RPM_BUILD_ROOT%{_libdir}/%{pname}/python_modules/*.py
chmod 0644 $RPM_BUILD_ROOT%{_datadir}/%{name}/css/smoothness/jquery-ui-1.10.2.custom.css
chmod 0644 $RPM_BUILD_ROOT%{_datadir}/%{name}/css/smoothness/jquery-ui-1.10.2.custom.min.css

# Remove shebang
sed -i '1{\@^#!@d}' $RPM_BUILD_ROOT%{_libdir}/%{pname}/python_modules/*.py

%pre
## Add the "ganglia" user
/usr/sbin/useradd -U -c "Ganglia Monitoring System" \
        -s /sbin/nologin -r -d %{_localstatedir}/lib/%{pname} ganglia 2> /dev/null || :
#/sbin/ldconfig -- this is already in post and postun, no reason to run it before

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%if 0%{?systemd}
%post -n %{pname}-gmond%{PROJ_DELIM}
%if 0%{?sles_version} || 0%{?suse_version}
%service_add_post gmond.service
%else
%systemd_post gmond.service
%endif

%preun -n %{pname}-gmond%{PROJ_DELIM}
%if 0%{?sles_version} || 0%{?suse_version}
%service_del_preun gmond.service
%else
%systemd_preun gmond.service
%endif

%postun -n %{pname}-gmond%{PROJ_DELIM}
%if 0%{?sles_version} || 0%{?suse_version}
%service_del_postun gmond.service
%else
%systemd_postun_with_restart gmond.service
%endif

%post -n %{pname}-gmetad%{PROJ_DELIM}
%if 0%{?sles_version} || 0%{?suse_version}
%service_add_post gmetad.service
%else
%systemd_post gmetad.service
%endif

%preun -n %{pname}-gmetad%{PROJ_DELIM}
%if 0%{?sles_version} || 0%{?suse_version}
%service_del_preun gmetad.service
%else
%systemd_preun gmetad.service
%endif

%postun -n %{pname}-gmetad%{PROJ_DELIM}
%if 0%{?sles_version} || 0%{?suse_version}
%service_del_postun gmetad.service
%else 
%systemd_postun_with_restart gmetad.service
%endif

%else

%post -n %{pname}-gmond%{PROJ_DELIM}
/sbin/chkconfig --add gmond

%post -n %{pname}-gmetad%{PROJ_DELIM}
/sbin/chkconfig --add gmetad

%preun -n %{pname}-gmetad%{PROJ_DELIM}
if [ "$1" = 0 ]; then
  /sbin/service gmetad stop >/dev/null 2>&1 || :
  /sbin/chkconfig --del gmetad
fi

%preun -n %{pname}-gmond%{PROJ_DELIM}
if [ "$1" = 0 ]; then
  /sbin/service gmond stop >/dev/null 2>&1 || :
  /sbin/chkconfig --del gmond
fi

%endif # systemd

%post -n %{pname}-devel%{PROJ_DELIM} -p /sbin/ldconfig
%postun -n %{pname}-devel%{PROJ_DELIM} -p /sbin/ldconfig

%post -n %{pname}-web%{PROJ_DELIM}
if [ ! -L /usr/share/%{name}/lib/Zend ]; then
  /usr/bin/ln -s /usr/share/php/Zend /usr/share/%{name}/lib/Zend
fi
%if 0%{?sles_version} || 0%{?suse_version}
/usr/sbin/a2enmod php5
/usr/bin/systemctl try-restart apache2.service
%else
/usr/bin/systemctl try-restart httpd.service
%endif

%files
%doc AUTHORS COPYING NEWS README ChangeLog
%{_libdir}/libganglia*.so.*
%dir %{_libdir}/ganglia
%{_libdir}/ganglia/*.so
%exclude %{_libdir}/ganglia/modpython.so

%files -n %{pname}-gmetad%{PROJ_DELIM}
%dir %{_localstatedir}/lib/%{pname}
%attr(0755,ganglia,ganglia) %{_localstatedir}/lib/%{pname}/rrds
%{_sbindir}/gmetad
%if 0%{?systemd}
%{_unitdir}/gmetad.service
%else
%{_sysconfdir}/init.d/gmetad
%endif
%{_mandir}/man1/gmetad.1*
%{_mandir}/man1/gmetad.py.1*
%dir %{_sysconfdir}/ganglia
%config(noreplace) %{_sysconfdir}/ganglia/gmetad.conf

%files -n %{pname}-gmond%{PROJ_DELIM}
%{_bindir}/gmetric
%{_bindir}/gstat
%{_sbindir}/gmond
%if 0%{?systemd}
%{_unitdir}/gmond.service
%else
%{_sysconfdir}/init.d/gmond
%endif
%{_mandir}/man5/gmond.conf.5*
%{_mandir}/man1/gmond.1*
%{_mandir}/man1/gstat.1*
%{_mandir}/man1/gmetric.1*
%dir %{_sysconfdir}/ganglia
%dir %{_sysconfdir}/ganglia/conf.d
%config(noreplace) %{_sysconfdir}/ganglia/gmond.conf
%config(noreplace) %{_sysconfdir}/ganglia/conf.d/*.conf
%exclude %{_sysconfdir}/ganglia/conf.d/modpython.conf

%files -n %{pname}-gmond-python%{PROJ_DELIM}
%dir %{_libdir}/ganglia/python_modules/
%{_libdir}/ganglia/python_modules/*.py*
%{_libdir}/ganglia/modpython.so*
%config(noreplace) %{_sysconfdir}/ganglia/conf.d/*.pyconf*
%config(noreplace) %{_sysconfdir}/ganglia/conf.d/modpython.conf

%files -n %{pname}-devel%{PROJ_DELIM}
%{_bindir}/ganglia-config
%{_includedir}/*.h
%{_libdir}/libganglia*.so

%files -n %{pname}-web%{PROJ_DELIM}
%doc web/AUTHORS web/COPYING web/README web/TODO
%config(noreplace) %{_sysconfdir}/%{pname}/conf.php
%if 0%{?sles_version} || 0%{?suse_version}
%config(noreplace) %{_sysconfdir}/apache2/conf.d/%{name}.conf
%else
%config(noreplace) %{_sysconfdir}/httpd/conf.d/%{name}.conf
%endif
%{_datadir}/%{name}
%if 0%{?sles_version} || 0%{?suse_version}
%dir %attr(0755,wwwrun,www) %{_localstatedir}/lib/%{pname}/conf
%dir %attr(0755,wwwrun,www) %{_localstatedir}/lib/%{pname}/dwoo
%dir %attr(0755,wwwrun,www) %{_localstatedir}/lib/%{pname}/dwoo/cache
%dir %attr(0755,wwwrun,www) %{_localstatedir}/lib/%{pname}/dwoo/compiled
%else
%dir %attr(0755,apache,apache) %{_localstatedir}/lib/%{pname}/conf
%dir %attr(0755,apache,apache) %{_localstatedir}/lib/%{pname}/dwoo
%dir %attr(0755,apache,apache) %{_localstatedir}/lib/%{pname}/dwoo/cache
%dir %attr(0755,apache,apache) %{_localstatedir}/lib/%{pname}/dwoo/compiled
%endif
