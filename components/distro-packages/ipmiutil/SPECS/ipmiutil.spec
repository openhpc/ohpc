
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

%define pname ipmiutil

Name:    %{pname}%{PROJ_DELIM}
Version: 2.9.6
Release: 1%{?dist}
Summary:   Easy-to-use IPMI server management utilities
License:   BSD 3-clause
Group:     %{PROJ_NAME}/distro-packages
Source0:   https://downloads.sourceforge.net/project/ipmiutil/%{pname}-%{version}.tar.gz
URL:       http://ipmiutil.sourceforge.net
Provides: %{pname}
%if 0%{?suse_version} >= 1210
%define req_systemd 1
%endif
%if 0%{?sles_version} >= 10
BuildRequires: libopenssl-devel
%else
BuildRequires: openssl-devel
%endif
%if 0%{?req_systemd}
BuildRequires: gcc gcc-c++ libtool systemd
%define unit_dir  %{_unitdir}
%define systemd_fls %{unit_dir}
%else
BuildRequires: gcc gcc-c++ libtool
%define unit_dir  %{_unitdir}
%define systemd_fls %{_datadir}/%{pname}
%endif
%define init_dir  %{_initrddir}

%description
The ipmiutil package provides easy-to-use utilities to view the SEL,
perform an IPMI chassis reset, set up the IPMI LAN and Platform Event Filter
entries to allow SNMP alerts, Serial-Over-LAN console, event daemon, and
other IPMI tasks.
These can be invoked with the metacommand ipmiutil, or via subcommand
shortcuts as well.  IPMIUTIL can also write sensor thresholds, FRU asset tags,
and has a full IPMI configuration save/restore.
An IPMI driver can be provided by either the OpenIPMI driver (/dev/ipmi0)
or the Intel IPMI driver (/dev/imb), etc.  If used locally and no driver is
detected, ipmiutil will use user-space direct I/Os instead.

%package -n %{pname}-devel%{PROJ_DELIM}
Group:    Development/Libraries
Summary:  Includes libraries and headers for the ipmiutil package
Requires: %{pname}
provides: %{pname}-devel

%description -n %{pname}-devel%{PROJ_DELIM}
The ipmiutil-devel package contains headers and libraries which are
useful for building custom IPMI applications.

%package -n %{pname}-static%{PROJ_DELIM}
Group:    Development/Libraries
Summary:  Includes static libraries for the ipmiutil package

%description -n %{pname}-static%{PROJ_DELIM}
The ipmiutil-static package contains static libraries which are
useful for building custom IPMI applications.

%prep
%setup -q -n %{pname}-%{version}

%build
%if 0%{?req_systemd}
%configure --enable-systemd --enable-libsensors
%else
%configure --enable-libsensors
%endif
make

%install
make install DESTDIR=%{buildroot}

%files
%dir %{_datadir}/%{pname}
%dir %{_var}/lib/%{pname}
%{_bindir}/ipmiutil
%{_bindir}/idiscover
%{_bindir}/ievents
%{_sbindir}/iseltime
%{_sbindir}/ipmi_port
%{_sbindir}/ialarms
%{_sbindir}/iconfig
%{_sbindir}/icmd
%{_sbindir}/ifru
%{_sbindir}/igetevent
%{_sbindir}/ihealth
%{_sbindir}/ilan
%{_sbindir}/ireset
%{_sbindir}/isel
%{_sbindir}/isensor
%{_sbindir}/iserial
%{_sbindir}/isol
%{_sbindir}/iwdt
%{_sbindir}/ipicmg
%{_sbindir}/ifirewall
%{_sbindir}/ifwum
%{_sbindir}/ihpm
%{_datadir}/%{pname}/ipmiutil_evt
%{_datadir}/%{pname}/ipmiutil_asy
%{_datadir}/%{pname}/ipmiutil_wdt
%{_datadir}/%{pname}/ipmi_port
%{_datadir}/%{pname}/ipmi_info
%{_datadir}/%{pname}/checksel
%{systemd_fls}/ipmiutil_evt.service
%{systemd_fls}/ipmiutil_asy.service
%{systemd_fls}/ipmiutil_wdt.service
%{systemd_fls}/ipmi_port.service
%{_datadir}/%{pname}/ipmiutil.env
%{_datadir}/%{pname}/ipmiutil.pre
%{_datadir}/%{pname}/ipmiutil.setup
%{_datadir}/%{pname}/ipmi_if.sh
%{_datadir}/%{pname}/evt.sh
%{_datadir}/%{pname}/ipmi.init.basic
%{_datadir}/%{pname}/bmclanpet.mib
%{_mandir}/man8/isel.8*
%{_mandir}/man8/isensor.8*
%{_mandir}/man8/ireset.8*
%{_mandir}/man8/igetevent.8*
%{_mandir}/man8/ihealth.8*
%{_mandir}/man8/iconfig.8*
%{_mandir}/man8/ialarms.8*
%{_mandir}/man8/iwdt.8*
%{_mandir}/man8/ilan.8*
%{_mandir}/man8/iserial.8*
%{_mandir}/man8/ifru.8*
%{_mandir}/man8/icmd.8*
%{_mandir}/man8/isol.8*
%{_mandir}/man8/ipmiutil.8*
%{_mandir}/man8/idiscover.8*
%{_mandir}/man8/ievents.8*
%{_mandir}/man8/ipmi_port.8*
%{_mandir}/man8/ipicmg.8*
%{_mandir}/man8/ifirewall.8*
%{_mandir}/man8/ifwum.8*
%{_mandir}/man8/ihpm.8*
%{_mandir}/man8/isunoem.8*
%{_mandir}/man8/idelloem.8*
%{_mandir}/man8/ismcoem.8*
%{_mandir}/man8/iekanalyzer.8*
%{_mandir}/man8/itsol.8*
%{_mandir}/man8/idcmi.8*
%doc AUTHORS ChangeLog COPYING NEWS README TODO
%doc doc/UserGuide

%files -n %{pname}-devel%{PROJ_DELIM}
%dir %{_datadir}/%{pname}
%{_datadir}/%{pname}/ipmi_sample.c
%{_datadir}/%{pname}/ipmi_sample_evt.c
%{_datadir}/%{pname}/isensor.c
%{_datadir}/%{pname}/ievents.c
%{_datadir}/%{pname}/isensor.h
%{_datadir}/%{pname}/ievents.h
%{_datadir}/%{pname}/Makefile
%{_includedir}/ipmicmd.h
%{_libdir}/libipmiutil.so

%files -n %{pname}-static%{PROJ_DELIM}
%{_libdir}/libipmiutil.a

%pre
%if 0%{?req_systemd}
%service_add_pre ipmi_port.service ipmiutil_evt.service ipmiutil_asy.service ipmiutil_wdt.service
%endif

%post
# POST_INSTALL, $1 = 1 if rpm -i, $1 = 2 if rpm -U
if [ "$1" = "1" ]
then
   # doing rpm -i, first time
   vardir=%{_var}/lib/%{pname}
   scr_dir=%{_datadir}/%{pname}

%if 0%{?req_systemd}
%service_add_post ipmi_port.service ipmiutil_evt.service ipmiutil_asy.service ipmiutil_wdt.service
%else
   if [ -x /bin/systemctl ]; then
      echo "IINITDIR=%{init_dir}" >>%{_datadir}/%{pname}/ipmiutil.env
      cp -f ${scr_dir}/ipmiutil_evt.service %{unit_dir}
      cp -f ${scr_dir}/ipmiutil_asy.service %{unit_dir}
      cp -f ${scr_dir}/ipmiutil_wdt.service %{unit_dir}
      cp -f ${scr_dir}/ipmi_port.service    %{unit_dir}
      # systemctl enable ipmi_port.service >/dev/null 2>&1 || :
   else
      cp -f ${scr_dir}/ipmiutil_wdt %{init_dir}
      cp -f ${scr_dir}/ipmiutil_asy %{init_dir}
      cp -f ${scr_dir}/ipmiutil_evt %{init_dir}
      cp -f ${scr_dir}/ipmi_port    %{init_dir}
      cp -f ${scr_dir}/ipmi_info    %{init_dir}
   fi
%endif

   # Run some ipmiutil command to see if any IPMI interface works.
   %{_bindir}/ipmiutil sel -v >/dev/null 2>&1 || :
   IPMIret=$?
   # If IPMIret==0, the IPMI cmd was successful, and IPMI is enabled locally.
   if [ $IPMIret -eq 0 ]; then
      # If IPMI is enabled, automate managing the IPMI SEL
      if [ -d %{_sysconfdir}/cron.daily ]; then
         cp -f %{_datadir}/%{pname}/checksel %{_sysconfdir}/cron.daily
      fi
      # IPMI_IS_ENABLED, so enable services, but only if Red Hat
      if [ -f /etc/redhat-release ]; then
         if [ -x /bin/systemctl ]; then
            touch ${scr_dir}/ipmi_port.service
         elif [ -x /sbin/chkconfig ]; then
            /sbin/chkconfig --add ipmi_port
            /sbin/chkconfig --add ipmiutil_wdt
            /sbin/chkconfig --add ipmiutil_evt
            /sbin/chkconfig --add ipmi_info
         fi
      fi

      # Capture a snapshot of IPMI sensor data once now for later reuse.
      sensorout=$vardir/sensor_out.txt
      if [ ! -f $sensorout ]; then
         %{_bindir}/ipmiutil sensor -q >$sensorout || :
	 if [ $? -ne 0 ]; then
	    # remove file if error, try again in ipmi_port on reboot.
	    rm -f $sensorout
	 fi
      fi
   fi
else
   # postinstall, doing rpm update
   %{_bindir}/ipmiutil sel -v >/dev/null 2>&1 || :
   if [ $? -eq 0 ]; then
      if [ -d %{_sysconfdir}/cron.daily ]; then
         cp -f %{_datadir}/%{pname}/checksel %{_sysconfdir}/cron.daily
      fi
   fi
fi
%if 0%{?fedora} >= 18
%systemd_post  ipmiutil_evt.service
%systemd_post  ipmiutil_asy.service
%systemd_post  ipmiutil_wdt.service
%systemd_post  ipmi_port.service
%endif

%preun
# before uninstall,  $1 = 1 if rpm -U, $1 = 0 if rpm -e
if [ "$1" = "0" ]
then
%if 0%{?req_systemd}
%service_del_preun ipmi_port.service ipmiutil_evt.service ipmiutil_asy.service ipmiutil_wdt.service
%else
   if [ -x /bin/systemctl ]; then
     if [ -f %{unit_dir}/ipmiutil_evt.service ]; then
%if 0%{?fedora} >= 18
%systemd_preun  ipmiutil_evt.service
%systemd_preun  ipmiutil_asy.service
%systemd_preun  ipmiutil_wdt.service
%systemd_preun  ipmi_port.service
%else
        systemctl disable ipmi_port.service >/dev/null 2>&1 || :
        systemctl disable ipmiutil_evt.service >/dev/null 2>&1 || :
        systemctl disable ipmiutil_asy.service >/dev/null 2>&1 || :
        systemctl disable ipmiutil_wdt.service >/dev/null 2>&1 || :
        systemctl stop ipmiutil_evt.service >/dev/null 2>&1 || :
        systemctl stop ipmiutil_asy.service >/dev/null 2>&1 || :
        systemctl stop ipmiutil_wdt.service >/dev/null 2>&1 || :
        systemctl stop ipmi_port.service    >/dev/null 2>&1 || :
%endif
     fi
   else
     if [ -x /sbin/service ]; then
        /sbin/service ipmi_port stop       >/dev/null 2>&1 || :
        /sbin/service ipmiutil_wdt stop    >/dev/null 2>&1 || :
        /sbin/service ipmiutil_asy stop    >/dev/null 2>&1 || :
        /sbin/service ipmiutil_evt stop    >/dev/null 2>&1 || :
     fi
     if [ -x /sbin/chkconfig ]; then
        /sbin/chkconfig --del ipmi_port    >/dev/null 2>&1 || :
        /sbin/chkconfig --del ipmiutil_wdt >/dev/null 2>&1 || :
        /sbin/chkconfig --del ipmiutil_asy >/dev/null 2>&1 || :
        /sbin/chkconfig --del ipmiutil_evt >/dev/null 2>&1 || :
     fi
   fi
%endif
   if [ -f %{_sysconfdir}/cron.daily/checksel ]; then
        rm -f %{_sysconfdir}/cron.daily/checksel
   fi
fi

%postun
%if 0%{?req_systemd}
%service_del_postun ipmi_port.service ipmiutil_evt.service ipmiutil_asy.service ipmiutil_wdt.service
%else
if [ -x /bin/systemctl ]; then
%if 0%{?fedora} >= 18
%systemd_postun_with_restart  ipmi_port.service
%else
   systemctl daemon-reload  || :
   if [ $1 -ge 1 ] ; then
      # Package upgrade, not uninstall
      systemctl try-restart ipmi_port.service  || :
   fi
%endif
   if [ -f %{unit_dir}/ipmiutil_evt.service ]; then
      rm -f %{unit_dir}/ipmiutil_evt.service  2>/dev/null || :
      rm -f %{unit_dir}/ipmiutil_asy.service  2>/dev/null || :
      rm -f %{unit_dir}/ipmiutil_wdt.service  2>/dev/null || :
      rm -f %{unit_dir}/ipmi_port.service     2>/dev/null || :
   fi
else
   if [ -f %{init_dir}/ipmiutil_evt.service ]; then
      rm -f %{init_dir}/ipmiutil_wdt 2>/dev/null || :
      rm -f %{init_dir}/ipmiutil_asy 2>/dev/null || :
      rm -f %{init_dir}/ipmiutil_evt 2>/dev/null || :
      rm -f %{init_dir}/ipmi_port    2>/dev/null || :
   fi
fi
%endif
