#---------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname nhc

%{!?nhc_script_dir:%global nhc_script_dir %{_sysconfdir}/%{pname}/scripts}
%{!?nhc_helper_dir:%global nhc_helper_dir %{_libexecdir}/%{pname}}

Summary: LBNL Node Health Check
Name: %{pname}%{PROJ_DELIM}
Version: 1.4.2
Release: 1%{?dist}
License: US Dept. of Energy (BSD-like)
Group: %{PROJ_NAME}/admin
URL: https://github.com/mej/nhc/
Source0: https://github.com/mej/nhc/archive/%{version}.tar.gz#/%{pname}-%{version}.tar.gz
# upstream patch will land in v1.4.3
Patch1:  nhc-bash-substitution.patch
Requires: bash
BuildRequires: automake autoconf
BuildRequires: rpm-build
Obsoletes: warewulf-nhc%{PROJ_DELIM} <= 1.4.2-1
BuildArch: noarch

%description
TORQUE (and other resource managers) allow for the execution of a
script to determine if a node is "healthy" or "unhealthy" and
potentially mark unhealthy nodes as unavailable.  The scripts
contained in this package provide a flexible, extensible mechanism for
collecting health checks to be run on your cluster and specifying
which checks should be run on which nodes.


%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build
if [ ! -f configure ]; then
  ./autogen.sh
fi
rpmbuild --showrc
%{configure}
%{__make} %{?mflags}


%install
umask 0077
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%triggerpostun -p /bin/bash -- warewulf-nhc <= 1.4.2-1
if [ $1 -gt 0 -a $2 -eq 0 ]; then
    cd %{_sysconfdir}/%{pname}/scripts
    for SCRIPT in ww_*.nhc.rpmsave ; do
        if [ -e $SCRIPT ]; then
            NEWSCRIPT=lbnl${SCRIPT##ww}
            NEWSCRIPT=${NEWSCRIPT%%.rpmsave}
            echo warning: Auto-fixing script naming due to modified script ${SCRIPT%%.rpmsave}
            mv -v $NEWSCRIPT $NEWSCRIPT.rpmnew && mv -v $SCRIPT $NEWSCRIPT
        fi
    done 2>/dev/null



%files
%doc COPYING ChangeLog LICENSE nhc.conf contrib/nhc.cron
%dir %{_sysconfdir}/%{pname}/
%dir %{_localstatedir}/lib/%{pname}/
%dir %{_localstatedir}/run/%{pname}/
%dir %{nhc_script_dir}/
%dir %{nhc_helper_dir}/
%config(noreplace) %{_sysconfdir}/%{pname}/%{pname}.conf
%config(noreplace) %{_sysconfdir}/logrotate.d/%{pname}
%config(noreplace) %{nhc_script_dir}/*.nhc
%config(noreplace) %{nhc_helper_dir}/*
%config(noreplace) %{_sbindir}/%{pname}
%config(noreplace) %{_sbindir}/%{pname}-genconf
%config(noreplace) %{_sbindir}/%{pname}-wrapper
