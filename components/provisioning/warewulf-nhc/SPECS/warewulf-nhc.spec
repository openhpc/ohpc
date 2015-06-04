#-------------------------------------------------------------------------------
# Copyright (c) 2015, Intel Corporation
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Intel Corporation nor the names of its contributors
#       may be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#-------------------------------------------------------------------------------

%{!?_rel:%{expand:%%global _rel 0.r%(test "1723" != "0000" && echo "1723" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}
%include %{_sourcedir}/FSP_macros
%define pname warewulf-nhc
%define sname nhc
%define debug_package %{nil}
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}


%{!?nhc_script_dir:%global nhc_script_dir %{_sysconfdir}/%{sname}/scripts}
%{!?nhc_helper_dir:%global nhc_helper_dir %{_libexecdir}/%{sname}}

Name: %{pname}%{PROJ_DELIM}
Summary: Warewulf Node Health Check System
Version: 1.4.1
Release: 1%{?dist}
License: US Dept. of Energy (BSD-like)
Group: fsp/provisioning
URL: http://warewulf.lbl.gov/
Source0: %{pname}-%{version}.tar.gz
Source1: FSP_macros
Packager: %{?_packager}%{!?_packager:Michael Jennings <mej@lbl.gov>}
Vendor: %{?_vendorinfo}%{!?_vendorinfo:Warewulf Project (http://warewulf.lbl.gov/)}
Distribution: %{?_distribution:%{_distribution}}%{!?_distribution:%{_vendor}}
Requires: bash
BuildConflicts: post-build-checks
BuildArch: noarch
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root

%description
This package contains the Warewulf Node Health Check system.

TORQUE (and other resource managers) allow for the execution of a
script to determine if a node is "healthy" or "unhealthy" and
potentially mark unhealthy nodes as unavailable.  The scripts
contained in this package provide a flexible, extensible mechanism for
collecting health checks to be run on your cluster and specifying
which checks should be run on which nodes.


%prep
%setup -n %{pname}-%{version}


%build
%{configure}
%{__make} %{?mflags}


%install
umask 0077
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}


%check
%{__make} test


%clean
test "$RPM_BUILD_ROOT" != "/" && %{__rm} -rf $RPM_BUILD_ROOT


%files
%defattr(-, root, root)
%doc COPYING ChangeLog LICENSE nhc.conf contrib/nhc.cron
%dir %{_sysconfdir}/%{sname}/
%dir %{_localstatedir}/lib/%{sname}/
%dir %{_localstatedir}/run/%{sname}/
%dir %{nhc_script_dir}/
%dir %{nhc_helper_dir}/
%config(noreplace) %{_sysconfdir}/%{sname}/%{sname}.conf
%config(noreplace) %{_sysconfdir}/logrotate.d/%{sname}
%config(noreplace) %{nhc_script_dir}/*.nhc
%config(noreplace) %{nhc_helper_dir}/*
%config(noreplace) %{_sbindir}/%{sname}
%config(noreplace) %{_sbindir}/%{sname}-genconf
%config(noreplace) %{_sbindir}/%{sname}-wrapper
