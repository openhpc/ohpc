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

%{!?_rel:%{expand:%%global _rel 0.r%(test "1686" != "0000" && echo "1686" || svnversion | sed 's/[^0-9].*$//' | grep '^[0-9][0-9]*$' || git svn find-rev `git show -s --pretty=format:%h` || echo 0000)}}
%include %{_sourcedir}/FSP_macros
%define debug_package %{nil}

%define pname warewulf-vnfs
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary: Warewulf VNFS Module
Name:    %{pname}%{PROJ_DELIM}
Version: 3.6
Release: %{_rel}%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   fsp/provisioning
URL:     http://warewulf.lbl.gov/
Source:  %{pname}-%{version}.tar.gz
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM}
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3
%if 0%{?sles_version} || 0%{?suse_version}
BuildArch: x86_64
%else
BuildArch: noarch
%endif
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{pname}-%{version}-%{release}-root
# Previous version had an architecture in its release. This is necessary for
# YUM to properly update a package of a different BuildArch...
Obsoletes: warewulf-vnfs < 3.2-0
# 06/18/14 charles.r.baird@intel.com - wwmkchroot patch for SLES
Patch1: warewulf-vnfs.wwmkchroot.patch
# 09/10/14 charles.r.baird@intel.com - special chars in vnfs filenames
Patch2: warewulf-vnfs.utf8.patch
# 09/19/14 karl.w.schulz@intel.com - include yum/numactl in centos6 default image
Patch3: centos-add-pkgs.patch
# 02/10/15 charles.r.baird@intel.com - add drivers to bootstrap image
Patch4: warewulf-vnfs.bootstrap.patch
# 03/11/15 karl.w.schulz@intel.com - add in centos7 template (culled from ww trunk)
Source1: centos-7.tmpl
# 03/13/15 karl.w.schulz@intel.com - honor local proxy setting if defined (rhel)
Patch5: rhel-proxy.patch


%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.

This is the VNFS module which supports the creation and management of
Virtual Node FileSystem objects.


%prep
%setup -n %{pname}-%{version}

# Intel FSP patches

%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1
%patch5 -p1


%build
%configure
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

# 03/11/15 karl.w.schulz@intel.com - add in centos7 template (culled from ww trunk)
install -D -m 0644 %SOURCE1 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/centos-7.tmpl

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root)
%doc AUTHORS COPYING ChangeLog INSTALL NEWS README TODO LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/vnfs.conf
%config(noreplace) %{_sysconfdir}/warewulf/bootstrap.conf
%{_libexecdir}/warewulf/*
%{_bindir}/*


%changelog
