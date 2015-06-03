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

%include %{_sourcedir}/FSP_macros

%define pname losf
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   A Linux operating system framework for managing HPC clusters
Name:      %{pname}%{PROJ_DELIM}
Version:   0.51.2
Release:   1
License:   GPL-2
Group:     fsp/admin
BuildArch: noarch
URL:       https://github.com/hpcsi/losf 
Source0:   %{pname}-%{version}.tar.gz
Source1:   FSP_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

%if 0%{?FSP_BUILD}
%{!?prefix: %define prefix %{FSP_ADMIN}}
%else
%{!?prefix: %define prefix /opt}
%endif


%define installPath %{prefix}/%{pname}

provides: perl(LosF_node_types)
provides: perl(LosF_rpm_topdir)
provides: perl(LosF_rpm_utils)
provides: perl(LosF_utils)
provides: perl(LosF_history_utils)

%if 0%{?sles_version} || 0%{?suse_version}
requires: perl-Config-IniFiles >= 2.43 
requires: perl-Log-Log4perl
%else
requires: yum-plugin-downloadonly
%endif

%define __spec_install_post %{nil}
%define debug_package %{nil}
%define __os_install_post %{_dbpath}/brp-compress

%description

LosF is designed to provide a lightweight configuration management system
designed for use with high-performance computing (HPC) clusters. Target users
for this package are HPC system administrators and system architects who desire
flexible command-line utilities for synchronizing various host types across a
cluster.

%prep
%setup -q -n %{pname}-%{version}

%build
# Binary pass-through - empty build section

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p %{buildroot}/%{installPath}
mkdir -p %{buildroot}/etc/profile.d
cp -a * %{buildroot}/%{installPath}

# Remove separate test dir to minimize dependencies

rm -rf %{buildroot}/%{installPath}/test

# Add soft links to CLI binaries in default path

mkdir -p ${RPM_BUILD_ROOT}/%{_bindir}

for i in losf update initconfig koomie_cf sync_config_files node_types rpm_topdir ; do
    ln -sf %{installPath}/$i ${RPM_BUILD_ROOT}/%{_bindir}
done

for i in idisk ilog ioff ion ipxe ireboot ireset isensor isoft istat ; do 
    ln -sf %{installPath}/utils/$i ${RPM_BUILD_ROOT}/%{_bindir}
done

%clean
rm -rf $RPM_BUILD_ROOT

%post

%postun

# Following occurs when removing last version of the package. Remove config_dir.

if [ "$1" = 0 ];then
    if [ -s %{installPath}/config/config_dir ];then
	rm %{installPath}/config/config_dir
    fi
fi

%files
%defattr(-,root,root,-)
%if 0%{?FSP_BUILD}
%dir %{FSP_HOME}
%dir %{prefix}
%endif

%{installPath}
%{_bindir}/*



