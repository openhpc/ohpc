Summary: A Linux operating system framework for managing HPC clusters
Name: losf
Version: 0.50.0
Release: 1
License: GPL-2
Group: System Environment/Base
BuildArch: noarch
URL: https://github.com/hpcsi/losf 
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%if 0%{?FSP_BUILD}
%{!?prefix: %define prefix %{FSP_LOCAL}}
%else
%{!?prefix: %define prefix /opt}
%endif

%define installPath %{prefix}/%{name}

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
%setup -q 

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



