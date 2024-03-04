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

%define dname ipmi
%define pname warewulf-%{dname}
%define wwsrvdir /srv
%define develSHA c6de604fc76eabfaef2cb99f4c6ae5ed44eff1e0
%define wwextract warewulf3-%{develSHA}

Name:    %{pname}%{PROJ_DELIM}
Version: 3.10.0
Provides: warewulf-ipmi = %{version}
Release: 1%{?dist}
Summary: Warewulf - IPMI support
License: US Dept. of Energy (BSD-like)
URL: http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/%{develSHA}.tar.gz
Patch0:  openEuler.initramfs.patch
Group:   %{PROJ_NAME}/provisioning
ExclusiveOS: linux
Conflicts: warewulf < 3
Requires: warewulf-common%{PROJ_DELIM}
Requires: %{name}-initramfs-%{_arch} = %{version}-%{release}

BuildRequires: ipmitool
Requires: ipmitool
BuildRequires: which
%if 0%{?rhel}
BuildRequires: perl-generators
Requires: perl(sys/ioctl.ph)
%endif

BuildRequires: autoconf
BuildRequires: automake
BuildRequires: make
BuildRequires: gcc
BuildRequires: warewulf-common%{PROJ_DELIM}
BuildRequires: openssl-devel

%description
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

Warewulf IPMI adds required tools and libraries to support IPMI on
cluster nodes.


%prep
cd %{_builddir}
%{__rm} -rf %{name}-%{version} %{wwextract}
%{__ln_s} %{wwextract}/%{dname} %{name}-%{version}
%setup -q -D
%if 0%{?openEuler}
%patch0 -p2
%endif

%build
./autogen.sh
%configure --sharedstatedir=%{wwsrvdir} --with-local-ipmitool=yes
%{__make} %{?_smp_mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT


%files
%doc AUTHORS ChangeLog INSTALL NEWS README TODO COPYING
%{perl_vendorlib}/Warewulf/Ipmi.pm
%{perl_vendorlib}/Warewulf/Module/Cli/*


%package initramfs-%{_arch}
Summary: Warewulf - Add IPMI to %{_arch} initramfs
BuildArch: noarch
Requires: warewulf-common%{PROJ_DELIM}
Requires: warewulf-provision%{PROJ_DELIM}-initramfs-%{_arch}

%description initramfs-%{_arch}
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

Warewulf IPMI-initramfs adds IPMI configuration to the %{_arch} cluster
node boot image.

%files initramfs-%{_arch}
%dir %{wwsrvdir}/warewulf
%dir %{wwsrvdir}/warewulf/initramfs
%{wwsrvdir}/warewulf/initramfs/%{_arch}
