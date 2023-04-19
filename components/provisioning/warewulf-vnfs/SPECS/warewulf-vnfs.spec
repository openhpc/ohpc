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

%define dname vnfs
%define pname warewulf-%{dname}
%define wwsrvdir /srv
%define develSHA c6de604fc76eabfaef2cb99f4c6ae5ed44eff1e0
%define wwextract warewulf3-%{develSHA}

Name:    %{pname}%{PROJ_DELIM}
Version: 3.10.0
Release: 1%{?dist}
Summary: Warewulf - Virtual Node File System support
License: US Dept. of Energy (BSD-like)
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/%{develSHA}.tar.gz
Source1: opensuse-15.2.tmpl
Source2: rocky-8.tmpl
Source3: opensuse-15.4.tmpl
Source4: openeuler-22.03.tmpl
Source5: include-openEuler
Source6: rocky-9.tmpl
Source7: opensuse-15.3.tmpl
Patch0:  warewulf-vnfs.aarch64.bootstrap.patch
Patch1:  warewulf-vnfs.aarch64.bootstrap_usb.patch
Patch2:  warewulf-vnfs.bootstrap_msr.patch
Patch3:  warewulf-vnfs.pigz.patch
Patch4:  warewulf-vnfs.rhel-proxy.patch
Patch5:  warewulf-vnfs.sle.bootstrap_kernel.patch
Patch6:  warewulf-vnfs.utf8.patch
Patch7:  warewulf-vnfs.dnf.rhel8.patch
Patch8:  warewulf-vnfs.centos8.patch
Patch9:  warewulf-vnfs.varlog.patch
Patch10:  warewulf-vnfs.bootstrap_qlogic.patch
Patch11: warewulf-vnfs.leap.patch
Patch12: warewulf-vnfs.bootstrap_drivers.patch
Patch13: warewulf-vnfs.hybridize.patch
Patch14: warewulf-vnfs.boot_fw_symlink.patch
Patch15: warewulf-vnfs.wwvnfs.requires.patch
Group:   %{PROJ_NAME}/provisioning
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM}
Requires: pigz
%if 0%{?rhel}
Requires: perl-IO-Compress
%endif
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: make
BuildRequires: warewulf-common%{PROJ_DELIM}
Conflicts: warewulf < 3
BuildArch: noarch

%description
Warewulf is an operating system management toolkit designed to facilitate
large scale deployments of systems on physical, virtual and cloud-based
infrastructures. It facilitates elastic and large deployments consisting
of groups of homogeneous systems.

Warewulf utilizes a Virtual Node File System (VNFS) where each node can
be managed using a chroot representing the root file system. The
Warewulf VNFS package provides tools to create VNFS images for different
node operating systems.


%prep
cd %{_builddir}
%{__rm} -rf %{name}-%{version} %{wwextract}
%{__ln_s} %{wwextract}/%{dname} %{name}-%{version}
%setup -q -D
%ifarch aarch64
%patch0 -p1
%patch1 -p1
%endif
%patch2 -p1
%patch3 -p1
%patch4 -p1
%patch5 -p1
%patch6 -p1
%patch7 -p1
%patch8 -p1
%patch9 -p1
%patch10 -p1
%patch11 -p1
%patch12 -p1
%patch13 -p1
%patch14 -p1
%patch15 -p1

%build
./autogen.sh
%configure --sharedstatedir=%{wwsrvdir}
%{__make} %{?mflags}


%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}
install -D -m 0644 %SOURCE1 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/opensuse-15.2.tmpl
install -D -m 0644 %SOURCE2 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/rocky-8.tmpl
install -D -m 0644 %SOURCE3 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/opensuse-15.3.tmpl
install -D -m 0644 %SOURCE4 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/openeuler-22.03.tmpl
install -D -m 0644 %SOURCE5 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/include-openEuler
install -D -m 0644 %SOURCE6 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/rocky-9.tmpl
install -D -m 0644 %SOURCE7 %{buildroot}/%{_libexecdir}/warewulf/wwmkchroot/opensuse-15.4.tmpl

%files
%doc AUTHORS ChangeLog INSTALL NEWS README TODO COPYING LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/vnfs.conf
%config(noreplace) %{_sysconfdir}/warewulf/bootstrap.conf
%{_libexecdir}/warewulf/*
%{_bindir}/*
%{_mandir}/*


