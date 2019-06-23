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

%define _cross_compile 0%{?cross_compile}
%define wwpkgdir /srv/warewulf

%define pname warewulf-provision
%define dname provision

Name:    %{pname}%{PROJ_DELIM}
Summary: Warewulf - Provisioning Module
Version: 3.9.0
Release: .1%{?dist}
License: US Dept. of Energy (BSD-like)
Group:   %{PROJ_NAME}/provisioning
URL:     http://warewulf.lbl.gov/
Source0: https://github.com/warewulf/warewulf3/archive/development.tar.gz
ExclusiveOS: linux
Requires: warewulf-common%{PROJ_DELIM}
Requires: warewulf-provision-initramfs-%{_arch}%{PROJ_DELIM} = %{version}-%{release}
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: warewulf-common%{PROJ_DELIM}
BuildRequires: libselinux-devel
BuildRequires: libacl-devel
BuildRequires: libattr-devel
BuildRequires: libuuid-devel
BuildRequires: device-mapper-devel
BuildRequires: xz-devel
BuildRequires: libtirpc-devel

%if 0%{?_cross_compile}
%define configopt_cross "--enable-cross-compile"
%if "%{_arch}" == "x86_64"
BuildRequires: gcc-aarch64-linux-gnu
%endif
%if "%{_arch}" == "aarch64"
BuildRequires: gcc-x86_64-linux-gnu
%endif
%endif

Conflicts: warewulf < 3.9
#!BuildIgnore: post-build-checks
Patch1: warewulf-provision.bin-file.patch
Patch2: warewulf-provision.ipxe-kargs.patch
Patch3: warewulf-provision.parted_libdir.patch
Patch4: warewulf-provision.ppc64le.patch
Patch5: warewulf-provision.pxe_file_modes.patch
Patch6: warewulf-provision.sles_tftpboot.patch
Patch7: warewulf-provision.wwgetfiles.patch

%description
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.  The
provision module provides functionality for provisioning, configuring,
and booting systems.

This package contains the core provisioning components and
administrative tools.  To actually provision systems, the
%{name}-server package is also required.


%package -n %{pname}-initramfs-%{_arch}%{PROJ_DELIM}
Summary: Warewulf - Provisioning Module - Initramfs Base and Capabilities for %{_arch}
Group: %{PROJ_NAME}/provisioning
BuildArch: noarch

%description -n %{pname}-initramfs-%{_arch}%{PROJ_DELIM}
Warewulf Provisioning initramfs Base and Capabilities for %{_arch}.

%package -n %{pname}-server%{PROJ_DELIM}
Summary: Warewulf - Provisioning Module - Server
Group: %{PROJ_NAME}/provisioning
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}

# 07/22/14 karl.w.schulz@intel.com - differentiate requirements per Base OS
%if 0%{?sles_version} || 0%{?suse_version}
Requires: apache2 apache2-mod_perl tftp dhcp-server xinetd tcpdump policycoreutils-python
%define configopt_suse "--with-apachemoddir=/usr/lib64/apache2"
%else
Requires: mod_perl httpd tftp-server dhcp xinetd tcpdump policycoreutils-python
%endif

# charles.r.baird@intel.com - required to determine where to stick warewulf-httpd.conf
%if 0%{?sles_version} || 0%{?suse_version} == 1315
BuildRequires: sles-release
%endif

%if "%{_arch}" == "x86_64" || 0%{?_cross_compile}
%package -n %{pname}-server-ipxe-x86_64%{PROJ_DELIM}
Summary: Warewulf - Provisioning Module - iPXE Bootloader x86_64
Group: %{PROJ_NAME}/provisioning
BuildArch: noarch

%description -n %{pname}-server-ipxe-x86_64%{PROJ_DELIM}
Warewulf bundled iPXE binaries for x86_64.
%endif

%if "%{_arch}" == "aarch64" || 0%{?_cross_compile}
%package -n %{pname}-server-ipxe-aarch64%{PROJ_DELIM}
Summary: Warewulf - Provisioning Module - iPXE Bootloader aarch64
Group: %{PROJ_NAME}/provisioning
BuildArch: noarch

%description -n %{pname}-server-ipxe-aarch64%{PROJ_DELIM}
Warewulf bundled iPXE binaries for aarch64.
%endif

%description -n %{pname}-server%{PROJ_DELIM}
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.  The
provision module provides functionality for provisioning, configuring,
and booting systems.

This package contains the CGI scripts and event components to actually
provision systems.  Systems used solely for administration of Warewulf
do not require this package.


%package -n %{pname}-gpl_sources%{PROJ_DELIM}
Summary: This package contains the GPL sources used in Warewulf
Group: %{PROJ_NAME}/provisioning
Requires: %{pname}%{PROJ_DELIM} = %{version}-%{release}

%description -n %{pname}-gpl_sources%{PROJ_DELIM}
Warewulf >= 3 is a set of utilities designed to better enable
utilization and maintenance of clusters or groups of computers.  The
provision module provides functionality for provisioning, configuring,
and booting systems.

Warewulf is distributed with some third-party software as a
convenience to our users.  While Warewulf itself is licensed under a
DOE license (a derivative of the BSD license), the third-party
software may have different licensing terms, including the GNU General
Public License (GPL).

In order to be 100% compatible with the GPL this package makes
available the included GPL software.


%prep
%setup -q -n warewulf3-development
cd %{dname}
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1
%patch5 -p1
%patch6 -p1
%patch7 -p1

%build
cd %{dname}
NO_CONFIGURE=1 ./autogen.sh

%configure --localstatedir=%{wwpkgdir} %{?configopt_cross} %{?configopt_suse}

%{__make} %{?mflags}


%install
cd %{dname}
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%{__mkdir} -p $RPM_BUILD_ROOT/%_docdir

%post -n %{pname}-server%{PROJ_DELIM}
# 07/22/14 karl.w.schulz@intel.com - specify alternate group per Base OS
%if 0%{?sles_version} || 0%{?suse_version}
usermod -a -G warewulf www >/dev/null 2>&1 || :
%else
usermod -a -G warewulf apache >/dev/null 2>&1 || :
%endif
systemctl enable httpd.service >/dev/null 2>&1 || :
systemctl restart httpd.service >/dev/null 2>&1 || :
systemctl enable tftp-server.socket >/dev/null 2>&1 || :
systemctl restart tftp-server.socket >/dev/null 2>&1 || :

mkdir -p %{_localstatedir}/warewulf/ipxe %{_localstatedir}/warewulf/bootstrap 2>/dev/null || :
semanage fcontext -a -t httpd_sys_content_t '%{_localstatedir}/warewulf/ipxe(/.*)?' 2>/dev/null || :
semanage fcontext -a -t httpd_sys_content_t '%{_localstatedir}/warewulf/bootstrap(/.*)?' 2>/dev/null || :
restorecon -R %{_localstatedir}/warewulf/bootstrap || :
restorecon -R %{_localstatedir}/warewulf/ipxe || :

if [ $1 -eq 2 ] ; then
    wwsh bootstrap rebuild
fi

%postun -n %{pname}-server%{PROJ_DELIM}
if [ $1 -eq 0 ] ; then
semanage fcontext -d -t httpd_sys_content_t '%{_localstatedir}/warewulf/ipxe(/.*)?' 2>/dev/null || :
semanage fcontext -d -t httpd_sys_content_t '%{_localstatedir}/warewulf/bootstrap(/.*)?' 2>/dev/null || :
fi

%files
%{OHPC_PUB}
%doc %{dname}/AUTHORS %{dname}/COPYING %{dname}/ChangeLog %{dname}/INSTALL %{dname}/NEWS %{dname}/README %{dname}/TODO %{dname}/LICENSE
%config(noreplace) %{_sysconfdir}/warewulf/provision.conf
%config(noreplace) %{_sysconfdir}/warewulf/livesync.conf
%config(noreplace) %{_sysconfdir}/warewulf/defaults/provision.conf
%{_sysconfdir}/warewulf/filesystem/examples/*.cmds
%{_mandir}/*
%{perl_vendorlib}/Warewulf/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Provision.pm
%{perl_vendorlib}/Warewulf/Vnfs.pm
%{perl_vendorlib}/Warewulf/DSO/*
%{perl_vendorlib}/Warewulf/Provision
%{perl_vendorlib}/Warewulf/Event/DynamicHosts.pm
%{perl_vendorlib}/Warewulf/Event/DefaultProvisionNode.pm
%{perl_vendorlib}/Warewulf/Event/ProvisionFileDelete.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Provision.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Vnfs.pm

%files -n %{pname}-initramfs-%{_arch}%{PROJ_DELIM}
%{wwpkgdir}/*

%files -n %{pname}-server%{PROJ_DELIM}
%config(noreplace) %{_sysconfdir}/warewulf/dhcpd-template.conf
%config(noreplace) %{_sysconfdir}/warewulf/dnsmasq-template.conf
%if 0%{?sles_version} || 0%{?suse_version}
%config(noreplace) %{_sysconfdir}/apache2/conf.d/warewulf-httpd.conf
%attr(0750, root, www) %{_libexecdir}/warewulf/cgi-bin/
%else
%config(noreplace) %{_sysconfdir}/httpd/conf.d/warewulf-httpd.conf
%attr(0750, root, apache) %{_libexecdir}/warewulf/cgi-bin/
%endif

%{_bindir}/*
%{perl_vendorlib}/Warewulf/Event/Bootstrap.pm
%{perl_vendorlib}/Warewulf/Event/Dhcp.pm
%{perl_vendorlib}/Warewulf/Event/Pxe.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Pxe.pm
%{perl_vendorlib}/Warewulf/Module/Cli/Dhcp.pm

%if "%{_arch}" == "x86_64" || 0%{?_cross_compile}
%files -n %{pname}-server-ipxe-x86_64%{PROJ_DELIM}
%{_datadir}/warewulf/ipxe/bin-i386-efi
%{_datadir}/warewulf/ipxe/bin-i386-pcbios
%{_datadir}/warewulf/ipxe/bin-x86_64-efi
%endif

%if "%{_arch}" == "aarch64" || 0%{?_cross_compile}
%files -n %{pname}-server-ipxe-aarch64%{PROJ_DELIM}
%{_datadir}/warewulf/ipxe/bin-arm64-efi
%endif

%files -n %{pname}-gpl_sources%{PROJ_DELIM}
%{_prefix}/src/warewulf/3rd_party/GPL/
