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

# Base package name
%define pname warewulf

%global wwgroup warewulf
%global wwshared /srv

%define debug_package %{nil}

Name: %{pname}%{PROJ_DELIM}
Summary: A provisioning system for large clusters of bare metal and/or virtual systems
Version: 4.2.0
Release: 1
License: BSD-3-Clause
URL:     https://github.com/hpcng/warewulf
Source:  https://github.com/hpcng/warewulf/archive/refs/tags/v%{version}.tar.gz
Patch0:  warewulf-4.2.0-rpm.patch
Patch1:  warewulf-4.2.0-shared_dir.patch

ExclusiveOS: linux

Conflicts: warewulf < 4
Conflicts: warewulf-common warewulf%{PROJ_DELIM}-common
Conflicts: warewulf-cluster warewulf%{PROJ_DELIM}-cluster
Conflicts: warewulf-vnfs warewulf%{PROJ_DELIM}-vnfs
Conflicts: warewulf-provision warewulf%{PROJ_DELIM}-provision
Conflicts: warewulf-ipmi warewulf%{PROJ_DELIM}-ipmi

BuildRequires: make

%if 0%{?rhel}
BuildRequires: systemd
BuildRequires: golang
Requires: tftp-server
%global tftpdir %{_sharedstatedir}/tftpboot
Requires: nfs-utils
%else
# sle_version
BuildRequires: systemd-rpm-macros
BuildRequires: go
Requires: tftp
%global tftpdir /srv/tftpboot
Requires: nfs-kernel-server
%endif

%if 0%{?rhel} >= 8 || 0%{?sle_version}
Requires: dhcp-server
%else
# rhel < 8
Requires: dhcp
%endif

%description
Warewulf is a stateless and diskless container operating system provisioning
system for large clusters of bare metal and/or virtual systems.


%prep -n %{pname}-%{version}
%setup -q -n %{pname}-%{version}
%patch0 -p1
%patch1 -p1


%build
make all
sed -i "s,/srv/tftp,%{tftpdir}," etc/warewulf.conf


%install
%make_install DESTDIR=%{buildroot} %{?mflags_install}
# Move tftp files from Debian default
mkdir -p %{buildroot}/$(dirname %{tftpdir})
mv %{buildroot}/srv/tftp %{buildroot}/%{tftpdir}


%pre
getent group %{wwgroup} >/dev/null || groupadd -r %{wwgroup}


%post
%systemd_post warewulfd.service


%preun
# Remove tftpboot files added during tftp init; requires tftp re-init after upgrade
rm -f %{tftpdir}/%{pname}/ipxe/*
rm -f %{tftpdir}/%{pname}/*
%systemd_preun warewulfd.service


%postun
%systemd_postun_with_restart warewulfd.service


%files
%defattr(-, root, %{wwgroup})
%dir %{_sysconfdir}/%{pname}
%config(noreplace) %{_sysconfdir}/%{pname}/*
%config(noreplace) %attr(0640, -, -) %{_sysconfdir}/%{pname}/nodes.conf
%{_sysconfdir}/bash_completion.d/warewulf

%dir %{tftpdir}/%{pname}
%dir %{tftpdir}/%{pname}/ipxe

%dir %{wwshared}/%{pname}
%{wwshared}/%{pname}/*

%attr(-, root, root) %{_bindir}/wwctl
%if 0%{?rhel}
%attr(-, root, root) %{_prefix}/lib/firewalld/services/warewulf.xml
%else
# sle_version
%attr(-, root, root) %{_libexecdir}/firewalld/services/warewulf.xml
%endif
%attr(-, root, root) %{_unitdir}/warewulfd.service
%attr(-, root, root) %{_mandir}/man1/wwctl*

%changelog
