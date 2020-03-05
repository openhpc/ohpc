#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%global ohpc_bootstrap 1
%include %{_sourcedir}/OHPC_macros

%if 0%{?rhel}
%define disttag .el8
%endif

%if 0%{?suse_version}
%define disttag .leap15
%endif

Summary:  OpenHPC release files
Name:     ohpc-release
Version:  2
Release:  1%{?disttag}
License:  Apache-2.0
Group:    %{PROJ_NAME}/admin
URL:      https://github.com/openhpc/ohpc
Source1:  RPM-GPG-KEY-OpenHPC-2

Provides: ohpc-release = %{version}


%if 0%{?rhel}
Requires: epel-release
Requires: redhat-release >= 8.1
%endif
%if 0%{?suse_version}
Requires: suse-release >= 15.1
%endif

%description

Collection of OpenHPC release files including package repository definition.

%prep

%build

%install

%{__mkdir} ${RPM_BUILD_ROOT}/etc

# /etc/ohpc-release

cat >> ${RPM_BUILD_ROOT}/etc/ohpc-release <<EOF
OpenHPC release %{version} (%{_repository})
HOME_URL="http://openhpc.community"
BUG_REPORT_URL="https://github.com/openhpc/ohpc/issues"
EOF

# package repository definitions

%if 0%{?suse_version}
%define __repodir /etc/zypp/repos.d
%else
%define __repodir /etc/yum.repos.d
%endif

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{__repodir}

cat >> ${RPM_BUILD_ROOT}/%{__repodir}/OpenHPC.repo <<EOF
[OpenHPC]
name=OpenHPC-%{ohpc_version} - Base
baseurl=%{ohpc_repo}/OpenHPC/%{ohpc_version}/%{_repository}
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-OpenHPC-2

[OpenHPC-updates]
name=OpenHPC-%{ohpc_version} - Updates
baseurl=%{ohpc_repo}/OpenHPC/%{ohpc_version}/updates/%{_repository}
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-OpenHPC-2

[OpenHPC-Sources]
name=OpenHPC-%{ohpc_version} - Sources
baseurl=%{ohpc_repo}/OpenHPC/%{ohpc_version}/sources/%{_repository}
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-OpenHPC-2
enabled=0
EOF

# repository GPG key

install -D -m 0644 %SOURCE1 ${RPM_BUILD_ROOT}/etc/pki/rpm-gpg/RPM-GPG-KEY-OpenHPC-2

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%config /etc/ohpc-release

%if 0%{?suse_version}
%dir /etc/zypp
%endif

%{__repodir}
/etc/pki
