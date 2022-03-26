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

%define pname hpc-workspace

Name:    %{pname}%{PROJ_DELIM}
Version: 1.4.0
Release: 1%{?dist}
Summary: Temporary workspace management
License: GPL
Source:  https://github.com/holgerBerger/hpc-workspace/archive/refs/tags/%{version}.tar.gz
Source1: ws.conf.template
Group:   %{PROJ_NAME}/admin
URL:     https://github.com/holgerBerger/hpc-workspace

BuildRequires: gcc-c++
BuildRequires: python3
BuildRequires: python3-devel
BuildRequires: boost-system
BuildRequires: boost-filesystem
BuildRequires: boost-regex
BuildRequires: boost-program-options
BuildRequires: yaml-cpp
BuildRequires: python3-pyyaml
BuildRequires: ncurses
BuildRequires: libcap
BuildRequires: lua5.1
BuildRequires: cmake
Requires: python3
Requires: python3-pyyaml
Requires: lua5.1

# Default library install path
%define install_path %{OHPC_ADMIN}/%{pname}/%version


%description
HPC Workspace provides tools to provision temporaty workspaces on scratch or 
working filesystems in an HPC environment.

%pre
# provide specific uid/gid to ensure that it is the same across the cluster
/usr/bin/getent group hpcws >/dev/null 2>&1 || \
  /usr/sbin/groupadd -r hpcws -g 85
/usr/bin/getent passwd hpcws >/dev/null 2>&1 || \
  /usr/sbin/useradd -c "HPC Workspace manager" \
  -d %{_sysconfdir} -g hpcws -s /sbin/nologin -r hpcws -u 85
exit 0

%setup -q -n %{pname}-%{version}

%build
cmake \
  -DCMAKE_INSTALL_PREFIX=%{install_path} \
  -DLUACALLOUTS=TRUE \
  .
make -j 4

%install
make DESTDIR=$RPM_BUILD_ROOT install
cp %{SOURCE1} $RPM_BUILD_ROOT/etc/

%files 
