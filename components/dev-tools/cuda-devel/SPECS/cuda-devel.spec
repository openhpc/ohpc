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

%define pname cuda-devel
%define cuda_version 12-8
%define dot_version %(tr "-" "." <<< %{cuda_version})

Summary:   OpenHPC compatibility package for cuda
Name:      %{pname}%{PROJ_DELIM}
Version:   %{dot_version}
Release:   %{?dist}.1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/dev-tools
BuildArch: x86_64
Source1:   https://developer.download.nvidia.com/compute/cuda/repos/rhel9/x86_64/cuda-rhel9.repo
Source2:   https://developer.download.nvidia.com/compute/cuda/repos/opensuse15/x86_64/cuda-opensuse15.repo

#!BuildIgnore: post-build-checks

Requires: cuda-toolkit-%{cuda_version}

%description
Provides OpenHPC-style compatible modules for use with cuda.

%package -n cuda-repo%{PROJ_DELIM}
Summary:   Cuda toolkit online repository

%description -n cuda-repo%{PROJ_DELIM}
Installs and configures the online repository for the cuda toolkit


%install
%if 0%{?suse_version}
%define repodir %{_sysconfdir}/zypp/repos.d
%define repofile %{SOURCE2}
%else
%define repodir %{_sysconfdir}/yum.repos.d
%define repofile %{SOURCE1}
%endif

install -D -m644 %{repofile} -t %{buildroot}%{repodir}

mkdir -p %{buildroot}/%{OHPC_MODULES}/cuda

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/cuda/%{dot_version}
#%Module1.0#####################################################################

proc ModulesHelp { } { puts stderr "Activate cuda-toolkit %{dot_version}" }

module-whatis "Name: cuda-toolkit"
module-whatis "Version: %{dot_version}"

set		version		%{dot_version}
set		root		/usr/local/cuda-%{dot_version}
setenv		CUDA_HOME	\$root
prepend-path	INCLUDE		\$root/include
setenv		CUDA_INC	\$root/include
setenv		CUDA_LIB	\$root/lib64

prepend-path	PATH		\$root/bin
prepend-path	LD_LIBRARY_PATH	\$root/lib64
prepend-path	MAN_PATH	\$root/man

EOF


%files
%{OHPC_MODULES}/cuda

%files -n cuda-repo%{PROJ_DELIM}
%{repodir}/%{basename:%{repofile}}
