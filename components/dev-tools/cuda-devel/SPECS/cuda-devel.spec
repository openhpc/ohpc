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
%define nvhpc_version 25-9
%define dot_version %(tr "-" "." <<< %{nvhpc_version})

Summary:   OpenHPC compatibility package for cuda
Name:      %{pname}%{PROJ_DELIM}
Version:   %{dot_version}
Release:   %{?dist}.1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/dev-tools
BuildArch: x86_64
# These repositories are needed for the driver.
Source1:   https://developer.download.nvidia.com/compute/cuda/repos/rhel10/x86_64/cuda-rhel10.repo
# These repositories are needed for the toolkit. nvhpc install the cuda toolkit
# in /opt in contrast to the normal cuda-toolkit which is installed in /usr/local.
# /opt is easier to manage for NFS exports. nvhpc also comes with modulefiles.
# https://developer.download.nvidia.com/hpc-sdk/rhel/nvhpc.repo
Source2:   nvhpc-rhel.repo

#!BuildIgnore: post-build-checks

Requires: nvhpc-%{nvhpc_version}
Recommends: gcc

%description
Provides OpenHPC-style compatible modules for use with cuda. This package
pulls in nvhpc-%{nvhpc_version}

%package -n cuda-repo%{PROJ_DELIM}
Summary:   Cuda toolkit online repository

%description -n cuda-repo%{PROJ_DELIM}
Installs and configures the online repository for the cuda toolkit


%install
%define repodir %{_sysconfdir}/yum.repos.d
%define cuda_repofile %{SOURCE1}
%define nvhpc_repofile %{SOURCE2}

install -D -m644 %{cuda_repofile} %{nvhpc_repofile} -t %{buildroot}%{repodir}

mkdir -p %{buildroot}/%{OHPC_MODULES}

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/cuda
#%Module1.0#####################################################################

proc ModulesHelp { } { puts stderr "Activate cuda-toolkit (nvhpc)" }

module-whatis "Name: cuda-toolkit (nvhpc)"

# The default compiler from OpenHPC (currently gcc 15) is not supported
# by nvhpc <= 25-9. We rely on the OS compiler for now.
setenv		NVCC_PREPEND_FLAGS	"-ccbin /usr/bin/gcc"
# Make the nvidia hpc_sdk module files visible
append-path	MODULEPATH		/opt/nvidia/hpc_sdk/modulefiles

EOF

%files
%{OHPC_MODULES}/cuda

%files -n cuda-repo%{PROJ_DELIM}
%{repodir}/%{basename:%{cuda_repofile}}
%{repodir}/%{basename:%{nvhpc_repofile}}
