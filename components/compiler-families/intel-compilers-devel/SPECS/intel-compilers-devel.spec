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

%global pname intel-compilers-devel
%global keyname GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
%global min_intel_ver 2022.0.0

Summary:   OpenHPC compatibility package for Intel(R) oneAPI HPC Toolkit
Name:      %{pname}%{PROJ_DELIM}
Version:   2022.1
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
AutoReq:   no
Source1:   oneapi-modgen.lua
Source2:   oneAPI.repo


#!BuildIgnore: post-build-checks

Requires: gcc libstdc++-devel
Requires: intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic >= %{min_intel_ver}
Requires: intel-oneapi-mkl intel-oneapi-mkl-devel
Requires: intel-oneapi-compiler-fortran
Recommends: intel-hpckit >= %{min_intel_ver}

%description
Provides OpenHPC-style compatible modules for use with the Intel(R) oneAPI
HPC Toolkit.


%prep
%build


%install
%if 0%{?suse_version} || 0%{?sle_version}
%global repodir %{_sysconfdir}/zypp/repos.d
%else
%global repodir %{_sysconfdir}/yum.repos.d
%endif

# Install RPM key and yum repo
install -D -m644 %{SOURCE2} -t %{buildroot}%{repodir}/%{basename:%{SOURCE2}}

# Mod generator for PSXE support
install -D -m755 %{SOURCE1} %{buildroot}/%{OHPC_ADMIN}/intel/%{basename:%{SOURCE1}}
sed -i '/C["GNU"] =/s/12/%{gnu_major_ver}/' %{buildroot}/%{OHPC_ADMIN}/intel/%{basename:%{SOURCE1}}

# Module directories
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/oneapi
mkdir -p %{buildroot}/%{OHPC_MODULES}/intel
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/mkl
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/intel


%post -p /bin/bash
# Create an OpenHPC module file for each version found in compilers/mkl
%{OHPC_ADMIN}/intel/oneapi-modgen.lua update compiler
%{OHPC_ADMIN}/intel/oneapi-modgen.lua update gnu-mkl


%preun -p /bin/bash
# Remove generated compilers/mkl modules
%{OHPC_ADMIN}/intel/oneapi-modgen.lua clean gnu-mkl
%{OHPC_ADMIN}/intel/oneapi-modgen.lua clean compiler


%files
%dir %{OHPC_MODULES}/intel
%{OHPC_ADMIN}/intel/%{basename:%{SOURCE1}}
%dir %{OHPC_MODULEDEPS}/oneapi
%dir %{OHPC_MODULEDEPS}/intel
%dir %{OHPC_MODULEDEPS}/gnu/mkl


################################################################################

%package -n intel-oneapi-toolkit-release%{PROJ_DELIM}
Summary:   Intel(R) oneAPI HPC Toolkit Repository Setup

%description -n intel-oneapi-toolkit-release%{PROJ_DELIM}
Installs and configures the online repository for the Intel(R) oneAPI Toolkit.


%files -n intel-oneapi-toolkit-release%{PROJ_DELIM}
%{repodir}/%{basename:%{SOURCE2}}
