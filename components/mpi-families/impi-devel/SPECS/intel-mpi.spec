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

%global pname intel-mpi-devel
%global min_intel_ver 2021.6.0

Summary:   OpenHPC compatibility package for Intel(R) oneAPI MPI Library
Name:      %{pname}%{PROJ_DELIM}
Version:   2022.1
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
AutoReq:   no

#!BuildIgnore: post-build-checks

Requires: intel-oneapi-mpi-devel >= %{min_intel_ver}
Requires: intel-compilers-devel%{PROJ_DELIM} >= 2022
Requires: prun%{PROJ_DELIM}

%description
Provides OpenHPC-style compatible modules for use with the oneAPI
MPI Library.


%prep
%build


%install
# Module directories
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi


%post
# Create an OpenHPC module file for each MPI version found
%{OHPC_ADMIN}/intel/oneapi-modgen.lua update mpi
%{OHPC_ADMIN}/intel/oneapi-modgen.lua update gnu-mpi
%{OHPC_ADMIN}/intel/oneapi-modgen.lua update gnu%{gnu_major_ver}-mpi


%preun -p /bin/bash
# Remove generated mpi modules
%{OHPC_ADMIN}/intel/oneapi-modgen.lua clean gnu%{gnu_major_ver}-mpi
%{OHPC_ADMIN}/intel/oneapi-modgen.lua clean gnu-mpi
%{OHPC_ADMIN}/intel/oneapi-modgen.lua clean mpi


%files
%dir %{OHPC_MODULEDEPS}/intel/impi
%dir %{OHPC_MODULEDEPS}/gnu/impi
%dir %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi
