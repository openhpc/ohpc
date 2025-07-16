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

%define pname intel-mpi-devel
%define gnu_major_ver 14
%define oneapi_manifest %{OHPC_MODULEDEPS}/intel/impi/.rpm-manifest
# Using a minimum version has been problematic as DNF will happily
# install newer versions during build time. If the user has the minimum
# version, but the build system was already using a newer version, then
# the resulting binaries might rely on symbols which are not present
# in the minimum version. Newer versions may still be installed in parallel.
%define exact_mpi_ver 2021.14
%define exact_mkl_ver 2025.0
%define exact_deps compiler/2025.0.0 mkl/%{exact_mkl_ver} compiler-rt/2025.0.0 debugger/2025.0.0 tbb/2022.0.0

Summary:   OpenHPC compatibility package for Intel(R) oneAPI MPI Library
Name:      %{pname}%{PROJ_DELIM}
Version:   2025.0
Release:   %{?dist}.1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
AutoReq:   no
Source1:   ohpc-update-modules-impi

#!BuildIgnore: post-build-checks

Requires: sed
Requires(pre): intel-compilers-devel%{PROJ_DELIM} = %{version}
Requires(pre): intel-oneapi-mpi-devel-%{exact_mpi_ver}
Requires: intel-oneapi-mpi-devel-%{exact_mpi_ver}
Requires: intel-compilers-devel%{PROJ_DELIM} = %{version}
Requires: prun%{PROJ_DELIM}

%description
Provides OpenHPC-style compatible modules for use with the oneAPI
MPI Library.

%install
# Mod generator for oneAPI support
sed -e 's|@@oneapi_manifest@@|%{oneapi_manifest}|' \
    -e 's|@@OHPC_ADMIN@@|%{OHPC_ADMIN}|' \
    -e 's|@@OHPC_MODULEDEPS@@|%{OHPC_MODULEDEPS}|g' \
    -e 's|@@OHPC_MODULES@@|%{OHPC_MODULES}|' \
    -e 's|@@exact_deps@@|%{exact_deps}|' \
    -e 's|@@exact_mpi_ver@@|%{exact_mpi_ver}|' \
    -e 's|@@gnu_major_ver@@|%{gnu_major_ver}|' %{SOURCE1} > ohpc-update-modules-impi
install -D -m755 ohpc-update-modules-impi %{buildroot}/%{OHPC_BIN}/ohpc-update-modules-impi

# Module directories
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi


# Regenerate module files when components are added or removed
%transfiletriggerin -- /opt/intel/oneapi/mpi
%{OHPC_BIN}/ohpc-update-modules-impi


%pre
if ! [ -d /opt/intel/oneapi/mpi/%{exact_mpi_ver}/etc/modulefiles ]; then
    echo " "
    echo "Error: Unable to detect the oneAPI MPI installation at /opt/intel."
    echo " "
    exit 1
fi


%post
%{OHPC_BIN}/ohpc-update-modules-impi

%preun -p /bin/bash
# Check current files against the manifest
# Remove files that match and backup files that don't
if [ -s %{oneapi_manifest} ]; then
    echo "Removing module files"
    while IFS= read -r line; do
       f=${line%:*}
       s=${line#*:}
       if [ "${s: -2}" = "OK" ]; then
           rm -f $f
       elif [ -f $f ]; then
           mv -T -f $f $f.rpmsave
       fi
    done <<< $(md5sum --check %{oneapi_manifest})
else
    # Don't touch any generated files if there's no manifest
    echo "WARNING: Manifest not found. Previously generated"
    echo "         modulefiles will not be removed or moved."
fi


%files
%{OHPC_BIN}/ohpc-update-modules-impi
%dir %{OHPC_MODULEDEPS}/intel/impi
%dir %{OHPC_MODULEDEPS}/gnu/impi
%dir %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi
%ghost %{oneapi_manifest}
