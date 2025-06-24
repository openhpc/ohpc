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

%define pname intel-compilers-devel
%define keyname GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
%define oneapi_manifest %{OHPC_MODULES}/intel/.rpm-manifest
# Using a minimum version has been problematic as DNF will happily
# install newer versions during build time. If the user has the minimum
# version, but the build system was already using a newer version, then
# the resulting binaries might rely on symbols which are not present
# in the minimum version.  Newer versions may still be installed in parallel.
%define exact_intel_ver 2025.0
%define exact_intel_ver_module 2025.0.1
%define exact_mkl_ver 2025.0
%define exact_deps umf/0.9.1 compiler/2025.0.1 mkl/%{exact_mkl_ver} compiler-rt/2025.0.1 debugger/2025.0.0 tbb/2022.0.0

Summary:   OpenHPC compatibility package for Intel(R) oneAPI HPC Toolkit
Name:      %{pname}%{PROJ_DELIM}
Version:   2025.0
Release:   %{?dist}.1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
AutoReq:   no
Source1:   oneAPI.repo
Source2:   ohpc-update-modules-intel


#!BuildIgnore: post-build-checks

Requires: gcc libstdc++-devel
Requires: intel-oneapi-dpcpp-cpp-%{exact_intel_ver}
Requires: intel-oneapi-mkl-devel-%{exact_mkl_ver}
Requires: intel-oneapi-compiler-fortran-%{exact_intel_ver}
Recommends: intel-hpckit-%{exact_intel_ver}

%description
Provides OpenHPC-style compatible modules for use with the Intel(R) oneAPI
HPC Toolkit.

%package -n intel-oneapi-toolkit-release%{PROJ_DELIM}
Summary:   Intel(R) oneAPI HPC Toolkit Repository Setup

%description -n intel-oneapi-toolkit-release%{PROJ_DELIM}
Installs and configures the online repository for the Intel(R) oneAPI Toolkit.

%install
%if 0%{?suse_version} || 0%{?sle_version}
%global repodir %{_sysconfdir}/zypp/repos.d
%else
%global repodir %{_sysconfdir}/yum.repos.d
%endif

# Install RPM key and yum repo
install -D -m644 %{SOURCE1} -t %{buildroot}%{repodir}/

# Mod generator for oneAPI support
sed -e 's|@@oneapi_manifest@@|%{oneapi_manifest}|' \
    -e 's|@@OHPC_MODULEDEPS@@|%{OHPC_MODULEDEPS}|' \
    -e 's|@@OHPC_MODULES@@|%{OHPC_MODULES}|' \
    -e 's|@@exact_deps@@|%{exact_deps}|' \
    -e 's|@@exact_intel_ver@@|%{exact_intel_ver}|' \
    -e 's|@@exact_intel_ver_module@@|%{exact_intel_ver_module}|' \
    -e 's|@@exact_mkl_ver@@|%{exact_mkl_ver}|' %{SOURCE2} > ohpc-update-modules-intel
install -D -m755 ohpc-update-modules-intel %{buildroot}/%{OHPC_BIN}/ohpc-update-modules-intel

# Module directories
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/oneapi
mkdir -p %{buildroot}/%{OHPC_MODULES}/intel
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/mkl
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/intel


# Regenerate module files when components are added or removed
%transfiletriggerin -- /opt/intel/oneapi
%{OHPC_BIN}/ohpc-update-modules-intel


%pre -p /bin/bash
if ! [ -f /opt/intel/oneapi/modulefiles-setup.sh ]; then
    echo " "
    echo "Error: Unable to detect the oneAPI installation at /opt/intel."
    echo " "
    exit 1
fi


%post -p /bin/bash
%{OHPC_BIN}/ohpc-update-modules-intel

%preun -p /bin/bash
if [ $1 -eq 0 ]; then
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
        # On upgrade, expect lots of modulefiles created as .rpmnew
        echo "WARNING: Manifest not found. Previously generated"
        echo "         modulefiles will not be removed or moved."
    fi
    # Remove the generated oneAPI module links, remove directories if empty
    echo "Removing oneAPI module directory"
    find %{OHPC_MODULEDEPS}/oneapi -type l -delete || true
    find %{OHPC_MODULEDEPS}/oneapi/* -empty -type d -delete || true
fi


%files
%{OHPC_BIN}/ohpc-update-modules-intel
%dir %{OHPC_MODULES}/intel
%dir %{OHPC_MODULEDEPS}/oneapi
%dir %{OHPC_MODULEDEPS}/intel
%dir %{OHPC_MODULEDEPS}/gnu/mkl
%ghost %{oneapi_manifest}

%files -n intel-oneapi-toolkit-release%{PROJ_DELIM}
%{repodir}/%{basename:%{SOURCE1}}
