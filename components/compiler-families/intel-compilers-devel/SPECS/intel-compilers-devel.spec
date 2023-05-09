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
%define exact_intel_ver 2023.1.0

Summary:   OpenHPC compatibility package for Intel(R) oneAPI HPC Toolkit
Name:      %{pname}%{PROJ_DELIM}
Version:   2021.1
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
AutoReq:   no
Source1:   oneAPI.repo


#!BuildIgnore: post-build-checks

Requires: gcc libstdc++-devel
Requires(pre): intel-oneapi-dpcpp-cpp-%{exact_intel_ver}
Requires: intel-oneapi-dpcpp-cpp-%{exact_intel_ver}
Requires: intel-oneapi-mkl-devel-%{exact_intel_ver}
Requires: intel-oneapi-compiler-fortran-%{exact_intel_ver}
Recommends: intel-hpckit >= %{exact_intel_ver}

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
install -D -m644 %{SOURCE1} -t %{buildroot}%{repodir}/

# Module directories
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/oneapi
mkdir -p %{buildroot}/%{OHPC_MODULES}/intel
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/mkl
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/intel


%pre -p /bin/bash
if ! [ -f /opt/intel/oneapi/modulefiles-setup.sh ]; then
    echo " "
    echo "Error: Unable to detect the oneAPI installation at /opt/intel."
    echo " "
    exit 1
fi


%post -p /bin/bash
# Do not overwrite existing files
# Write the new file as rpmnew
testfile () {
    if [ -e $1 ]; then
       echo "$1.rpmnew"
    else
       echo "$1"
    fi
}

rm -f %{oneapi_manifest}

# Regenerate the oneAPI modules directory
echo "Generating new oneAPI modulefiles"
/opt/intel/oneapi/modulefiles-setup.sh --ignore-latest --force --output-dir=%{OHPC_MODULEDEPS}/oneapi/ > /dev/null

# Create an OpenHPC module file for each version found in compilers
echo "Creating OpenHPC-style modulefiles for local oneAPI compiler installation(s)."
for compilers in %{OHPC_MODULEDEPS}/oneapi/compiler/2*; do
    ver=$(basename "$compilers")
    echo "--> Installing modulefile for version=${ver}"
    # Do not overwrite existing files
    # Write the new file as rpmnew
    modname=$(testfile %{OHPC_MODULES}/intel/$ver)

    cat << EOF > ${modname}
#%Module1.0#####################################################################

set version "$ver"

proc ModulesHelp { } {
global version
puts stderr "\nThis module loads the oneAPI compiler environment.\n"
puts stderr "\nSee the man pages for icc, icpc, and ifort for detailed information"
puts stderr "on available compiler options and command-line syntax."
puts stderr "\nVersion \$version\n"
}

module-whatis "Name: Intel(R) Compiler"
module-whatis "Version: \$version"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: Intel(R) Compiler Family (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-compilers/"

# update module path hierarchy
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/intel
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/oneapi

# Assume no PAC device; allow override on each node
if { ![info exists ::env(ACL_SKIP_BSP_CONF)] } {
    setenv          ACL_SKIP_BSP_CONF   1
}

module load "compiler/\$version"
module load "mkl"

family "compiler"
EOF

    md5sum ${modname} >> %{oneapi_manifest}

    modname=$(testfile %{OHPC_MODULES}/intel/.version.$ver)


    cat << EOF > ${modname}
#%Module1.0#####################################################################
set     ModulesVersion      "$ver"
EOF

    md5sum ${modname} >> %{oneapi_manifest}

    # Provide standalone module for use with GNU toolchain
    modname=$(testfile  %{OHPC_MODULEDEPS}/gnu/mkl/$ver)

    cat << EOF > ${modname}
#%Module1.0#####################################################################

set version "$ver"

proc ModulesHelp { } {
global version
puts stderr "\nConfigures oneAPI MKL environment\n"
puts stderr "\$version\n"
}

module-whatis "Name: Intel(R) Math Kernel Library"
module-whatis "Version: \$version"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel(R) Math Kernel Library for C/C++ and Fortran"
module-whatis "URL: https://software.intel.com/en-us/en-us/intel-mkl"

prepend-path  MODULEPATH       %{OHPC_MODULEDEPS}/oneapi
module load   "mkl/\$version"
EOF

    md5sum ${modname} >> %{oneapi_manifest}
done


%preun -p /bin/bash
# Check current files against the manifest
# Remove files that match and backup files that don't
if [ -s %{oneapi_manifest} ]; then
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
find %{OHPC_MODULEDEPS}/oneapi -type l -delete
find %{OHPC_MODULEDEPS}/oneapi/* -empty -type d -delete


%files
%dir %{OHPC_MODULES}/intel
%dir %{OHPC_MODULEDEPS}/oneapi
%dir %{OHPC_MODULEDEPS}/intel
%dir %{OHPC_MODULEDEPS}/gnu/mkl
%ghost %{oneapi_manifest}

################################################################################

%package -n intel-oneapi-toolkit-release%{PROJ_DELIM}
Summary:   Intel(R) oneAPI HPC Toolkit Repository Setup

%description -n intel-oneapi-toolkit-release%{PROJ_DELIM}
Installs and configures the online repository for the Intel(R) oneAPI Toolkit.
m

%files -n intel-oneapi-toolkit-release%{PROJ_DELIM}
%{repodir}/%{basename:%{SOURCE1}}
