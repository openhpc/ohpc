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
%define psxe_manifest %{OHPC_MODULES}/intel/.manifest
%define min_intel_ver 2021.4.0


Summary:   OpenHPC compatability package for Intel(R) oneAPI HPC Toolkit
Name:      %{pname}%{PROJ_DELIM}
Version:   2021
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
AutoReq:   no
Source1:   mod_generator.sh
Source2:   oneAPI.repo


#!BuildIgnore: post-build-checks

Requires: gcc libstdc++-devel
Requires(pre): intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic >= %{min_intel_ver}
Requires: intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic >= %{min_intel_ver}
Requires: intel-oneapi-mkl intel-oneapi-mkl-devel
Requires: intel-oneapi-compiler-fortran
Recommends: intel-hpckit >= %{min_intel_vers}

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
install -D -m644 %{SOURCE2} -t %{buildroot}%{repodir}/

# Mod generator for PSXE support
install -D -m755 %{SOURCE1} %{buildroot}/%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh

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


%files -n intel-oneapi-toolkit-release%{PROJ_DELIM}
%{repodir}/%{basename:%{SOURCE2}}

################################################################################

%define psxemod intel-psxe-compilers-devel%{PROJ_DELIM}

%package -n %{psxemod}
Summary: OpenHPC compatibility package for Intel(R) Parallel Studio XE

Obsoletes: %pname%{PROJ_DELIM} < %{version}

%description -n %{psxemod}
Provides OpenHPC-style compatible modules for use with the Intel(R) Parallel
Studio compiler suite.


%pre -n %{psxemod} -p /bin/bash
# Since OpenHPC 2.0 will not upgrade 1.x, there's no need for
# older intel-compilers-devel upgrade patches

# Verify psxe compilers are installed. Punt if not detected.
echo "Checking for local PSXE compiler installation(s)."
icc_subpath="linux/bin/intel64/icc$"
versions_all=$(rpm -qal | grep "${icc_subpath}" | grep -v "oneapi")

if [ $? -eq 1 ];then
    echo " "
    echo "Error: Unable to detect local Parallel Studio installation. The toolchain"
    echo "       providing ${icc_subpath} must be installed prior to this compatibility package"
    echo " "
    exit 1
fi

# Verify min version expectations
min_ver="19.1.0"
declare -a versions=()
declare -a topDirs=()
for file in ${versions_all}; do
    version=$(rpm -q --qf '%%{VERSION}.%%{RELEASE}\n' -f ${file})
    echo "--> Version ${version} detected"
    if [ "$min_ver" = "$(printf '${version}\n${min_ver}' | sort -V | head -n 1)" ]; then
        echo "Warning: ${version} < $min_ver, skipping"
    else
        versions+="${version}"
        topDirs+="$(echo $file | sed "s,/$icc_subpath,,")"
    fi
done

if [ -z "${versions}" ]; then
    echo ""
    echo "Error: local PSXE compatibility support is for versions > ${min_ver}"
    echo " "
    exit 1
fi

mkdir -p %{_localstatedir}/lib/rpm-state/%{name}/
printf "%s\n" ${versions[@]} > %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-versions
printf "%s\n" ${topDirs[@]} > %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-dirs


%post -n %{psxemod}
scanner=%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh
declare -a versions=()
declare -a topDirs=()
readarray -t versions < %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-versions
readarray -t topDirs < %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-dirs
rm -rf %{_localstatedir}/lib/rpm-state/%{name}/

echo "Creating OpenHPC-style modulefiles for local PSXE compiler installation(s)."

# initialize manifest to log files created during this process
rm -f %{psxe_manifest}

# Create modulefiles for each locally detected installation.
for (( x=0; x < ${#versions[@]}; x++ )); do
    topDir=${topDirs[$x]}
    version=${versions[$x]}
    echo "--> Installing modulefile for version=${version}"

    # Check for compilervars before writing a file
    if [ ! -e ${topDir}/linux/bin/compilervars.sh ]; then
	   echo "Error: Unable to access compilervars.sh"
           echo "       Skipping modules for (${topDir})"
	   break
    fi

    # Main module
    cat << EOF > %{OHPC_MODULES}/intel/${version}
#%Module1.0#####################################################################

set version "${version}"

proc ModulesHelp { } {
global version
puts stderr "\nSee the man pages for icc, icpc, and ifort for detailed information"
puts stderr "on available compiler options and command-line syntax."
puts stderr "\nVersion \$version\n"
}

module-whatis "Name: Intel(R) Compiler"
module-whatis "Version: \$version"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: Intel(R) Compiler Family (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-compilers/"

set     version    \$version

# update module path hierarchy
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/intel

family "compiler"
EOF

    echo "%{OHPC_MODULES}/intel/${version}" >> %{psxe_manifest}

    # Append with environment vars parsed directlry from mpivars.sh
    ${scanner} ${topDir}/linux/bin/compilervars.sh -arch intel64 -platform linux >> %{OHPC_MODULES}/intel/${version}
        if [ $? -ne 0 ]; then
           echo "ERROR: Could not generate content for %{OHPC_MODULES}/intel/${version}"
           break
        fi

    # .version file
    cat << EOF > %{OHPC_MODULES}/intel/.version.${version}
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF

    echo "%{OHPC_MODULES}/intel/.version.${version}" >> %{psxe_manifest}

    # Provide standalone module for use with GNU toolchain
    mkdir -p %{OHPC_MODULEDEPS}/gnu/mkl

    cat << EOF > %{OHPC_MODULEDEPS}/gnu/mkl/${version}
#%Module1.0#####################################################################

set version "${version}

proc ModulesHelp { } {
global version
puts stderr "\nSets MKLROOT environment variable\n"
puts stderr "\$version"
}

module-whatis "Name: Intel(R) Math Kernel Library"
module-whatis "Version: \$version"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel Math Kernel Library for C/C++ and Fortran"
module-whatis "URL: https://software.intel.com/en-us/en-us/intel-mkl"

setenv        MKLROOT            ${topDir}/linux/mkl
prepend-path  LD_LIBRARY_PATH    ${topDir}/linux/mkl/lib/intel64
EOF

    echo "%{OHPC_MODULEDEPS}/gnu/mkl/${version}" >> %{psxe_manifest}

done


%preun -n %{psxemod}
if [ -s %{psxe_manifest} ]; then
    while IFS= read -r file; do
        if [ -e $file ]; then
            rm -f $file
        fi
    done < %{psxe_manifest}
else
   echo "WARNING: Manifest not found"
fi


%files -n %{psxemod}
%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh
%dir %{OHPC_MODULES}/intel
%ghost %{psxe_manifest}
