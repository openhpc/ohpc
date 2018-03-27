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
%global pname arm-compilers-devel

Summary:   OpenHPC compatibility package for Arm HPC compiler
Name:      %{pname}%{PROJ_DELIM}
Version:   1.3.4
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: aarch64

#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks

Requires: lmod%{PROJ_DELIM}

%description

Provides OpenHPC-style module compatibility for use with the Arm HPC compiler suite.

%install

%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/arm


%pre

# Verify Arm HPC compilers are installed. Punt if not detected.

arm_subpath="aarch64-linux/bin/armclang$"

echo "Checking for local Arm HPC compiler installation(s)."

versions_all=`rpm -qal | grep ${arm_subpath}`

if [ $? -eq 1 ];then
    echo ""
    echo "Error: Unable to detect local Arm compiler installation. The toolchain"
    echo "       providing ${arm_subpath} must be installed prior to this compatibility package"
    echo " "
    exit 1
fi

# Verify min version expectations

min_ver="18.3"
versions=""
for file in ${versions_all}; do
    version=`rpm -q --qf '%{VERSION}.%{RELEASE}\n' -f ${file}`
    echo "--> Version ${version} detected"
    echo -e "${version}\n${min_ver}" | sort -V | head -n 1 | grep -q "^${min_ver}"
    if [ $? -ne 0 ];then
        echo "Warning: skipping version ${version}"
    else
        versions="${versions} ${version}"
    fi
done
if [ -z "${versions}" ]; then
    echo ""
    echo "Error: local Arm compiler compatibility support is for versions > ${min_ver}"
    echo " "
    exit 1
fi

%post

arm_subpath="aarch64-linux/bin/armclang$"
packages=`rpm -qal | grep ${arm_subpath}`

if [ $? -eq 1 ];then
    echo ""
    echo "Error: Unable to detect local Arm compiler installation. The toolchain"
    echo "       providing ${arm_subpath} must be installed prior to this compatibility package"
    exit 1
fi

echo "Creating OpenHPC compatibility modulefile for local Arm compiler installation(s)."

# Create a top-level arm/compat module which appends the lmod modulepath to see
# modulefiles provided by Arm installation

package=`echo ${packages} | awk '{print $1}'`
modpath=`rpm -ql -f ${package} | grep modulefiles$`

if [ ! -n "${modpath}" ];then
    echo ""
    echo "Error: Unable to determine path to modulefiles provided by Arm compiler toolchain"
    exit 1
fi

# cache path to generic compiler modulename

generic=`find /opt/arm/modulefiles/Generic-AArch64/ -name arm-hpc-compiler | awk -F '/opt/arm/modulefiles/' '{print $2}'`
if [ ! -n "${generic}" ];then
    echo ""
    echo "Error: Unable to determine path to Generic modulefiles provided by Arm compiler toolchain"
    exit 1
else
    echo "--> Setting generic variant path to: $generic"
fi

# Module header
%{__cat} << EOF > %{OHPC_MODULES}/arm/compat
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "OpenHPC compatibility module that enables access to modulefiles provided"
puts stderr "by separate Arm compiler installation."

}

module-whatis "Name:  Compiler"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: Arm Compiler Family (C/C++/Fortran for aarch64)"
module-whatis "URL: https://developer.arm.com/products/software-development-tools/hpc"

set    ARM_GENERIC ${generic}
setenv ARM_GENERIC \$ARM_GENERIC

# update module path hierarchy
prepend-path    MODULEPATH          ${modpath}:%{OHPC_MODULEDEPS}/arm
# load generic variant
depends-on      \$ARM_GENERIC
family "compiler"
EOF


%postun

if [ $1 -eq 0 ];then

    if [ -s %{OHPC_MODULES}/arm/compat ];then
	rm -f %{OHPC_MODULES}/arm/compat 
    fi
fi

%files
%{OHPC_MODULES}

