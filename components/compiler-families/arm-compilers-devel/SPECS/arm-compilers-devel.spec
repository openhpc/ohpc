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
%global arm_compt_version arm1
%global pname %{arm_compt_version}-compilers-devel

Summary:   OpenHPC compatibility package for Arm HPC compiler
Name:      %{pname}%{PROJ_DELIM}
Version:   2.4
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: aarch64

#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks

# We need to ensure the installed version is >=21.0
#Requires: arm-linux-compiler-%{latest_installed_ver}-Generic-AArch64-RHEL-8-aarch64-linux >= 21.0
Requires: arm-linux-compiler >= 21.0
Requires: lmod%{PROJ_DELIM}

%description

Provides OpenHPC-style module compatibility for use with the Arm HPC compiler suite.

%install

%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/%{arm_compt_version}

%post

echo "Creating OpenHPC compatibility modulefile for local Arm compiler installation(s)."

# Create a top-level arm/compat module which appends the lmod modulepath to see
# modulefiles provided by Arm installation

latest_installed_ver=$(rpm -qa --queryformat "%%{VERSION} %%{NAME}\\n" | grep " arm-linux-compiler" | sort -rn | cut -d " " -f 1)
major_version=$(echo "${latest_installed_ver}" | cut -d '.' -f 1)

echo "Using latest installed version: ${latest_installed_ver}"
modpath=`rpm -qa | grep -i arm-linux-compiler-${latest_installed_ver} | xargs rpm -ql | grep \/modulefiles$`

if [ ! -n "${modpath}" ];then
    echo ""
    echo "Error: Unable to determine path to modulefiles provided by Arm compiler toolchain"
    exit 1
fi

# path to generic compiler modulename
generic=arm${major_version}/${latest_installed_ver}

# Module header

%{__cat} << EOF > %{OHPC_MODULES}/%{arm_compt_version}/compat
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
prepend-path    MODULEPATH          ${modpath}
# load generic variant
depends-on      \$ARM_GENERIC
EOF


%postun

if [ $1 -eq 0 ];then

    if [ -s %{OHPC_MODULES}/%{arm_compt_version}/compat ];then
	rm -f %{OHPC_MODULES}/%{arm_compt_version}/compat
    fi
fi

%files
%{OHPC_MODULES}

