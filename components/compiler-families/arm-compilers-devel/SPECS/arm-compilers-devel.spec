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
Version:   2.6
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: aarch64

#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks

%define latest_installed_ver 22.1

%if 0%{?rhel} == 8
Requires: arm-linux-compiler-%{latest_installed_ver}-Generic-AArch64-RHEL-8-aarch64-linux
Requires: armpl-%{latest_installed_ver}.0-AArch64-RHEL-8-arm-linux-compiler-aarch64-linux
Requires: gcc-11.2.0-Generic-AArch64-RHEL-8-aarch64-linux
%endif
%if 0%{?sle_version} || 0%{?suse_version}
Requires: arm-linux-compiler-%{latest_installed_ver}-Generic-AArch64-SLES-15-aarch64-linux
Requires: armpl-%{latest_installed_ver}.0-AArch64-SLES-15-arm-linux-compiler-aarch64-linux
Requires: gcc-11.2.0-Generic-AArch64-SLES-15-aarch64-linux
%endif
Requires: lmod%{PROJ_DELIM}

# The package gcc-*.0-Generic-AArch64-SLES-15-aarch64-linux-11 does not
# list all dependencies it has in its list of provides. Let's add them here:
Provides: libstdc++.so.6(GLIBCXX_3.4.26)(64bit)
Provides: libstdc++.so.6(GLIBCXX_3.4.29)(64bit)
Provides: libstdc++.so.6(CXXABI_1.3.13)(64bit)

%description

Provides OpenHPC-style module compatibility for use with the Arm HPC compiler suite.

%install

%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/%{arm_compt_version}

%post

echo "Creating OpenHPC compatibility modulefile for local Arm compiler installation(s)."

# Create a top-level arm/compat module which appends the lmod modulepath to see
# modulefiles provided by Arm installation

latest_installed_ver=$(rpm -qa --queryformat "%%{VERSION} %%{NAME}\\n" | grep " arm-linux-compiler" | sort -rn | cut -d " " -f 1)

echo "Using latest installed version: ${latest_installed_ver}"
modpath=`rpm -qa | grep -i arm-linux-compiler-${latest_installed_ver} | xargs rpm -ql | grep \/modulefiles$`

if [ ! -n "${modpath}" ];then
    echo ""
    echo "Error: Unable to determine path to modulefiles provided by Arm compiler toolchain"
    exit 1
fi

# path to generic compiler modulename
generic=acfl/${latest_installed_ver}

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
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/arm1
# load generic variant
depends-on      \$ARM_GENERIC
# load performance libraries
depends-on      armpl
EOF

%postun
if [ $1 -eq 0 ];then

    if [ -s %{OHPC_MODULES}/%{arm_compt_version}/compat ];then
	rm -f %{OHPC_MODULES}/%{arm_compt_version}/compat
    fi
fi

%files
%{OHPC_MODULES}
