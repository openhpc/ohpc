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
%global pname arm1-compilers-devel

Summary:   OpenHPC compatibility package for Arm HPC compiler
Name:      %{pname}%{PROJ_DELIM}
Version:   2.0
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: aarch64

#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks

Requires: arm-compiler-for-linux >= 20.2.1
Requires: lmod%{PROJ_DELIM}

%description

Provides OpenHPC-style module compatibility for use with the Arm HPC compiler suite.

%install

%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/arm1

%post

echo "Creating OpenHPC compatibility modulefile for local Arm compiler installation(s)."

# Create a top-level arm/compat module which appends the lmod modulepath to see
# modulefiles provided by Arm installation

modpath=`rpm -ql arm-compiler-for-linux | grep modulefiles$`

if [ ! -n "${modpath}" ];then
    echo ""
    echo "Error: Unable to determine path to modulefiles provided by Arm compiler toolchain"
    exit 1
fi

# cache path to generic compiler modulename
generic=`find $modpath/Generic-AArch64/ -name arm-linux-compiler | awk -F "$modpath/" '{print $2}'`
if [ ! -n "${generic}" ];then
    echo ""
    echo "Error: Unable to determine path to Generic modulefiles provided by Arm compiler toolchain"
    exit 1
else
    echo "--> Setting generic variant path to: $generic"
fi

# cache path to generic armpl modulename
armpl_generic=`find $modpath/Generic-AArch64/ -name armpl | grep arm-linux-compiler | awk -F "$modpath/" '{print $2}'`
if [ ! -n "${generic}" ];then
    echo ""
    echo "Error: Unable to determine path to Generic Performance Library modulefile provided by Arm compiler toolchain"
    exit 1
else
    echo "--> Setting ARM PL generic variant to: $armpl_generic"
fi

# Module header
%{__cat} << EOF > %{OHPC_MODULES}/arm1/compat
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
prepend-path    MODULEPATH          ${modpath}:%{OHPC_MODULEDEPS}/arm1
# load generic variant
depends-on      \$ARM_GENERIC
depends-on      ${armpl_generic}
family "compiler"
EOF


%postun

if [ $1 -eq 0 ];then

    if [ -s %{OHPC_MODULES}/arm1/compat ];then
	rm -f %{OHPC_MODULES}/arm1/compat
    fi
fi

%files
%{OHPC_MODULES}

