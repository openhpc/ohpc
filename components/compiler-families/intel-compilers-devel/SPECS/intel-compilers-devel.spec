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
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

%define pname intel-compilers-devel
%define year 2017

Summary:   OpenHPC compatibility package for Intel(R) Parallel Studio XE
Name:      %{pname}%{PROJ_DELIM}
Version:   %{year}
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
Source1:   OHPC_macros
Source2:   OHPC_mod_generator.sh
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks

Requires: gcc-c++
Requires: grep

Provides: %{pname}%{PROJ_DELIM}

%description

Provides OpenHPC-style compatible modules for use with the Intel(R) Parallel
Studio compiler suite.

%prep

%build

%install

install -D -m755 %{SOURCE2}  $RPM_BUILD_ROOT/%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh
%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/intel



%pre

# Verify psxe compilers are installed. Punt if not detected.

icc_subpath="linux/bin/intel64/icc$"

echo "Checking for local PSXE compiler installation(s)."

versions_all=`rpm -qal | grep ${icc_subpath}`

if [ $? -eq 1 ];then
    echo ""
    echo "Error: Unable to detect local Parallel Studio installation. The toolchain"
    echo "       providing ${icc_subpath} must be installed prior to this compatability package"
    echo " "
    exit 1
fi

# Verify min version expectations

min_ver="16.0"
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
    echo "Error: local PSXE compatability support is for versions > ${min_ver}"
    echo " "
    exit 1
fi

%post

icc_subpath="linux/bin/intel64/icc"
scanner=%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh

versions=`rpm -qal | grep ${icc_subpath}$`

if [ $? -eq 1 ];then
    echo ""
    echo "Error: Unable to detect local Parallel Studio installation. The toolchain"
    echo "       providing ${icc_subpath} must be installed prior to this compatability package"
    exit 1
fi

echo "Creating OpenHPC-style modulefiles for local PSXE compiler installation(s)."

# Create modulefiles for each locally detected installation.

for file in ${versions}; do
    version=`rpm -q --qf '%{VERSION}.%{RELEASE}\n' -f ${file}`
    topDir=`echo $file | sed "s|$icc_subpath||"`
    echo "--> Installing modulefile for version=${version}"

    # Module header
    %{__cat} << EOF > %{OHPC_MODULES}/intel/${version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "See the man pages for icc, icpc, and ifort for detailed information"
puts stderr "on available compiler options and command-line syntax."

puts stderr "\nVersion ${version}\n"

}

module-whatis "Name: Intel Compiler"
module-whatis "Version: ${version}"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: Intel Compiler Family (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-compilers/"

set     version    ${version}

# update module path hierarchy
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/intel

family "compiler"
EOF
    # Append with environment vars parsed directly from compilervars.sh
    if [ ! -e ${topDir}/linux/bin/compilervars.sh ];then
	echo "Error: unable to access compilervars.sh (${topDir})"
	exit 1
    fi
    ${scanner} ${topDir}/linux/bin/compilervars.sh -arch intel64 -platform linux >> %{OHPC_MODULES}/intel/${version} || exit 1

    # .version file
    %{__cat} << EOF > /%{OHPC_MODULES}/intel/.version.${version}
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF

    # Provide standalone module for use with GNU toolchain

    %{__mkdir} -p %{OHPC_MODULEDEPS}/gnu/mkl
    %{__cat} << EOF > %{OHPC_MODULEDEPS}/gnu/mkl/${version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "Sets MKLROOT environment variable"
puts stderr " "
puts stderr "${version}"

}

module-whatis "Name: Intel Math Kernel Library"
module-whatis "Version: ${version}"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel Math Kernel Library for C/C++ and Fortran"
module-whatis "URL: https://software.intel.com/en-us/en-us/intel-mkl"

setenv        MKLROOT            ${topDir}/linux/mkl
prepend-path  LD_LIBRARY_PATH    ${topDir}/linux/mkl/lib/intel64

EOF

    # Inventory for later removal
    echo "%{OHPC_MODULES}/intel/${version}" >> %{OHPC_MODULES}/intel/.manifest
    echo "%{OHPC_MODULES}/intel/.version.${version}" >> %{OHPC_MODULES}/intel/.manifest
    echo "%{OHPC_MODULEDEPS}/gnu/mkl/${version}" >> %{OHPC_MODULES}/intel/.manifest

done

%postun

if [ -s %{OHPC_MODULES}/intel/.manifest ];then
    for file in `cat %{OHPC_MODULES}/intel/.manifest`; do
        rm $file
    done
    rm -f %{OHPC_MODULES}/intel/.manifest
fi

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_ADMIN}
%{OHPC_MODULES}

%changelog

