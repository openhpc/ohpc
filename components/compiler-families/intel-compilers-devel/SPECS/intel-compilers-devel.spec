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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define pname intel-compilers-devel
%define year 2017

Summary:   OpenHPC compatibility package for Intel(R) Parallel Studio XE
Name:      %{pname}-%{year}%{PROJ_DELIM}
Version:   %{year}
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
Source1:   OHPC_macros
Source2:   OHPC_mod_generator.sh
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq: no
#!BuildIgnore: brp-check-suse

%{!?build_id: %define build_id 210}

BuildRequires:-post-build-checks

Requires: gcc-c++
Requires: intel-compxe-doc >= 2016

Provides: %{pname}%{PROJ_DELIM}
# Provides: libimf.so()(64bit)
# Provides: libsvml.so()(64bit)
# Provides: libiomp5.so()(64bit)
# Provides: libirng.so()(64bit)
# Provides: libifcoremt.so.5()(64bit)
# Provides: libintlc.so.5()(64bit)
# Provides: libifcore.so.5()(64bit)
# Provides: libifport.so.5()(64bit)
# Provides: libiomp5.so(VERSION)(64bit)
# Provides: libmkl_core.so()(64bit)
# Provides: libmkl_sequential.so()(64bit)
# Provides: libmkl_intel_lp64.so()(64bit)
# Provides: libirc.so()(64bit)
# Provides: libmkl_rt.so()(64bit)
# 
%description

Provides OpenHPC-style compatible modules for use with the Intel(R) Parallel
Studio compiler suite.

%prep

%build

%install

install -D -m755 %{SOURCE2}  $RPM_BUILD_ROOT/%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh
%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/intel



%post

topDir=`rpm -q --qf '%{FILENAMES}\n' intel-compxe-doc` || exit 1

echo " "
echo "Scanning top-level dir = $topDir"
if [ -d ${topDir} ];then
    versions=`find -L ${topDir} -maxdepth 1 -type d -name "compilers_and_libraries_????.*" -printf "%f "` || exit 1

    scanner=%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh

    for dir in ${versions}; do
	if [ -e ${topDir}/${dir}/linux/bin/intel64/icc ];then
        version=`${topDir}/${dir}/linux/bin/intel64/icc -V 2>&1 | grep Version | awk -F 'Version' '{print $2}' | awk '{print $1}'`
	    echo "--> Installing OpenHPC-style modulefile for version=${version}"

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
		if [ ! -e ${topDir}/${dir}/linux/bin/compilervars.sh ];then
		    echo "Error: unable to access compilervars.sh (${dir})"
		    exit 1
		fi
		${scanner} ${topDir}/${dir}/linux/bin/compilervars.sh -arch intel64 -platform linux >> %{OHPC_MODULES}/intel/${version} || exit 1

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

setenv        MKLROOT     ${topDir}/${dir}/linux/mkl
prepend-path    LD_LIBRARY_PATH     ${topDir}/${dir}/linux/mkl/lib/intel64

EOF
	fi
    done
fi


%postun
if [ "$1" = 0 ]; then
    find %{OHPC_MODULES}/intel -type f -exec rm {} \;
    find %{OHPC_MODULEDEPS}/gnu/mkl -type f -exec rm {} \;
fi


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_ADMIN}
%{OHPC_MODULES}

%changelog

