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

%{!?major_ver:  %define major_ver 16}
%{!?update_num: %define update_num 3}
%{!?build_id:   %define build_id 210}

Summary:   Intel(R) Parallel Studio XE compatability package for OpenHPC
Name:      %{pname}%{PROJ_DELIM}
Version:   %{major_ver}.%{update_num}.%{build_id}
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
Source1:   OHPC_macros
#Source2:   modfile-ohpc.input
Source3:   OHPC_mod_generator.sh
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq: no

BuildRequires: intel-icc-l-all-%{build_id}

Requires: gcc-c++
Requires: intel-icc-l-all-%{build_id}
Requires: intel-ifort-l-ps-devel-%{build_id}

%define composer_release compilers_and_libraries_20%{version}
%define package_target /opt/intel

%define package_version %{version}
%define module_version %{major_ver}.0.%{update_num}.%{build_id}

%description

Provides OpenHPC-style compatible modules for use with the Intel(R) Parallel
Studio compiler suite.

%prep

%build

%install

# Parse provided shell script to derive appropriate module settings
%{SOURCE3} %{package_target}/%{compiler_release}/linux/bin/compilervars.sh -arch intel64 -platform linux > modfile-ohpc.input

# OpenHPC module file
%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/intel
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/intel/%{module_version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "See the man pages for icc, icpc, and ifort for detailed information"
puts stderr "on available compiler options and command-line syntax."

puts stderr "\nVersion %{module_version}\n"

}

module-whatis "Name: Intel Compiler"
module-whatis "Version: %{version}"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: Intel Compiler Family (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-compilers/"

set     version			    %{version}

# update module path hierarchy
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/intel

family "compiler"
EOF

# Append machine-generated module settings
 
%{__cat} modfile-ohpc.input >> %{buildroot}/%{OHPC_MODULES}/intel/%{module_version}

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/intel/.version.%{module_version}
#%Module1.0#####################################################################
set     ModulesVersion      "%{module_version}"
EOF

# Provide standalone module for use with GNU toolchain

%{__mkdir} -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/mkl
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/gnu/mkl/%{module_version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "Sets MKLROOT environment variable"
puts stderr " "
puts stderr "%{module_version}"

}

module-whatis "Name: Intel Math Kernel Library"
module-whatis "Version: %{version}"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel Math Kernel Library for C/C++ and Fortran"
module-whatis "URL: https://software.intel.com/en-us/en-us/intel-mkl"

setenv	        MKLROOT 	    %{package_target}/compilers_and_libraries_20%{version}/linux/mkl
prepend-path    LD_LIBRARY_PATH     %{package_target}/compilers_and_libraries_20%{version}/linux/mkl/lib/intel64

EOF


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%changelog

