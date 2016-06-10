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
Source3:   OHPC_mod_generator.sh
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq: no

BuildRequires: intel-ccomp-doc
BuildRequires: intel-ccompxe-doc
BuildRequires: intel-comp-all-doc
BuildRequires: intel-comp-l-all-common-%{build_id}
BuildRequires: intel-comp-l-all-devel-%{build_id}
BuildRequires: intel-comp-l-all-vars-%{build_id}
BuildRequires: intel-comp-l-ps-common-%{build_id}
BuildRequires: intel-comp-l-ps-devel-%{build_id}
BuildRequires: intel-comp-l-ps-ss-devel-%{build_id}
BuildRequires: intel-comp-ps-doc-jp
BuildRequires: intel-comp-ps-ss-doc
BuildRequires: intel-compxe
BuildRequires: intel-compxe-doc
BuildRequires: intel-daal-%{build_id}
BuildRequires: intel-daal-common-%{build_id}
BuildRequires: intel-daal-doc
BuildRequires: intel-daal-ps-common-jp-%{build_id}
BuildRequires: intel-daal-ps-doc-jp
BuildRequires: intel-fcomp-doc
BuildRequires: intel-fcompxe-doc
BuildRequires: intel-gdb
BuildRequires: intel-gdb-common
BuildRequires: intel-gdb-doc
BuildRequires: intel-gdb-doc-jp
BuildRequires: intel-gdb-gt
BuildRequires: intel-gdb-gt-common
BuildRequires: intel-gdb-gt-devel
BuildRequires: intel-gdb-gt-doc
BuildRequires: intel-gdb-gt-libelfdwarf
BuildRequires: intel-gdb-gt-src
BuildRequires: intel-gdb-ps-cdt
BuildRequires: intel-gdb-ps-cdt-source
BuildRequires: intel-gdb-ps-common
BuildRequires: intel-gdb-ps-doc
BuildRequires: intel-gdb-ps-doc-jp
BuildRequires: intel-gdb-ps-mic
BuildRequires: intel-gdb-ps-mpm
BuildRequires: intel-gdb-python-source
BuildRequires: intel-gdb-source
BuildRequires: intel-icc-doc
BuildRequires: intel-icc-l-all-%{build_id}
BuildRequires: intel-icc-l-all-common-%{build_id}
BuildRequires: intel-icc-l-all-devel-%{build_id}
BuildRequires: intel-icc-l-all-vars-%{build_id}
BuildRequires: intel-icc-l-ps-common-%{build_id}
BuildRequires: intel-icc-l-ps-devel-%{build_id}
BuildRequires: intel-icc-l-ps-ss-%{build_id}
BuildRequires: intel-icc-l-ps-ss-devel-%{build_id}
BuildRequires: intel-icc-ps-doc
BuildRequires: intel-icc-ps-doc-jp
BuildRequires: intel-icc-ps-ss-doc
BuildRequires: intel-icsxe-pset
BuildRequires: intel-ifort-l-ps-%{build_id}
BuildRequires: intel-ifort-l-ps-common-%{build_id}
BuildRequires: intel-ifort-l-ps-devel-%{build_id}
BuildRequires: intel-ifort-l-ps-jp-%{build_id}
BuildRequires: intel-ifort-l-ps-vars-%{build_id}
BuildRequires: intel-ifort-ps-doc
BuildRequires: intel-ifort-ps-doc-jp
BuildRequires: intel-ipp-l-common-%{build_id}
BuildRequires: intel-ipp-l-doc
BuildRequires: intel-ipp-l-mt-%{build_id}
BuildRequires: intel-ipp-l-mt-devel-%{build_id}
BuildRequires: intel-ipp-l-ps-common-%{build_id}
BuildRequires: intel-ipp-l-ps-doc-jp
BuildRequires: intel-ipp-l-ps-st-devel-%{build_id}
BuildRequires: intel-ipp-l-st-%{build_id}
BuildRequires: intel-ipp-l-st-devel-%{build_id}
BuildRequires: intel-mkl-%{build_id}
BuildRequires: intel-mkl-common-%{build_id}
BuildRequires: intel-mkl-devel-%{build_id}
BuildRequires: intel-mkl-doc
BuildRequires: intel-mkl-gnu-%{build_id}
BuildRequires: intel-mkl-gnu-devel-%{build_id}
BuildRequires: intel-mkl-ps-%{build_id}
BuildRequires: intel-mkl-ps-cluster-%{build_id}
BuildRequires: intel-mkl-ps-cluster-common-%{build_id}
BuildRequires: intel-mkl-ps-cluster-devel-%{build_id}
BuildRequires: intel-mkl-ps-common-%{build_id}
BuildRequires: intel-mkl-ps-common-jp-%{build_id}
BuildRequires: intel-mkl-ps-doc
BuildRequires: intel-mkl-ps-doc-jp
BuildRequires: intel-mkl-ps-f95-common-%{build_id}
BuildRequires: intel-mkl-ps-f95-devel-%{build_id}
BuildRequires: intel-mkl-ps-gnu-%{build_id}
BuildRequires: intel-mkl-ps-gnu-devel-%{build_id}
BuildRequires: intel-mkl-ps-jp-%{build_id}
BuildRequires: intel-mkl-ps-mic-%{build_id}
BuildRequires: intel-mkl-ps-mic-devel-%{build_id}
BuildRequires: intel-mkl-ps-mic-jp-%{build_id}
BuildRequires: intel-mkl-ps-pgi-%{build_id}
BuildRequires: intel-mkl-ps-pgi-devel-%{build_id}
BuildRequires: intel-mkl-ps-ss-tbb-%{build_id}
BuildRequires: intel-mkl-ps-ss-tbb-devel-%{build_id}
BuildRequires: intel-mkl-ps-tbb-mic-%{build_id}
BuildRequires: intel-mkl-ps-tbb-mic-devel-%{build_id}
BuildRequires: intel-mkl-sp2dp-%{build_id}
BuildRequires: intel-mkl-sp2dp-devel-%{build_id}
BuildRequires: intel-openmp-l-all-%{build_id}
BuildRequires: intel-openmp-l-all-devel-%{build_id}
BuildRequires: intel-openmp-l-ps-%{build_id}
BuildRequires: intel-openmp-l-ps-devel-%{build_id}
BuildRequires: intel-openmp-l-ps-devel-jp-%{build_id}
BuildRequires: intel-openmp-l-ps-mic-%{build_id}
BuildRequires: intel-openmp-l-ps-mic-devel-%{build_id}
BuildRequires: intel-openmp-l-ps-mic-devel-jp-%{build_id}
BuildRequires: intel-openmp-l-ps-ss-%{build_id}
BuildRequires: intel-openmp-l-ps-ss-devel-%{build_id}
BuildRequires: intel-psxe-common
BuildRequires: intel-psxe-doc
BuildRequires: intel-tbb-common-%{build_id}
BuildRequires: intel-tbb-common-jp-%{build_id}
BuildRequires: intel-tbb-devel-%{build_id}
BuildRequires: intel-tbb-doc
BuildRequires: intel-tbb-doc-jp
BuildRequires: intel-tbb-libs-%{build_id}
BuildRequires: intel-tbb-ps-common-%{build_id}

##BuildRequires: intel-comp-l-all-vars-%{build_id}
##BuildRequires: intel-openmp-l-all-devel-%{build_id}
##BuildRequires: intel-openmp-l-ps-devel-%{build_id}
##BuildRequires: intel-icc-l-all-%{build_id}
##BuildRequires: intel-comp-l-all-devel-%{build_id}
##BuildRequires: intel-comp-l-ps-devel-%{build_id}
##BuildRequires: intel-mkl-%{build_id}
##BuildRequires: intel-mkl-common-%{build_id}
##BuildRequires: intel-daal-common-%{build_id}
##BuildRequires: intel-tbb-devel-%{build_id}
##BuildRequires: intel-ipp-l-common-%{build_id}
##BuildRequires: intel-gdb-common

Requires: gcc-c++
Requires: intel-icc-l-all-%{build_id}
Requires: intel-ifort-l-ps-devel-%{build_id}
Requires: intel-mkl-%{build_id}

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
%{__chmod} +x %{SOURCE3}
%{SOURCE3} %{package_target}/%{composer_release}/linux/bin/compilervars.sh -arch intel64 -platform linux > modfile-ohpc.input

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

