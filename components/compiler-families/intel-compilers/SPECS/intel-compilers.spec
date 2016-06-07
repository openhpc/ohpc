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

%global pname intel-compilers

Summary:   Intel(R) Parallel Studio XE
Name:      %{pname}%{PROJ_DELIM}
Version:   16.2.181
Release:   1
License:   Intel(R)
URL:       https://software.intel.com/en-us/intel-parallel-studio-xe
Group:     %{PROJ_NAME}/compiler-families
BuildArch: x86_64
Source0:   intel-compilers%{PROJ_DELIM}-16.0.2-181.tar.gz
Source1:   OHPC_macros
Source2:   modfile-ohpc.input
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq: no

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

requires: gcc-c++

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%define composer_release compilers_and_libraries_20%{version}
%define package_target %{OHPC_COMPILERS}/intel

%define module_version 16.0.2.181

%description

OpenHPC collection of runtime packages for Intel(R) Parallel Studio
compiler suite (including compilers for C,C++, and Fortran).


%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz %{SOURCE0}

cd -

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
 
%{__cat} %{SOURCE2} >> %{buildroot}/%{OHPC_MODULES}/intel/%{module_version}

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

export NO_BRP_CHECK_RPATH true

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%changelog

