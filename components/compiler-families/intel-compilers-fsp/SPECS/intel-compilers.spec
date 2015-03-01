%include %{_sourcedir}/FSP_macros

%define pname intel-compilers
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   Intel(R) Parallel Studio XE
Name:      %{pname}%{PROJ_DELIM}
Version:   15.2.164
Release:   1
License:   Intel(R)
URL:       http://www.intel.com/software/products
Group:     fsp/compiler-families
BuildArch: x86_64
Source0:   intel-compilers-fsp-20%{version}.tar.gz
Source1:   FSP_macros
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReqProv: no
AutoReq: no

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

requires: gcc-c++

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%define composer_release composer_xe_20%{version}
%define package_target %{FSP_COMPILERS}/intel/%{composer_release}

%define package_version {%version}

%description

FSP collection of the Intel Composer compilers for C,C++, and Fortran.

%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz %{SOURCE0}
cd -

# FSP module file
%{__mkdir} -p %{buildroot}/%{FSP_MODULES}/intel
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/intel/%{package_version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "See the man pages for icc, icpc, and ifort for detailed information"
puts stderr "on available compiler options and command-line syntax."
puts stderr " "
puts stderr "See the man pages for idb or idbc for more information on using the"
puts stderr "Intel debugger."

puts stderr "\nVersion %{package_version}\n"

}

module-whatis "Name: Intel Compiler"
module-whatis "Version: %{version}"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: Intel Compiler Family (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-compilers/"

set     version			    %{version}

setenv	        MKLROOT 	    %{package_target}/mkl
prepend-path    PATH                %{package_target}/bin/intel64:%{package_target}/mpirt/bin/intel64
prepend-path    MANPATH             %{package_target}/man/en_US
prepend-path	LIBRARY_PATH	    %{package_target}/compiler/lib/intel64
prepend-path 	LD_LIBRARY_PATH     %{package_target}/compiler/lib/intel64:%{package_target}/compiler/mpirt/lib/intel64:%{package_target}/mkl/lib/intel64
prepend-path	MIC_LD_LIBRARY_PATH %{package_target}/compiler/lib/mic:%{package_target}/mpirt/lib/mic
prepend-path    MODULEPATH          %{FSP_MODULEDEPS}/intel

# debugger related

prepend-path    MANPATH             %{package_target}/debugger/gdb/intel64/share/man/:%{package_target}/debugger/gdb/intel64_mic/share/man
prepend-path    PATH                %{package_target}/debugger/gdb/intel64_mic/bin
setenv          GDBSERVER_MIC       %{package_target}/debugger/gdb/target/mic/bin/gdbserver
setenv          GDB_CROSS           %{package_target}/debugger/gdb/intel64_mic/bin/gdb-mic
setenv          INTEL_PYTHONHOME    %{package_target}/debugger/python/intel64
setenv          MPM_LAUNCHER        %{package_target}/debugger/mpm/bin/start_mpm.sh

family "compiler"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/intel/.version.%{package_version}
#%Module1.0#####################################################################
set     ModulesVersion      "%{package_version}"
EOF

# Provide standalone module for use with GNU toolchain

%{__mkdir} -p %{buildroot}/%{FSP_MODULEDEPS}/gnu/mkl
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/gnu/mkl/%{package_version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "Sets MKLROOT environment variable"
puts stderr " "
puts stderr "%{package_version}"

}

module-whatis "Name: Intel Math Kernel Library"
module-whatis "Version: %{version}"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel Math Kernel Library for C/C++ and Fortran"
module-whatis "URL: https://software.intel.com/en-us/en-us/intel-mkl"

setenv	        MKLROOT 	    %{package_target}/mkl
prepend-path    INCLUDE             %{package_target}/mkl/include
prepend-path    CPATH               %{package_target}/mkl/include
prepend-path    LD_LIBRARY_PATH     %{package_target}/mkl/lib/intel64

EOF

export NO_BRP_CHECK_RPATH true

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

