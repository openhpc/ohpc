#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#-fsp-header-comp-begin----------------------------------------------

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM}
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%endif
%if 0%{FSP_BUILD}
BuildRequires: intel_licenses
%endif

#-fsp-header-comp-end------------------------------------------------

# Base package name
%define pname easybuild
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   Build and installation framework
Name:      EasyBuild%{PROJ_DELIM}
Version:   2.1.1
Release:   1
License:   GPLv2
Group:     System/Configuration
URL:       http://hpcugent.github.com/easybuild
Source0:   %{pname}-easyblocks-%{version}.tar.gz
Source1:   %{pname}-easyconfigs-%{version}.tar.gz
Source2:   %{pname}-framework-%{version}.tar.gz
Source3:   vsc-base-2.2.2.tar.gz
Source4:   bootstrap_eb.py
Source4:   FSP_macros
Source5:   FSP_setup_compiler
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: patch
BuildRequires: python
Requires: lmod
Requires: python

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{pname}/%version

%description
EasyBuild is a software build and installation framework that allows 
you to manage (scientific) software on High Performance Computing (HPC) 
systems in an efficient way.

%prep

%build

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

export EASYBUILD_BOOTSTRAP_SOURCEPATH=.

python ./bootstrap_eb.py %{install_path}

%install

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

make DESTDIR=$RPM_BUILD_ROOT install

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: system tool"
module-whatis "Description: %{summary}"
module-whatis "URL: http://hpcugent.github.com/easybuild/"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib64

setenv          EBROOTEASYBUILD     %{install_path}
setenv          EBVERSIONEASYBUILD  %{version}

prepend-path	PYTHON_LIBRARY_PATH	    %{install_path}/lib/python2.7/site-packages
prepend-path	PYTHON_LIBRARY_PATH	    %{install_path}/lib64/python2.7/site-packages

EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}


%changelog
* Tue Aug  5 2014  <karl.w.schulz@intel.com> - 
- Initial build.

