#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/OHPC_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM}
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
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
Source5:   easybuild-sles12.patch
Source6:   OHPC_macros
Source7:   OHPC_setup_compiler
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: patch
BuildRequires: python
BuildRequires: python-setuptools
Requires: python
#!BuildIgnore: post-build-checks

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{pname}/%version

%description
EasyBuild is a software build and installation framework that allows 
you to manage (scientific) software on High Performance Computing (HPC) 
systems in an efficient way.

%prep
mkdir %{buildroot}

%build

cd %{buildroot}
cp %{_sourcedir}/*py .

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

export EASYBUILD_BOOTSTRAP_SKIP_STAGE0=1
export EASYBUILD_BOOTSTRAP_SOURCEPATH=%{_sourcedir}
export PATH=${LMOD_DIR}:${PATH}

python ./bootstrap_eb.py %{buildroot}/%{install_path}

rm %{buildroot}%{install_path}/modules/base/EasyBuild/2.1.1
rm bootstrap_eb.py
cd %{buildroot}%{install_path}/software
patch -p1 < %{_sourcedir}/easybuild-sles12.patch

%install

# remove buildroot
#for f in $RPM_BUILD_ROOT%{install_path}/conf/*; do
    #sed -i -e 's!%{buildroot}!!g' $f
#done

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULES}/EasyBuild
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/EasyBuild/%{version}
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

set             version                 %{version}
set             home                    \$::env(HOME)

prepend-path    PATH                    %{install_path}/software/EasyBuild/%{version}/bin
prepend-path    PATH                    ${LMOD_DIR}
prepend-path	LD_LIBRARY_PATH         %{install_path}/software/EasyBuild/%{version}/lib
prepend-path	LIBRARY_PATH            %{install_path}/software/EasyBuild/%{version}/lib
module          use                     \$home/.local/easybuild/modules/all

setenv          EBROOTEASYBUILD         %{install_path}/software/EasyBuild/%{version}
setenv          EBVERSIONEASYBUILD      %{version}
setenv          EASYBUILD_MODULES_TOOL  Lmod

prepend-path	PYTHONPATH	    %{install_path}/software/EasyBuild/%{version}/lib/python2.7/site-packages


EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/EasyBuild/.version.%{version}
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

