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

# Base package name
%define pname easybuild

Summary:   Software build and installation framework
Name:      EasyBuild%{PROJ_DELIM}
Version:   4.5.5
Release:   1%{?dist}
License:   GPLv2
Group:     %{PROJ_NAME}/dev-tools
URL:       https://easybuilders.github.io/easybuild

Source0:   https://pypi.io/packages/source/e/easybuild-easyblocks/easybuild-easyblocks-%{version}.tar.gz
Source1:   https://pypi.io/packages/source/e/easybuild-easyconfigs/easybuild-easyconfigs-%{version}.tar.gz
Source2:   https://pypi.io/packages/source/e/easybuild-framework/easybuild-framework-%{version}.tar.gz
Source3:   bootstrap_eb.py
BuildRequires: python3
BuildRequires: python3-setuptools
Requires:  python3
Requires:  patch
#!BuildIgnore: post-build-checks

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency).
%if !0%{?OHPC_BUILD}
BuildRequires: lmod%{PROJ_DELIM}
Requires: lmod%{PROJ_DELIM}
%endif


# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
EasyBuild is a software build and installation framework that allows 
you to manage (scientific) software on High Performance Computing (HPC) 
systems in an efficient way.

%prep
mkdir %{buildroot}

%build

cd %{buildroot}
cp %{_sourcedir}/*py .

export EASYBUILD_BOOTSTRAP_SOURCEPATH=%{_sourcedir}
export EASYBUILD_INSTALLPATH=%{install_path}
export EASYBUILD_MODULE_SYNTAX=Tcl
export PATH=${LMOD_DIR}:${PATH}

MODULEPATH= python3 ./bootstrap_eb.py %{buildroot}/%{install_path}

rm bootstrap_eb.py*
pushd %{buildroot}%{install_path}/modules/tools/EasyBuild/
rm %version
ln -s ../../all/EasyBuild/%version .
popd

# OHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULES}/EasyBuild
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/EasyBuild/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads %{pname}"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: system tool"
module-whatis "Description: %{summary}"
module-whatis "URL: https://easybuilders.github.io/easybuild/"

set             version                 %{version}
set             home                    \$::env(HOME)

prepend-path    PATH                    %{install_path}/software/EasyBuild/%{version}/bin
prepend-path    PATH                    ${LMOD_DIR}
prepend-path	LD_LIBRARY_PATH         %{install_path}/software/EasyBuild/%{version}/lib
prepend-path	LIBRARY_PATH            %{install_path}/software/EasyBuild/%{version}/lib
module          use                     \$home/.local/easybuild/modules/all

setenv          EBROOTEASYBUILD         %{install_path}/software/EasyBuild/%{version}
setenv          EBVERSIONEASYBUILD      %{version}
setenv          EB_PYTHON               python3

prepend-path	PYTHONPATH	    %{install_path}/software/EasyBuild/%{version}/lib/python%{python3_version}/site-packages

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/EasyBuild/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%{OHPC_HOME}
