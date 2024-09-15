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

%define __brp_mangle_shebangs_exclude_from easyconfigs

Summary:   Software build and installation framework
Name:      EasyBuild%{PROJ_DELIM}
Version:   4.9.3
Release:   %{?dist}.1
License:   GPLv2
Group:     %{PROJ_NAME}/dev-tools
URL:       https://easybuilders.github.io/easybuild

Source0:   https://pypi.io/packages/source/e/easybuild/easybuild-%{version}.tar.gz
Source1:   https://pypi.io/packages/source/e/easybuild-easyblocks/easybuild_easyblocks-%{version}.tar.gz
Source2:   https://pypi.io/packages/source/e/easybuild-easyconfigs/easybuild_easyconfigs-%{version}.tar.gz
Source3:   https://pypi.io/packages/source/e/easybuild-framework/easybuild_framework-%{version}.tar.gz
BuildRequires: python3-devel python3-pip
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
%setup -q -n %{pname}-%{version} -a1 -a2 -a3

%install
for eb in framework easyblocks easyconfigs; do
	cd %{pname}_${eb}-%{version}
	pip3 install --prefix=%{install_path} --root=%{buildroot} .
	cd ..
done

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

prepend-path    PATH                    %{install_path}/bin
prepend-path    PATH                    ${LMOD_DIR}
module          use                     \$home/.local/easybuild/modules/all

setenv          EBROOTEASYBUILD         %{install_path}
setenv          EBVERSIONEASYBUILD      %{version}
setenv          EB_PYTHON               python3

prepend-path	PYTHONPATH	    %{install_path}/lib/python%{python3_version}/site-packages

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/EasyBuild/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

# Change ambiguous python shebangs into python3

for file in \
	easybuild/easyconfigs/p/PyTorch/PyTorch-check-cpp-extension.py \
	easybuild/scripts/bootstrap_eb.py \
	easybuild/scripts/clean_gists.py \
	easybuild/scripts/findPythonDeps.py \
	easybuild/scripts/fix_docs.py \
	easybuild/scripts/mk_tmpl_easyblock_for.py \
	easybuild/scripts/rpath_args.py; do
	sed -e "s,^#\!/usr/bin/env python,#\!/usr/bin/env python3,g" \
		-i %{buildroot}/%{install_path}/${file}
done

%files
%{OHPC_HOME}
