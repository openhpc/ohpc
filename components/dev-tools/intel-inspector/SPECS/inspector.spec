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

%define pname inspector

Summary:   Intel(R) Inspector XE
Name:      intel-%{pname}%{PROJ_DELIM}
Version:   16.1.2.450824
Source0:   intel-%{pname}%{PROJ_DELIM}-%{version}.tar.gz
Source1:   OHPC_macros
Source2:   modfile-ohpc.input
Release:   1
License:   Copyright (C) 2011-2016 Intel Corporation. All rights reserved.
Vendor:    Intel Corporation
URL:       https://software.intel.com/en-us/intel-parallel-studio-xe
Group:     %{PROJ_NAME}/dev-tools
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no

%if 0%{?sles_version} || 0%{?suse_version}
requires:  libpng12-0
%endif

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%define package_target %{OHPC_PUB}/%{pname}/%{version}

%description

OpenHPC collection of the Intel(R) Inspector memory and thread debugging tools.

%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz %{SOURCE0}
cd -

# OpenHPC module file
%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################
proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the Intel(R) Inspector environment:"
puts stderr " "
puts stderr "Version %{version}"
puts stderr " "

}

module-whatis "Name: Intel(R) Inspector XE"
module-whatis "Version: %{version}"
module-whatis "Category: debug tools"
module-whatis "Description: Intel(R) Inspector memory and thread debugger"
module-whatis "URL: https://software.intel.com/en-us/intel-inspector-xe"

set     version                 %{version}

setenv          INSPECTOR_DIR   %{package_target}
setenv          INSPECTOR_BIN   %{package_target}/bin64
setenv          INSPECTOR_LIB   %{package_target}/lib64
prepend-path    MANPATH         %{package_target}/man 
EOF

# Append with machine-generated contribution for modulefile settings
%{__cat} %{SOURCE2} >> %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%changelog

