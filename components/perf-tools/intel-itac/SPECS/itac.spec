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

%define pname itac

Summary:   Intel(R) Trace Analyzer and Collector
Name:      intel-%{pname}%{PROJ_DELIM}
Version:   9.1.2.024
Source0:   intel-%{pname}%{PROJ_DELIM}-%{version}.tar.gz
Source1:   OHPC_macros
#Source2:   OHPC_mod_generator.sh
Source2:   modfile-ohpc.input
Release:   1
License:   Copyright (C) 2003-2016 Intel Corporation. All rights reserved.
Vendor:    Intel Corporation
URL:       https://software.intel.com/en-us/intel-parallel-studio-xe
Group:     %{PROJ_NAME}/perf-tools
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no
#AutoReqProv: no

%if 0%{?rhel_version} > 600 || 0%{?centos_version} > 600
Requires: libpng12
%endif

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%define package_target %{OHPC_PUB}/%{pname}/%{version}

%description

OpenHPC collection of the Intel(R) Trace Analyzer and Collector for Linux* OS.

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
puts stderr "This module loads the Intel(R) Trace Analyzer and Collector environment:"
puts stderr " "
puts stderr "Version %{version}"
puts stderr " "

}

module-whatis "Name: Intel Trace Analyzer and Collector"
module-whatis "Version: %{version}"
module-whatis "Category: performance tools"
module-whatis "Description: Intel(R) Trace Analyzer and Collector"
module-whatis "URL: https://software.intel.com/en-us/intel-trace-analyzer"

set     version                 %{version}

setenv          ITAC_DIR        %{package_target}
setenv          ITAC_BIN        %{package_target}/bin
setenv          ITAC_LIB        %{package_target}/lib

EOF

# Parse shell script to derive module settings
 
%{__cat} %{SOURCE2} >> %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}


# Parse shell script to derive module settings

# %%{__chmod} 700 %{_sourcedir}/OHPC_mod_generator.sh 
# %%{_sourcedir}/OHPC_mod_generator.sh %{buildroot}/%{package_target}/intel64/bin/itacvars.sh >> %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}

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

