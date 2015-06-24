#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%define compiler_family intel
%{!?PROJ_DELIM: %define PROJ_DELIM %{nil}}

Summary:   Intel(R) VTune(TM) Amplifier XE 2015 Update 2
Name:      intel-vtune%{PROJ_DELIM}
Version:   15.2.0.393444
Source0:   intel-vtune-amplifier-fsp-%{version}.tar.gz
Source1:   FSP_macros
Release:   1
License:   Copyright (C) 2011-2014 Intel Corporation. All rights reserved.
Vendor:    Intel Corporation
URL:       http://www.intel.com/software/products/
Group:     fsp/perf-tools
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no
#AutoReqProv: no

%include %{_sourcedir}/FSP_macros

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%define package_target /opt/fsp/pub/vtune_amplifier/%{version}

%description

FSP collection of the Intel(R) VTune(TM) distribution.

%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz %{SOURCE0}
cd -

# FSP module file for Intel Vtune
%{__mkdir} -p %{buildroot}/%{FSP_MODULES}/vtune
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/vtune/%{version}
#%Module1.0#####################################################################
proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the Intel(R) Vtune(TM) Amplifier environment:"
puts stderr "   amplxe-cl  --> command-line tool
puts stderr "   amplxe-gui --> GUI-based interface
puts stderr " "
puts stderr "Version %{version}"
puts stderr " "

}

module-whatis "Name: Intel Vtune"
module-whatis "Version: %{version}"
module-whatis "Category: performance tools"
module-whatis "Description: Intel(R) VTune(TM) Amplifier"
module-whatis "URL: https://software.intel.com/en-us/intel-vtune-amplifier-xe"

set     version                 %{version}

prepend-path    VTUNE_DIR       %{package_target}
prepend-path    VTUNE_BIN       %{package_target}/bin64
prepend-path    VTUNE_LIB       %{package_target}/lib64
prepend-path    PATH            %{package_target}/bin64
prepend-path    MANPATH         %{package_target}/man/

setenv          LC_ALL C
setenv          VTUNE_AMPLIFIER_XE_2015_DIR %{package_target}
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/vtune/.version.%{version}
#%Module1.0#####################################################################
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

