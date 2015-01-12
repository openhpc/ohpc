%define compiler_family intel
%{!?PROJ_DELIM: %define PROJ_DELIM %{nil}}

Summary:   Intel(R) VTune(TM) Amplifier XE 2015 Update 1
Name:      intel-vtune%{PROJ_DELIM}
Version:   15.1.1.380310
Source0:   intel-vtune-amplifier-fsp-%{version}.tar.gz
Source1:   FSP_macros
Release:   1
License:   Copyright (C) 2011-2014 Intel Corporation. All rights reserved.
Vendor:    Intel Corporation
URL:       http://www.intel.com/software/products/
Group:     Performance
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no
#AutoReqProv: no

%include %{_sourcedir}/FSP_macros

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

%define debug_package %{nil}

%define package_target /opt/fsp/pub/vtune_amplifier/%{version}

%description

FSP collection of the Intel Vtune distribution.

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
puts stderr "This module loads the Intel Vtune environment:"
puts stderr " "
puts stderr "Execute 'amplxe-gui' to launch the GUI-based interface or 'amplxe-cl'. "
puts stderr "for the command-line tool."
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

prepend-path    PATH            %{package_target}/bin64
prepend-path    MANPATH         %{package_target}/man/

setenv          VTUNE_AMPLIFIER_XE_2015_DIR      %{package_target}
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/intel/vtune/.version.%{version}
#%Module1.0#####################################################################
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

