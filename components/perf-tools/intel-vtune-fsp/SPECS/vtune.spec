#-------------------------------------------------------------------------------
# Copyright (c) 2015, Intel Corporation
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Intel Corporation nor the names of its contributors
#       may be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#-------------------------------------------------------------------------------

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

