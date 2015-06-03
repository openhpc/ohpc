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

%include %{_sourcedir}/FSP_macros
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   Intel(R) Cluster Checker
Name:      intel-clck%{PROJ_DELIM}
Version:   2.2.1
Release:   1
License:   Intel
URL:       http://intel.com/go/cluster
Group:     fsp/admin
BuildArch: x86_64
Source1:   stream.static
Source2:   dgemm_mflops.static
Source3:   FSP_macros
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%description

Intel cluster checker.

%prep

%build

%install

%{__mkdir} -p %{buildroot}/
cd %{buildroot}
%{__tar} xfz $RPM_SOURCE_DIR/intel-clck%{PROJ_DELIM}-%{version}.tar.gz
# Update key executiables with static versions
cp %{SOURCE1} %{buildroot}/%{FSP_ADMIN}/clck/%{version}/share/intel64/stream
cp %{SOURCE2} %{buildroot}/%{FSP_ADMIN}/clck/%{version}/share/intel64/dgemm_mflops
cd -

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

