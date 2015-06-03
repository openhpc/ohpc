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

%define pname autoconf
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   A GNU tool for automatically configuring source code
Name:      %{pname}%{PROJ_DELIM}
Version:   2.69
Release:   1
License:   GPLv3+ and GFDL
Group:     fsp/dev-tools
URL:       http://www.gnu.org/software/autoconf/
Source0:   autoconf-%{version}.tar.gz
Source1:   FSP_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

Requires: m4

%{!?FSP_PUB: %define FSP_PUB /opt/fsp/pub}
%define install_path %{FSP_PUB}/autotools

%description
GNU Autoconf is a tool for configuring source code and Makefiles.
Using Autoconf, programmers can create portable and configurable
packages, since the person building the package is allowed to
specify various configuration options.

You should install Autoconf if you are developing software and
would like to create shell scripts that configure your source code
packages. If you are installing Autoconf, you will also need to
install the GNU m4 package.

Note that the Autoconf package is not required for the end-user who
may be configuring software with an Autoconf-generated script;
Autoconf is only required for the generation of the scripts, not
their use.

%prep
%setup -n autoconf-%{version}

%build
./configure --prefix=%{install_path}

%install
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# remove share/info/dir to avoid conflict with other package installs
rm -f $RPM_BUILD_ROOT/%{install_path}/share/info/dir

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%dir %{FSP_HOME}
%dir %{FSP_PUB}
%{FSP_PUB}/autotools


%changelog
* Mon Sep 15 2014  <karl.w.schulz@intel.com> - 
- Initial build.

