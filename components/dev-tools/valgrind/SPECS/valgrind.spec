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

%define pname valgrind
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   Valgrind Memory Debugger
Name:      %{pname}%{PROJ_DELIM}
Version:   3.10.1
Release:   1
License:   GPL
URL:       http://www.valgrind.org/
Group:     fsp/dev-tools
Source:    valgrind-%{version}.tar.bz2
Source1:   FSP_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

# Default library install path
%define install_path %{FSP_PUB}/%{pname}/%version

%description 

Valgrind is an award-winning instrumentation framework for building dynamic
analysis tools. There are Valgrind tools that can automatically detect many
memory management and threading bugs, and profile your programs in detail. You
can also use Valgrind to build new tools.  Valgrind runs on the following
platforms: x86/Linux, AMD64/Linux, PPC32/Linux, PPC64/Linux, x86/MacOSX,
AMD64/MacOSX.

%prep
%setup -q -n %{pname}-%{version}

%build
./configure --prefix=%{install_path} || cat config.log
make %{?_smp_mflags}

%install
make install DESTDIR=$RPM_BUILD_ROOT

# modulefile

%{__mkdir} -p %{buildroot}/%{FSP_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################
# FSP %{pname} environment
#############################################################################

proc ModulesHelp { } {
puts stderr "This module loads the %{pname} package for performing dynamic analysis."
puts stderr " "
}

module-whatis "Name: Valgrind Memory Debugger"
module-whatis "Version: %{version}"
module-whatis "Category: utility, developer support"
module-whatis "Keywords: Debugging"
module-whatis "Description: Memory debugging utilities"

prepend-path    PATH             %{install_path}/bin
prepend-path    MANPATH          %{install_path}/share/man
prepend-path    PKG_CONFIG_PATH  %{install_path}/lib/pkgconfig
prepend-path    LD_LIBRARY_PATH  %{install_path}/lib/pkgconfig

setenv          %{PNAME}_DIR     %{install_path}
setenv          %{PNAME}_LIB     %{install_path}/lib/valgrind
setenv          %{PNAME}_INC     %{install_path}/include
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%defattr(-,root,root)
%{FSP_HOME}

%clean
rm -rf $RPM_BUILD_ROOT
