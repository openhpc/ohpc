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

%define pname lua-posix
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define lualibdir %{_libdir}/lua/%{luaver}
%define luapkgdir %{_datadir}/lua/%{luaver}

Name:           %{pname}%{PROJ_DELIM}
Version:        33.2.1
Release:        %{?dist}
Summary:        POSIX library for Lua

Group:          Development/Libraries
License:        MIT
Url:            https://github.com/luaposix/luaposix
Source0:        luaposix-release-v%{version}.tar.gz
BuildRoot:      %{_tmppath}/luaposix-%{version}-%{release}-root-

BuildRequires:  lua >= %{luaver}, lua-devel >= %{luaver}
BuildRequires:  ncurses-devel
%if 0%{?suse_version} <= 1220
BuildRequires:  lua-bit%{PROJ_DELIM}
Requires:       lua-bit%{PROJ_DELIM}
%endif
Requires:       lua >= %{luaver}
BuildRequires:  autoconf-fsp
BuildRequires:  automake-fsp

# 02/27/15 karl.w.schulz@intel.com - introduce patch to allow for non-hardcoded path install
Patch1:  install_path.patch

%description
This is a POSIX library for Lua which provides access to many POSIX features
to Lua programs.

%prep
%setup -n luaposix-release-v%{version}

# Intel FSP patches
%patch1 -p0

%build
# include path to newer autotools
export PATH=/opt/fsp/pub/autotools/bin:$PATH
autoreconf
%configure --libdir=%{lualibdir} --datadir=%{luapkgdir} --docdir=%{_docdir}
make %{?_smp_mflags}


%install
# include path to newer autotools
export PATH=/opt/fsp/pub/autotools/bin:$PATH

rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT


%check
make check


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc README ChangeLog NEWS
%dir %{_docdir}/examples
%dir %{_docdir}/modules
%dir %{_docdir}/classes
%doc %{_docdir}/classes/posix.curses.*.html
%doc %{_docdir}/examples/*.lua.html
%doc %{_docdir}/modules/posix.*.html
%doc %{_docdir}/modules/posix.html
%doc %{_docdir}/index.html
%doc %{_docdir}/ldoc.css
%dir %{_datadir}/lua
%dir %{_libdir}/lua
%dir %{luapkgdir}/posix
%dir %{lualibdir}
%{lualibdir}/*
%{luapkgdir}/*.lua
%{luapkgdir}/posix/*.lua
