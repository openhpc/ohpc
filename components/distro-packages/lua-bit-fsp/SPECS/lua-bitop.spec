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

%define pname lua-bit
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define lualibdir %{_libdir}/lua/%{luaver}
%define luapkgdir %{_datadir}/lua/%{luaver}

Name:           %{pname}%{PROJ_DELIM}
Version:        1.0.2
Release:        1%{?dist}
Summary:        Module for Lua which adds bitwise operations on numbers
Group:          fsp/distro-packages
License:        MIT
Url:            http://bitop.luajit.org
Source0:        LuaBitOp-%{version}.tar.gz
Patch0:         Makefile.patch
BuildRoot:      %{_tmppath}/%{pname}-%{version}-%{release}-root

BuildRequires:  lua >= %{luaver}, lua-devel >= %{luaver}
Requires:       lua >= %{luaver}

%description
Lua BitOp is a C extension module for Lua 5.1/5.2 which adds bitwise operations on numbers.

%prep
%setup -n LuaBitOp-%{version}
%patch0

%build
make


%install
rm -rf $RPM_BUILD_ROOT
make install LUA_LIBDIR=%{lualibdir} DESTDIR=$RPM_BUILD_ROOT


%check
make test


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc README
%dir %{_libdir}/lua
%dir %{lualibdir}
%{lualibdir}/*
