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

%define pname lua-filesystem

#
# spec file for package luafilesystem
# Copyright (c) 2009 florian.leparoux@gmail.com
# Copyright (c) 2012 Togan Muftuoglu toganm@opensuse.org
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

#
# Please submit bugfixes or comments via http://bugs.opensuse.org/
#
%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define lualibdir %{_libdir}/lua/%{luaver}
%define luapkgdir %{_datadir}/lua/%{luaver}
%define debug_package %{nil}

%define version_exp 1_6_3

Name:           %{pname}%{PROJ_DELIM}
Version:        1.6.3
Release:        0
License:        MIT
Summary:        Lua library to Access Directories and Files
Url:            http://keplerproject.github.com/luafilesystem
Group:          %{PROJ_NAME}/distro-packages
Source:         https://github.com/keplerproject/luafilesystem/archive/v_%{version_exp}.tar.gz
# PATCH-FIX_UPSTREAM -- toganm@opensuse.org provide optflags for config
Patch1:         luafilesystem-%{luaver}-optflags.patch
# PATCH-FIX-UPSTREAM -- toganm@opensuse.org fixes Makefile for DESTDIR
Patch2:         luafilesystem-%{version}-destdir.patch
BuildRequires:  lua-devel
BuildRoot:      %{_tmppath}/%{pname}-%{version}-build

%description
LuaFileSystem is a Lua library developed to complement the set of functions
related to file systems offered by the standard Lua distribution.
LuaFileSystem offers a portable way to access the underlying directory
structure and file attributes.

%prep
%setup -q -n luafilesystem-v_%{version_exp}
%patch1
%patch2 -p1

sed -i 's|@@LIBDIR@@|%{_libdir}|g;s|@@INCLUDEDIR@@|%{_includedir}|g;' $(perl -ne 'print $1,"\n" if /^\+{3}\s+(.+?)\s+/' "%{PATCH1}")

%build

make %{?_smp_mflags} \
    PREFIX=%{_prefix} \
    LUA_LIBDIR=%{lualibdir} \
    CFLAGS="%{optflags} -fPIC"

%install
%make_install \
    DESTDIR=%buildroot \
    PREFIX="%{_prefix}" \
    LUA_LIBDIR="%{lualibdir}"


%files
%defattr(-,root,root,-)
%dir %{_libdir}/lua
%dir %{lualibdir}
%{lualibdir}/lfs*

