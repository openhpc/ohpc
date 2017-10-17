#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%global ohpc_bootstrap 1

%include %{_sourcedir}/OHPC_macros

%global pname lua-filesystem

%if 0%{?suse_version} <= 1220
%global luaver 5.1
%else
%global luaver 5.2
%endif
%global lualibdir %{_libdir}/lua/%{luaver}
%global luapkgdir %{_datadir}/lua/%{luaver}

%global version_exp 1_6_3

Name:           %{pname}%{PROJ_DELIM}
Version:        1.6.3
Release:        0%{?dist}
License:        MIT
Summary:        Lua library to Access Directories and Files
Url:            http://keplerproject.github.com/luafilesystem
Group:          %{PROJ_NAME}/distro-packages
Source:         https://github.com/keplerproject/luafilesystem/archive/v_%{version_exp}.tar.gz
Source1:        OHPC_macros
# PATCH-FIX_UPSTREAM -- toganm@opensuse.org provide optflags for config
Patch1:         luafilesystem-%{luaver}-optflags.patch
# PATCH-FIX-UPSTREAM -- toganm@opensuse.org fixes Makefile for DESTDIR
Patch2:         luafilesystem-%{version}-destdir.patch
BuildRequires:  lua-devel

%description
LuaFileSystem is a Lua library developed to complement the set of functions
related to file systems offered by the standard Lua distribution.
LuaFileSystem offers a portable way to access the underlying directory
structure and file attributes.

%prep
%setup -q -n luafilesystem-v_%{version_exp}
%patch1
%patch2 -p1

sed -i 's|@@LIBDIR@@|%{_libdir}|g;s|@@INCLUDEDIR@@|%{_includedir}|g;' config

%build
make %{?_smp_mflags} LUA_LIBDIR=%{lualibdir} CFLAGS="%{optflags} -fPIC"

%install
%make_install LUA_LIBDIR="%{lualibdir}"

%files
%dir %{_libdir}/lua
%dir %{lualibdir}
%{lualibdir}/lfs*

%changelog
* Tue Oct 17 2017 Adrian Reber <areber@redhat.com> - 1.6.3-0
- removed setting PROJ_DELIM; this is done by OHPC_macros
- replaced 'define' with 'global' to avoid scope problems and warnings
- removed BuildRoot (not necessary) and debug_package (OHPC_macros)
- removed implicit build dependency on perl
- removed setting unused variables
- removed opensuse comments; it is an OpenHPC spec file now
