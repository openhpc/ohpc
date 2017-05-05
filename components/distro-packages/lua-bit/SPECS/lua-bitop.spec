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
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

%define pname lua-bit

%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define lualibdir %{_libdir}/lua/%{luaver}
%define luapkgdir %{_datadir}/lua/%{luaver}
%define debug_package %{nil}

Name:           %{pname}%{PROJ_DELIM}
Version:        1.0.2
Release:        1%{?dist}
Summary:        Module for Lua which adds bitwise operations on numbers
Group:          %{PROJ_NAME}/distro-packages
License:        MIT
Url:            http://bitop.luajit.org
Source0:        http://bitop.luajit.org/download/LuaBitOp-%{version}.tar.gz
Source1:        OHPC_macros
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
