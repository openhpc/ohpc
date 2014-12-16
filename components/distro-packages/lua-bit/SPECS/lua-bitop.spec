%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define lualibdir %{_libdir}/lua/%{luaver}
%define luapkgdir %{_datadir}/lua/%{luaver}

Name:           lua-bit
Version:        1.0.2
Release:        1%{?dist}
Summary:        Module for Lua which adds bitwise operations on numbers
Group:          Development/Libraries
License:        MIT
Url:            http://bitop.luajit.org
Source0:        LuaBitOp-%{version}.tar.gz
Patch0:         Makefile.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root

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
