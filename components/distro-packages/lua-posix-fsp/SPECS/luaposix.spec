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
BuildRequires:  lunit%{PROJ_DELIM}
BuildRequires:  ncurses-devel
%if 0%{?suse_version} <= 1220
BuildRequires:  lua-bit%{PROJ_DELIM}
Requires:       lua-bit%{PROJ_DELIM}
%endif
Requires:       lua >= %{luaver}
BuildRequires:  autoconf-fsp
BuildRequires:  automake-fsp

# 02/27/15 karl.w.schulz@intel.com - patch to allow for non-hardcoded path install
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
%dir %{lualibdir}
%{lualibdir}/*
%{luapkgdir}/*.lua
%{luapkgdir}/posix/*.lua
