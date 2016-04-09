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

%define pname lua-posix

%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define lualibdir %{_libdir}/lua/%{luaver}
%define luapkgdir %{_datadir}/lua/%{luaver}
%define debug_package %{nil}

Name:           %{pname}%{PROJ_DELIM}
Version:        33.2.1
Release:        %{?dist}
Summary:        POSIX library for Lua

Group:          %{PROJ_NAME}/distro-packages
License:        MIT
Url:            https://github.com/luaposix/luaposix
Source0:        https://github.com/luaposix/luaposix/archive/release-v%{version}.tar.gz
BuildRoot:      %{_tmppath}/luaposix-%{version}-%{release}-root-

BuildRequires:  lua >= %{luaver}, lua-devel >= %{luaver}
BuildRequires:  ncurses-devel
%if 0%{?suse_version} <= 1220
BuildRequires:  lua-bit%{PROJ_DELIM}
Requires:       lua-bit%{PROJ_DELIM}
%endif
Requires:       lua >= %{luaver}
BuildRequires:  autoconf
BuildRequires:  automake

# 02/27/15 karl.w.schulz@intel.com - introduce patch to allow for non-hardcoded path install
Patch1:  install_path.patch

%description
This is a POSIX library for Lua which provides access to many POSIX features
to Lua programs.

%prep
%setup -n luaposix-release-v%{version}

# OpenHPC patches
%patch1 -p0

%build
# include path to newer autotools
export PATH=%{OHPC_PUB}/autotools/bin:$PATH
autoreconf
%configure --libdir=%{lualibdir} --datadir=%{luapkgdir} --docdir=%{_docdir}
make %{?_smp_mflags}


%install
# include path to newer autotools
export PATH=%{OHPC_PUB}/autotools/bin:$PATH

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
