#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

Name:           libconfuse0
Version:        2.7
Release:        1
License:        LGPL-2.1+
Group:          Development/Libraries/C and C++
BuildRequires:  gcc-c++ gettext-devel libtool pkgconfig
%if 0%{?suse_version} > 1020
BuildRequires:  check-devel
Recommends:     %{name}-lang
%else
BuildRequires:  check
%endif
%define pkg_name confuse
#
URL:            http://www.nongnu.org/confuse/
# taken from    http://download.savannah.gnu.org/releases/confuse/%{pkg_name}-%{version}.tar.gz
Source:         http://savannah.nongnu.org/download/confuse/confuse-%{version}.tar.gz 
#
Summary:        A configuration file parser library

%description
libConfuse is a configuration file parser library, licensed under the
terms of the LGPL, and written in C. It supports sections and (lists
of) values (strings, integers, floats, booleans or other sections), as
well as some other features (such as single/double-quoted strings,
environment variable expansion, functions and nested include
statements). It makes it very easy to add configuration file capability
to a program using a simple API.

The goal of libConfuse is not to be the configuration file parser
library with a gazillion of features. Instead, it aims to be easy to
use and quick to integrate with your code. libConfuse was called libcfg
before, but was changed to not confuse with other similar libraries.

%package -n libconfuse-devel
Group:          Development/Libraries/C and C++
Requires:       %{name} = %{version}
Summary:        The development files for libconfuse

%description -n libconfuse-devel
libConfuse is a configuration file parser library, licensed under the
terms of the LGPL, and written in C. It supports sections and (lists
of) values (strings, integers, floats, booleans or other sections), as
well as some other features (such as single/double-quoted strings,
environment variable expansion, functions and nested include
statements). It makes it very easy to add configuration file capability
to a program using a simple API.

The goal of libConfuse is not to be the configuration file parser
library with a gazillion of features. Instead, it aims to be easy to
use and quick to integrate with your code. libConfuse was called libcfg
before, but was changed to not confuse with other similar libraries.

This package holds the development files for libconfuse.

%lang_package
%prep
%setup -q -n %{pkg_name}-%{version}

%build
autoreconf -fi
%configure --enable-shared --disable-static
%{__make} %{?_smp_mflags}

%install
%makeinstall
%find_lang %{pkg_name}
%{__install} -Dd %{buildroot}%{_mandir}
%{__cp} -Rv doc/man/man3/ %{buildroot}%{_mandir}
# clean up examples
%{__make} -C examples clean
rm -rf examples/.deps/ examples/Makefile*

%post   -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%{_libdir}/libconfuse.so.0.0.0
%{_libdir}/libconfuse.so.0

%files -n libconfuse-devel
%{_libdir}/libconfuse.so
%{_libdir}/libconfuse.la
%{_libdir}/pkgconfig/libconfuse.pc
%{_includedir}/confuse.h
%{_mandir}/man3/*.3*
%{_datadir}/
%doc doc/html/ doc/tutorial-html/ examples/

