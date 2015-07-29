#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package mxml
#
# Copyright (c) 2015 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

%include %{_sourcedir}/FSP_macros
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

Name:           mxml
Url:            http://www.msweet.org/projects.php?Z3
Version:        2.9
Release:        0
Summary:        Small XML Parsing Library
License:        LGPL-2.1+
Group:          Development/Libraries/C and C++
DocDir:         %{FSP_PUB}/doc/contrib

Source:         http://www.msweet.org/files/project3/%{name}-%{version}.tar.gz
Source1:        baselibs.conf
Patch:          mxml-2.3-nobinstrip.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRequires:  pkgconfig
%define debug_package %{nil}

%description
Mini-XML is a small XML parsing library that you can use to read XML
and XML-like data files in your application without requiring large
nonstandard libraries.

This package holds the commandline tools for mxml.

%define library_name libmxml1

%package -n %library_name
#
Summary:        Shared library for mxml
License:        LGPL-2.1+
Group:          System/Libraries

%description -n %library_name
Mini-XML is a small XML parsing library that you can use to read XML
and XML-like data files in your application without requiring large
nonstandard libraries.

This package holds the shared library for mxml.

%prep
%setup
%patch

%build
%configure --enable-shared --with-docdir=%{_docdir}
make %{?_smp_mflags}

%install
#_makeinstall DSTROOT=_{buildroot}
make DESTDIR=%{buildroot} install DSTROOT=%{buildroot}
# we dont want the static lib
%{__rm} -rv %{buildroot}%{_libdir}/libmxml.a
%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%post   -n %{library_name} -p /sbin/ldconfig

%postun -n %{library_name} -p /sbin/ldconfig

%files
%defattr(-,root,root)
%{_bindir}/mxmldoc
%{_mandir}/man1/mxmldoc.1*
%doc %{_docdir}
%exclude %{_docdir}/mxml.html
%exclude %{_docdir}/*gif

%files -n %{library_name}
%defattr(-,root,root)
%{_libdir}/libmxml.so.1*
%{_includedir}/mxml.h
%{_libdir}/libmxml.so
%{_libdir}/pkgconfig/mxml.pc
%{_mandir}/man3/mxml.3*
%doc %{_docdir}/mxml.html
%doc %{_docdir}/*gif

%changelog
