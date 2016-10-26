#
# spec file for package hwloc
#
# Copyright (c) 2014 SUSE LINUX Products GmbH, Nuernberg, Germany.
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


%global lname libhwloc5

Name:           hwloc
Version:        1.11.4
Release:        2%{?dist}
Summary:        Portable Hardware Locality
License:        BSD-3-Clause
Group:          Productivity/Clustering/Computing
Url:            http://www.open-mpi.org/projects/hwloc/
Source0:        http://www.open-mpi.org/software/hwloc/v1.11/downloads/%{name}-%{version}.tar.bz2
Requires:       %{lname} = %{version}-%{release}

BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  doxygen
BuildRequires:  fdupes
BuildRequires:  gcc-c++
BuildRequires:  libtool
%if 0%{?suse_version} <= 1220 && !0%{?suse_version}
BuildRequires:  pkgconfig(cairo)
BuildRequires:  pkgconfig(libxml-2.0)
BuildRequires:  pkgconfig(pciaccess)
BuildRequires:  pkgconfig(x11)
%else
BuildRequires:  cairo-devel
BuildRequires:  libxml2-devel
BuildRequires:  ncurses-devel
BuildRequires:  xorg-x11-libICE-devel
BuildRequires:  xorg-x11-libSM-devel
BuildRequires:  xorg-x11-libX11-devel
%endif
#BuildRequires:  libXNVCtrl-devel
BuildRequires:  ncurses-devel
#BuildRequires:  texlive-latex
%if 0%{?suse_version} <= 1220
BuildRequires:  texlive-bin-latex
%else
BuildRequires:  texlive-makeindex-bin
%endif
BuildRequires:  transfig
BuildRequires:  w3m
# % ifnarch s390 s390x
# BuildRequires:  libibverbs-devel
# % endif
%ifnarch s390 s390x i586 %{arm}
BuildRequires:  libnuma-devel
%endif
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
The Portable Hardware Locality (hwloc) software package provides 
a portable abstraction (across OS, versions, architectures, ...) 
of the hierarchical topology of modern architectures, including 
NUMA memory nodes,  shared caches, processor sockets, processor cores
and processing units (logical processors or "threads"). It also gathers
various system attributes such as cache and memory information. It primarily
aims at helping applications with gathering information about modern
computing hardware so as to exploit it accordingly and efficiently.

hwloc may display the topology in multiple convenient formats. 
It also offers a powerful programming interface (C API) to gather information 
about the hardware, bind processes, and much more.

%package lstopo
Summary:        Shows the topology in various formats
Group:          Productivity/Clustering/Computing

%description lstopo
Lstopo shows the topology of the system in various formats.


%package devel
Summary:        Headers and shared development libraries for hwloc
Group:          Development/Libraries/C and C++
Requires:       %{lname} = %{version}
Provides:       lib%{name}-devel = %{version}-%{release}
Obsoletes:      lib%{name}-devel <= 1.4.1

%description devel
Headers and shared object symbolic links for the hwloc.

%package -n %{lname}
Summary:        Run time libraries for the hwloc
Group:          System/Libraries
Requires:       %{name}-data

%description -n %{lname}
Run time libraries for the %{name}.

%package data
Summary:        Run time data for hwloc
Group:          Development/Libraries/C and C++

%description data
Run time data for the hwloc.

%package doc
Summary:        Documentation for hwloc
Group:          Documentation/Other

%description doc
Package contains documentation for %{name}.

%prep
%setup -q

%build
%if 0%{?sles_version}
sed -i 's/1.11 dist-bzip2 subdir-objects foreign tar-ustar parallel-tests -Wall -Werror/1.10 dist-bzip2 subdir-objects foreign tar-ustar -Wall -Werror/g' configure.ac
%endif
autoreconf --force --install
%configure
##sed -i 's|^hardcode_libdir_flag_spec=.*|hardcode_libdir_flag_spec=""|g' libtool
##sed -i 's|^runpath_var=LD_RUN_PATH|runpath_var=DIE_RPATH_DIE|g' libtool
%{__make} %{?_smp_mflags} V=1

%install
%{__make} install DESTDIR=%{buildroot} INSTALL="%{__install} -p"

#Fix wrong permition on file hwloc-assembler-remote => I have reported this to upstream already
%{__chmod} 0755 %{buildroot}%{_bindir}/hwloc-assembler-remote

# We don't ship .la files.
%{__rm} -rf %{buildroot}%{_libdir}/libhwloc.la

# documentation will be handled by % doc macro
%{__rm} -rf %{buildroot}%{_datadir}/doc/ doc/doxygen-doc/man
%{__rm} -rf doc/.deps
%fdupes -s %{buildroot}/%{_mandir}/man1
%fdupes -s %{buildroot}/%{_mandir}/man3
%fdupes -s %{buildroot}/%{_mandir}/man7
%fdupes -s doc/

%check
#XXX: this is weird, but make check got broken by removing doxygen-doc/man above
#     the only one fix is to install documentation by hand, or to ignore check error
%{__make} check || :

%post -n %{lname} -p /sbin/ldconfig

%postun -n %{lname} -p /sbin/ldconfig

%files
%defattr(-, root, root, -)
%doc AUTHORS COPYING NEWS README VERSION
%attr(0755,root,root) %{_bindir}/%{name}*
%doc %{_mandir}/man1/%{name}*

%files lstopo
%defattr(-,root,root)
%attr(0755,root,root) %{_bindir}/lstopo*
%doc %{_mandir}/man1/lstopo.1*
%doc %{_mandir}/man1/lstopo-no-graphics.1.*

%files devel
%defattr(-, root, root, -)
%doc %{_mandir}/man3/*
%doc %{_mandir}/man7/%{name}*
%{_includedir}/%{name}
%{_includedir}/%{name}.h
%{_libdir}/lib%{name}.so
%{_libdir}/pkgconfig/%{name}.pc

%files -n %{lname}
%defattr(-, root, root, -)
%{_libdir}/libhwloc*so.*

%files data
%defattr(-, root, root, -)
%dir %{_datadir}/%{name}
%{_datadir}/%{name}/%{name}.dtd
%{_datadir}/%{name}/%{name}-valgrind.supp

%files doc
%defattr(-, root, root, -)
%doc doc/

%changelog


