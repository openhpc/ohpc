#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
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

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

%define pname hwloc

%global lname libhwloc5

Name:           %{pname}%{PROJ_DELIM}
Version:        1.11.6
Release:        2%{?dist}
Summary:        Portable Hardware Locality
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Url:            http://www.open-mpi.org/projects/hwloc/
Source0:        http://www.open-mpi.org/software/hwloc/v1.11/downloads/%{name}-%{version}.tar.bz2
Source1:   OHPC_macros
Source2:   LICENSE

BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  doxygen
%if 0%{?sles_version}
BuildRequires:  fdupes
%endif
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
%if 0%{?suse_version} && 0%{?suse_version} <= 1220
BuildRequires:  texlive-bin-latex
%else
BuildRequires:  texlive-latex-bin
%endif
BuildRequires:  transfig
BuildRequires:  w3m
# % ifnarch s390 s390x
# BuildRequires:  libibverbs-devel
# % endif
%ifnarch s390 s390x i586 %{arm}
%if 0%{?suse_version}
BuildRequires:  libnuma-devel
%else
BuildRequires:  numactl-devel
%endif
%endif
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
#!BuildIgnore: post-build-checks rpmlint-Factory
#!BuildIgnore: #!BuildIgnore: brp-check-suse

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

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

%prep
%setup -q -n %{pname}-%{version}

%build
%if 0%{?sles_version}
sed -i 's/1.11 dist-bzip2 subdir-objects foreign tar-ustar parallel-tests -Wall -Werror/1.10 dist-bzip2 subdir-objects foreign tar-ustar -Wall -Werror/g' configure.ac
%endif
autoreconf --force --install
%configure --prefix=%{install_path}
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
%if 0%{?sles_version}
%fdupes -s %{buildroot}/%{_mandir}/man1
%fdupes -s %{buildroot}/%{_mandir}/man3
%fdupes -s %{buildroot}/%{_mandir}/man7
%fdupes -s doc/
%endif

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

        puts stderr " "
        puts stderr "This module loads the %{pname} utility"
        puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path    MANPATH             %{install_path}/man

setenv          %{pname}_DIR        %{install_path}
setenv          %{pname}_BIN        %{install_path}/bin
setenv          %{pname}_INC        %{install_path}/inc
setenv          %{pname}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-, root, root, -)
%doc AUTHORS COPYING NEWS README VERSION
%{OHPC_HOME}
%{OHPC_PUB}

%changelog


