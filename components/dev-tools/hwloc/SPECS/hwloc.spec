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

%define pname hwloc

Name:           %{pname}%{PROJ_DELIM}
Version:        2.0.3
Release:        1%{?dist}
Summary:        Portable Hardware Locality
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Url:            http://www.open-mpi.org/projects/hwloc/
Source0:        https://download.open-mpi.org/release/hwloc/v2.0/%{pname}-%{version}.tar.bz2

BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  doxygen
%if 0%{?sles_version} || 0%{?suse_version}
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
%if 0%{?sles_version} || 0%{?suse_version}
sed -i 's/1.11 dist-bzip2 subdir-objects foreign tar-ustar parallel-tests -Wall -Werror/1.10 dist-bzip2 subdir-objects foreign tar-ustar -Wall -Werror/g' configure.ac
%endif
autoreconf --force --install
./configure --prefix=%{install_path} --libdir=%{install_path}/lib
##sed -i 's|^hardcode_libdir_flag_spec=.*|hardcode_libdir_flag_spec=""|g' libtool
##sed -i 's|^runpath_var=LD_RUN_PATH|runpath_var=DIE_RPATH_DIE|g' libtool
%{__make} %{?_smp_mflags} V=1

%install
%{__make} install DESTDIR=%{buildroot} INSTALL="%{__install} -p"

# We don't ship .la files.
%{__rm} -rf %{buildroot}%{install_path}/lib/libhwloc.la

# documentation will be handled by % doc macro
%{__rm} -rf %{buildroot}%{install_path}/doc/ doc/doxygen-doc/man
%{__rm} -rf doc/.deps
%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s %{buildroot}/%{install_path}/share/man/man1
%fdupes -s %{buildroot}/%{install_path}/share/man/man3
%fdupes -s %{buildroot}/%{install_path}/share/man/man7
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

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

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
%doc AUTHORS COPYING NEWS README VERSION
%{OHPC_PUB}
