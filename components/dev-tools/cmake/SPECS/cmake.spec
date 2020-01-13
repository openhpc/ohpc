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

%define pname cmake

%define major_version 3.16
%define minor_version 2

Summary: CMake is an open-source, cross-platform family of tools designed to build, test and package software.
Name:    %{pname}%{PROJ_DELIM}
Version: %{major_version}.%{minor_version}
Release: 1%{?dist}
# https://spdx.org/licenses/BSD-3-Clause.html
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
URL:            https://cmake.org/
Source0:        https://cmake.org/files/v%{major_version}/cmake-%{version}.tar.gz
BuildRequires:  gcc-c++
BuildRequires:  curl-devel
BuildRequires:  ncurses-devel
BuildRequires:  xz-devel
BuildRequires:  zlib-devel
BuildRequires:  pkgconfig

%if 0%{?rhel}
BuildRequires:  expat-devel
BuildRequires:  bzip2-devel
%endif
%if 0%{?suse_version}
BuildRequires:  libexpat-devel
BuildRequires:  libbz2-devel
%endif

%define install_path %{OHPC_UTILS}/%{pname}/%version

%description
CMake is used to control the software compilation process using simple
platform and compiler independent configuration files, and generate native
makefiles and workspaces that can be used in the compiler environment
of your choice.

%prep
%setup -q -n %{pname}-%{version}

./bootstrap --system-libs \
--no-system-librhash \
--no-system-libuv \
--no-system-libarchive \
--no-system-jsoncpp \
--no-qt-gui \
--prefix=%{install_path}

%build
%{__make} %{?_smp_mflags}

%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

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
module-whatis "Category: utility, developer support"
module-whatis "Keywords: System, Utility"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%dir %{OHPC_UTILS}
%{OHPC_UTILS}/%{pname}
%{OHPC_MODULES}/%{pname}
