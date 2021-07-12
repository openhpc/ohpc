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

Summary: CMake is an open-source, cross-platform family of tools designed to build, test and package software.
Name:    %{pname}%{PROJ_DELIM}
Version: 3.20.5
Release: 1%{?dist}
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
URL:            https://cmake.org/
Source0:        https://github.com/Kitware/CMake/releases/download/v%{version}/cmake-%{version}.tar.gz
BuildRequires:  gcc-c++
BuildRequires:  make
BuildRequires:  curl-devel
BuildRequires:  ncurses-devel
BuildRequires:  xz-devel
BuildRequires:  zlib-devel
BuildRequires:  pkgconfig

%if 0%{?rhel}
BuildRequires:  expat-devel
BuildRequires:  bzip2-devel
%endif

%if 0%{?suse_version} || 0%{?sle_version}
BuildRequires:  libexpat-devel
BuildRequires:  libbz2-devel
%endif

%define install_path %{OHPC_UTILS}/%{pname}/%version

%description
CMake is an open-source, cross-platform family of tools designed to build,
test and package software. CMake is used to control the software compilation
process using simple platform and compiler independent configuration files,
and generate native makefiles and workspaces that can be used in the compiler
environment of your choice. 

CMake is maintained and supported by Kitware and developed in collaboration
with a productive community of contributors.


%prep
%setup -q -n %{pname}-%{version}


%build
SMP_COUNT=$(echo "%{?_smp_mflags}" | sed "s/-j//")

./bootstrap --system-libs \
            --parallel=$SMP_COUNT \
            --no-system-librhash \
            --no-system-libuv \
            --no-system-libarchive \
            --no-system-jsoncpp \
            --no-qt-gui \
            --prefix=%{install_path}

make %{?_smp_mflags}


%install
make install DESTDIR=%{buildroot} %{?mflags_install}

# OpenHPC module file
mkdir -p %{buildroot}%{OHPC_MODULES}/%{pname}
cat << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
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

set           version %{version}

prepend-path  PATH    %{install_path}/bin
EOF

cat << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF


%files
%{install_path}
%doc %{install_path}/doc/*
%license %{install_path}/doc/cmake-3.20/Copyright.txt
%license %{install_path}/doc/cmake-3.20/cmlibarchive/COPYING
%license %{install_path}/doc/cmake-3.20/cmlibrhash/COPYING
%license %{install_path}/doc/cmake-3.20/cmlibuv/LICENSE
%license %{install_path}/doc/cmake-3.20/cmsys/Copyright.txt
%{OHPC_MODULES}/%{pname}
