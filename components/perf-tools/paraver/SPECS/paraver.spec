#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler/mpi toolchains
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname paraver

Summary:       Paraver
Name:          %{pname}%{PROJ_DELIM}
Version:       4.8.1
Release:       1%{?dist}
License:       LGPL-2.1
Group:         %{PROJ_NAME}/perf-tools
URL:           https://tools.bsc.es
Source0:       https://ftp.tools.bsc.es/wxparaver/wxparaver-%{version}-src.tar.bz2


BuildRequires: bison
BuildRequires: boost-devel

%if 0%{?suse_version}
BuildRequires: libfabric1
BuildRequires: flex
BuildRequires: wxGTK3-3_2-devel
# libboost::DATE_TIME needed for build.
%if 0%{?sle_version} >= 150000 
BuildRequires: libboost_date_time1_66_0-devel
%endif
%else
BuildRequires: flex-devel
BuildRequires: wxGTK3-devel
%endif

BuildRequires: autoconf%{PROJ_DELIM}
BuildRequires: automake%{PROJ_DELIM}
BuildRequires: libtool%{PROJ_DELIM}
BuildRequires: binutils-devel
BuildRequires: libxml2-devel

%define __arch_install_post %{nil}

# Default library install path
%define install_path %{OHPC_UTILS}/%{pname}/%version

%description 
Paraver was developed to respond to the need to have a qualitative
global perception of the application behavior by visual inspection and then to
be able to focus on the detailed quantitative analysis of the
problems. Expressive power, flexibility and the capability of efficiently
handling large traces are key features addressed in the design of Paraver. The
clear and modular structure of Paraver plays a significant role towards
achieving these targets.

%prep
%setup -q -n wxparaver-%{version}

%build

%if 0%{?centos_version} || 0%{?rhel}
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-wx-config=/usr/bin/wx-config-3.0 "
%endif
%if 0%{?suse_version}
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-wx-config=/usr/bin/wx-config "
%endif

./configure --prefix=%{install_path} $CONFIGURE_OPTIONS \
    --libdir=%{install_path}/lib \
    --with-boost-libdir=/usr/lib64 \
    CXXFLAGS=-I$RPM_BUILD_ROOT/%{install_path}/include \
    LDFLAGS=-L$RPM_BUILD_ROOT/%{install_path}/lib/paraver-kernel

# 2-stage Build: first we have to build and install paraver-kernel
cd $RPM_BUILD_DIR/wxparaver-%{version}/src/paraver-kernel/
make %{?_smp_mflags}
make DESTDIR=$RPM_BUILD_ROOT install

# next, build paraver gui
cd $RPM_BUILD_DIR/wxparaver-%{version}/
make %{?_smp_mflags}

%install
make DESTDIR=$RPM_BUILD_ROOT install
export NO_BRP_CHECK_RPATH=true

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} <<EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} utility."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} <<EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}


%files
%{OHPC_PUB}

%doc
