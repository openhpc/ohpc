#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Serial metis build dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname example

Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary: A hello world example
Version: 1.0
Release: 1%{?dist}
License: Apache License 2.0
Group:   %{PROJ_NAME}/serial-libs
URL:     https://github.com/shanecelis/amhello
Source:  amhello-%{version}.tar.gz
BuildRequires: make
BuildRequires: pkgconfig

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%description
Simple example.

%prep
%setup -q -n amhello-%{version}
%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

./configure --prefix=%{install_path} || { cat config.log && exit 1; }
make

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

make install DESTDIR=${RPM_BUILD_ROOT}

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%{OHPC_PUB}

