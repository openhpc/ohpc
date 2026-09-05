#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
# OpenHPC:check-updates:skip no standard upstream source

# Serial metis build dependent on compiler toolchain
%global ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros
%global pname metis
%global dist_ver 0.5

Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary: Serial Graph Partitioning and Fill-reducing Matrix Ordering
Version: 5.1.0
Release: 1%{?dist}
License: ASL 2.0
Group:   %{PROJ_NAME}/serial-libs
URL:     http://glaros.dtc.umn.edu/gkhome/metis/metis/overview
Source0: https://github.com/mfem/tpls/raw/refs/heads/gh-pages/metis-5.1.0.tar.gz
BuildRequires: make
BuildRequires: pkgconfig
BuildRequires: cmake
Requires:      libmetis0 = %{version}
Provides:      libmetis = %{version}
Provides:      libmetis0 = %{version}

# Default library install path
%global install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version
%global module_path %{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}

%description
METIS is a set of serial programs for partitioning graphs, partitioning
finite element meshes, and producing fill reducing orderings for sparse
matrices. The algorithms implemented in METIS are based on the multilevel
recursive-bisection, multilevel k-way, and multi-constraint partitioning
schemes developed in our lab.


%prep
%setup -q -n %{pname}-%{version}

%build
%ohpc_setup_compiler
make config shared=1 prefix=%{install_path}
make


%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

make install DESTDIR=${RPM_BUILD_ROOT}

# OpenHPC module file
mkdir -p %{buildroot}%{module_path}
cat << EOF > %{buildroot}%{module_path}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
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

set version     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family metis
EOF

# Set default version
cat <<EOF >%{buildroot}%{module_path}/.version
#%Module1.0#####################################################################
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

mkdir -p %{buildroot}%{_docdir}

%files
%{OHPC_PUB}
%doc Changelog
%license LICENSE.txt
