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
%global ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros
%global pname metis

Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary: Serial Graph Partitioning and Fill-reducing Matrix Ordering
Version: 5.1.0
Release: 1%{?dist}
License: Apache License 2.0
Group:   %{PROJ_NAME}/serial-libs
URL:     http://glaros.dtc.umn.edu/gkhome/metis/metis/overview
Source0: http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/metis-%{version}.tar.gz
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
METIS is a family of programs for partitioning unstructured graphs and hypergraph
and computing fill-reducing orderings of sparse matrices. The underlying algorithms
used by METIS are based on the state-of-the-art multilevel paradigm that has been
shown to produce high quality results and scale to very large problems.


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
cat << EOF > %{buildroot}%{module_path}/%{version}%{OHPC_CUSTOM_PKG_DELIM}.lua
help([[
This module loads the %{pname} library built with the %{compiler_family} compiler toolchain.
Version %{version}
]])

whatis("Name: %{pname} built with %{compiler_family} toolchain")
whatis("Version: %{version}")
whatis("Category: runtime library")
whatis("Description: %{summary}")
whatis("%{url}")

local version = "%{version}"

prepend_path("PATH",            "%{install_path}/bin")
prepend_path("INCLUDE",         "%{install_path}/include")
prepend_path("LD_LIBRARY_PATH",	"%{install_path}/lib")

setenv("%{pname}_DIR", "%{install_path}")
setenv("%{pname}_BIN", "%{install_path}/bin")
setenv("%{pname}_LIB", "%{install_path}/lib")
setenv("%{pname}_INC", "%{install_path}/include")

family("metis")
EOF

ln -s %{version}%{OHPC_CUSTOM_PKG_DELIM} %{buildroot}%{module_path}/default

mkdir -p %{buildroot}%{_docdir}

%files
%{install_path}
%{module_path}
%doc BUILD.txt Changelog Install.txt
%license LICENSE.txt
