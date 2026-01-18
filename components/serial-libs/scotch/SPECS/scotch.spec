#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# scotch - Graph, mesh and hypergraph partitioning library (serial version)
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

%define pname scotch

Name:		%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:	7.0.10
Release:	1%{?dist}
Summary:	Graph, mesh and hypergraph partitioning library
License:	CeCILL-C
Group:		%{PROJ_NAME}/serial-libs
URL:		https://gitlab.inria.fr/scotch/scotch
Source0:	https://gitlab.inria.fr/scotch/scotch/-/archive/v%{version}/scotch-v%{version}.tar.bz2
Source1:	%{pname}-rpmlintrc

BuildRequires:	flex bison make cmake%{PROJ_DELIM}
BuildRequires:  bzip2-devel
Requires:       bzip2-devel
BuildRequires:  zlib-devel

%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%description
Scotch is a software package for graph and mesh/hypergraph partitioning and
sparse matrix ordering.

%prep
%setup -q -n %{pname}-v%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
module load cmake

cmake -S . -B build \
    -DCMAKE_INSTALL_PREFIX=%{install_path} \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_FLAGS="${CFLAGS} -fPIC" \
    -DBUILD_SHARED_LIBS=ON \
    -DBUILD_PTSCOTCH=OFF \
    -DBUILD_LIBSCOTCHMETIS=ON \
    -DBUILD_LIBESMUMPS=ON \
    -DTHREADS=ON \
    -DIDXSIZE=64 \
    -DSCOTCH_DETERMINISTIC=FIXED_SEED

cmake --build build %{?_smp_mflags}

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
module load cmake

DESTDIR=%{buildroot} cmake --install build

# Convert the license files to utf8
pushd doc
iconv -f iso8859-1 -t utf-8 < CeCILL-C_V1-en.txt > CeCILL-C_V1-en.txt.conv
iconv -f iso8859-1 -t utf-8 < CeCILL-C_V1-fr.txt > CeCILL-C_V1-fr.txt.conv
mv -f CeCILL-C_V1-en.txt.conv CeCILL-C_V1-en.txt
mv -f CeCILL-C_V1-fr.txt.conv CeCILL-C_V1-fr.txt
popd

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib64

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib64
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}%{OHPC_CUSTOM_PKG_DELIM}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}%{OHPC_CUSTOM_PKG_DELIM}"
EOF

%files
%doc README.txt ./doc/*
%{OHPC_PUB}
