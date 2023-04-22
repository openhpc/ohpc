#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Parallel NetCDF library that is dependent on compiler toolchain and MPI

# Build that is dependent on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname pnetcdf

Summary:   A Parallel NetCDF library (PnetCDF)
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   1.12.3
%global    sonum 1
Release:   1%{?dist}
License:   NetCDF
Group:     %{PROJ_NAME}/io-libs
URL:       http://cucis.ece.northwestern.edu/projects/PnetCDF
Source0:   https://parallel-netcdf.github.io/Release/pnetcdf-%{version}.tar.gz

BuildRequires:  grep
BuildRequires:  make
BuildRequires:  m4
BuildRequires:  zlib-devel

%if "%{compiler_family}" == "intel"
BuildRequires: libtool%{PROJ_DELIM}
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
PnetCDF is a high-performance parallel I/O library for accessing files in format compatibility with
Unidata's NetCDF, specifically the formats of CDF-1, 2, and 5. The CDF-5 file format, an extension of
CDF-2, supports unsigned data types and uses 64-bit integers to allow users to define large dimensions,
attributes, and variables (> 2B array elements).

%prep

%setup -q -n pnetcdf-%{version}

%build

# override with newer config.guess for aarch64
%ifarch aarch64 || ppc64le
%if 0%{?rhel} >= 9
cp /usr/lib/rpm/redhat/config.guess bin
%else
cp /usr/lib/rpm/config.guess bin
%endif
%endif

%if "%{compiler_family}" == "intel"
export PATH=%{OHPC_UTILS}/autotools/bin:${PATH}
autoreconf -if
%endif

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "arm1"
# For some reason the flag '-lmpi"' has a double quote at the end.
# This tries to adapt the configure script to remove double quotes
# from the linker options.
%{__sed} -i -e 's,^  ac_cv_fc_libs="\$ac_cv_fc_libs $ac_arg,  ac_cv_fc_libs="\$ac_cv_fc_libs \$(echo \$ac_arg | sed "s/\\"//g"),g' configure
export CFLAGS="${CFLAGS} -fsimdmath"
export CXXFLAGS="${CXXFLAGS} -fsimdmath"
export FCFLAGS="${FCFLAGS} -fsimdmath"
export F77LAGS="${F77LAGS} -fsimdmath"
%endif

CC=mpicc \
CXX=mpicxx \
F77=mpif77 \
FC=mpif90 \
MPICC=mpicc \
MPIFC=mpifc \
MPIF77=mpif77 \
MPICXX=mpicxx \
CFLAGS="${CFLAGS} -fPIC -DPIC" \
CXXFLAGS="${CXXFLAGS} -fPIC -DPIC" \
FCFLAGS="${FCFLAGS} -fPIC" \
FFLAGS="${F77FLAGS} -fPIC" \
./configure --prefix=%{install_path} || { cat config.log && exit 1; }

%{__make}

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%{__make} DESTDIR=$RPM_BUILD_ROOT install

find %{buildroot}/%{install_path} -name '*.la' -delete

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc AUTHORS ChangeLog COPYRIGHT CREDITS INSTALL NEWS README RELEASE_NOTES
