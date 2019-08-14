#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# mfem library that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros


# Base package name
%define pname mfem

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        Lightweight, general, scalable C++ library for finite element methods
License:        LGPLv2.1
Group:          %{PROJ_NAME}/parallel-libs
Version:        3.4
Release:        1%{?dist}
Source0:        https://github.com/mfem/mfem/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar
Url:            http://mfem.org
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  hypre-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       hypre-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  metis-%{compiler_family}%{PROJ_DELIM}
Requires:       metis-%{compiler_family}%{PROJ_DELIM}
BuildRequires:  netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       netcdf-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  petsc-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       petsc-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  ptscotch-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       ptscotch-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  superlu_dist-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       superlu_dist-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
MFEM is a modular parallel C++ library for finite element methods. Its goal is
to enable the research and development of scalable finite element discretization
and solver algorithms through general finite element abstractions, accurate and
flexible visualization, and tight integration with the hypre library.

%prep
%setup -q -n %{pname}-%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load hypre
module load metis
module load netcdf
module load petsc
module load superlu_dist

make config \
    PREFIX=%{install_path} \
    LIBDIR=%{install_path} \
    CXXFLAGS="-O3 -fPIC" \
    MFEM_USE_MPI=YES \
    MFEM_USE_LAPACK=NO \
    HYPRE_OPT=-I$HYPRE_INC HYPRE_LIB="-L$HYPRE_LIB -lHYPRE" \
    METIS_OPT=-I$METIS_INC METIS_LIB="-L$METIS_LIB -lmetis" \
    MFEM_USE_NETCDF=YES NETCDF_OPT=-I$NETCDF_INC NETCDF_LIB="-L$NETCDF_LIB -lnetcdf" \
    MFEM_USE_PETSC=YES PETSC_OPT=-I$PETSC_INC PETSC_LIB="-L$PETSC_LIB -lpetsc" \
    MFEM_USE_SUPERLU=YES SUPERLU_OPT=-I$SUPERLU_DIST_INC SUPERLU_LIB="-L$SUPERLU_DIST_LIB -lsuperlu_dist" \
    STATIC=YES SHARED=YES MFEM_DEBUG=NO

make %{?_smp_mflags}

mkdir tmp
(cd tmp; ar -x ../libmfem.a)
mpicxx -shared -Wl,-soname,libmfem.so.3 -o libmfem.so tmp/*.o

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load hypre
module load metis
module load netcdf
module load petsc
module load superlu_dist

make PREFIX=%{buildroot}%{install_path} install

sed -i 's|%{buildroot}||g' %{buildroot}%{install_path}/share/mfem/config.mk

install -m 644 libmfem.so %{buildroot}%{install_path}/lib/libmfem.so.%{version}
pushd %{buildroot}%{install_path}/lib
ln -s libmfem.so.%{version} libmfem.so.3
rm -f libmfem.a
popd

find %{buildroot}%{install_path}/. -type f -exec chmod 644 -- {} +

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}


depends-on hypre
depends-on metis
depends-on netcdf
depends-on petsc
depends-on superlu_dist

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc LICENSE README INSTALL
