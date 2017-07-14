#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# slepc - A library for solving large scale sparse eigenvalue problems
#

# Build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros


# Base package name

%define pname slepc
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        3.7.4
Release:        1
Summary:        A library for solving large scale sparse eigenvalue problems
License:        LGPL-3.0
Group:          %{PROJ_NAME}/parallel-libs
Url:            http://www.grycap.upv.es/slepc/
Source0:        http://slepc.upv.es/download/distrib/%{pname}-%{version}.tar.gz
Source1:        OHPC_macros
BuildRoot:      %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:         %{OHPC_PUB}/doc/contrib
BuildRequires: petsc-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires: petsc-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

# A configure script in slepc is made by python
BuildRequires: python

%if "%{compiler_family}" != "intel"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Disable debug packages
%define debug_package %{nil}
# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
SLEPc is a software library for the solution of large scale sparse
eigenvalue problems on parallel computers. It is an extension of
PETSc and can be used for either standard or generalized eigenproblems,
with real or complex arithmetic. It can also be used for computing a
partial SVD of a large, sparse, rectangular matrix, and to solve
quadratic eigenvalue problems.

%prep
%setup -q -n %{pname}-%{version}

set -- *

%build
%ohpc_setup_compiler
%if "%{compiler_family}" != "intel"
module load openblas
%endif
module load petsc
./configure --prefix=/tmp%{install_path}
make 

%install
%ohpc_setup_compiler
%if "%{compiler_family}" != "intel"
module load openblas
%endif
module load petsc
make install

# move from tmp install dir to %install_path
rm -rf %buildroot
mkdir -p %buildroot%{install_path}
pushd /tmp
export tmp_path=%{install_path}
mv ${tmp_path#*/} %buildroot%{install_path}/..
popd

# clean up
pushd %{buildroot}%{install_path}
sed -i 's|/tmp||g' $(egrep -R '/tmp' ./ |awk -F : '{print $1}')
popd

# Module file
# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
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

set     version             %{version}

# Require petsc

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
        if { ![is-loaded petsc]  } {
          module load petsc
        }
}

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_ARCH       ""
EOF

%clean
rm -rf ${RPM_BUILD_ROOT}

%files 
%defattr(-,root,root,-)
%{OHPC_PUB}
%{OHPC_HOME}
%doc COPYING COPYING.LESSER README docs/slepc.pdf

%changelog
