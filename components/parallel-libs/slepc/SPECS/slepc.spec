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

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        3.10.2
Release:        1
Summary:        A library for solving large scale sparse eigenvalue problems
License:        LGPL-3.0
Group:          %{PROJ_NAME}/parallel-libs
Url:            http://slepc.upv.es
Source0:        http://slepc.upv.es/download/distrib/%{pname}-%{version}.tar.gz
BuildRequires:  petsc-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       petsc-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

# A configure script in slepc is made by python
BuildRequires: python

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

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
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
module load openblas
%endif
module load petsc
./configure --prefix=/tmp%{install_path}
make

%install
%ohpc_setup_compiler
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
module load openblas
%endif
module load petsc
make install

# move from tmp install dir to %install_path
# dirname removes the last directory
mkdir -p `dirname %{buildroot}%{install_path}`
pushd /tmp
export tmp_path=%{install_path}
mv ${tmp_path#*/} `dirname %{buildroot}%{install_path}`
popd

# clean up
pushd %{buildroot}%{install_path}
sed -i 's|/tmp||g' $(egrep -R '/tmp' ./ |\
egrep -v 'Binary\ file.*matches' |awk -F : '{print $1}')
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

depends-on petsc

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_ARCH       ""
EOF

%files
%{OHPC_PUB}
%doc LICENSE README docs/slepc.pdf
