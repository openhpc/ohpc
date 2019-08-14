#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname ocr

# Build options

# --with mpi (disbled by default)
%bcond_with mpi

Summary:   Open Community Runtime (OCR) for shared memory
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   1.2.1
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/runtimes
URL:       https://xstack.exascale-tech.com/wiki
Source0:   OCRv%{version}_ohpc.tar.bz2

%description
The Open Community Runtime project is creating an application
building framework that explores new methods of high-core-count
programming. The initial focus is on HPC applications. Its goal
is to create a tool that helps app developers improve the power
efficiency, programmability, and reliability of their work
while maintaining app performance.

This version is for shared memory systems.

#!BuildIgnore: post-build-checks rpmlint-Factory

%if %{with mpi}
%package -n %{pname}_mpi-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
# MPI stuff for MPI version
# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi-devel%{PROJ_DELIM}
Requires:      intel-mpi-devel%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi3-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi3-%{compiler_family}%{PROJ_DELIM}
%endif
Summary:   Open Community Runtime (OCR) for clusters using MPI
Group:   %{PROJ_NAME}/runtimes
%description -n %{pname}_mpi-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
The Open Community Runtime project is creating an application
building framework that explores new methods of high-core-count
programming. The initial focus is on HPC applications. Its goal
is to create a tool that helps app developers improve the power
efficiency, programmability, and reliability of their work
while maintaining app performance.

This version is for clusters using MPI.
%endif # End of {with mpi}

%prep

%setup -q -n ocr

%build
cd ocr/build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if %{compiler_family} == intel
export CFLAGS="-fp-model strict $CFLAGS"
%endif

OCR_TYPE=x86 make %{?_smp_mflags} all
%if %{with mpi}
OCR_TYPE=x86-mpi make %{?_smp_mflags} all
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%install
cd ocr/build
# OpenHPC compiler designation
%ohpc_setup_compiler

mkdir -p $RPM_BUILD_ROOT/%{install_path}
make OCR_TYPE=x86 OCR_INSTALL=$RPM_BUILD_ROOT/%{install_path} %{?_smp_mflags} install
%if %{with mpi}
make OCR_TYPE=x86-mpi OCR_INSTALL=$RPM_BUILD_ROOT/%{install_path} %{?_smp_mflags} install
%endif
# Remove static libraries
find "%buildroot" -type f -name "*.la" -print0 | xargs -0 rm -f
find "%buildroot" -type f -name "*.a" -print0 | xargs -0 rm -f
# Add the spec
mkdir -p $RPM_BUILD_ROOT/%{install_path}/share/ocr/doc
cp ../spec/ocr-1.0.1.pdf $RPM_BUILD_ROOT/%{install_path}/share/ocr/doc


# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain for shared memory"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} for shared memory built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version		    %{version}

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          OCR_INSTALL         %{install_path}
setenv          OCR_TYPE            x86

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%if %{with mpi}
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}-mpi
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}-mpi/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} for clusters using MPI built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version		    %{version}

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          OCR_INSTALL         %{install_path}
setenv          OCR_TYPE            x86-mpi

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}-mpi/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-mpi-%{version}
##
set     ModulesVersion      "%{version}"
EOF
%endif

%files
%{OHPC_HOME}
%doc %{install_path}/share/ocr/doc/ocr-1.0.1.pdf
%exclude %{install_path}/bin/ocrrun_mpi
%exclude %{install_path}/lib/libocr_mpi.*
%exclude %{install_path}/share/ocr/config/x86-mpi
%exclude %{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}-mpi/.version.%{version}
%exclude %{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}-mpi/%{version}

%if %{with mpi}
%files -n %{pname}_mpi-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
%{OHPC_HOME}
%doc %{install_path}/share/ocr/doc/ocr-1.0.1.pdf
%exclude %{install_path}/bin/ocrrun_x86
%exclude %{install_path}/lib/libocr_x86.*
%exclude %{install_path}/share/ocr/config/x86
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
%endif
