#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family variable via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?OHPC_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-ohpc-header-comp-end------------------------------------------------

# Base package name
%define pname cube
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name: %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

Version:   4.3.4
Release:   1%{?dist}
Summary:   Score-P and Scalasca performance report explorer
License:   BSD-style license 
Group:     ohpc/perf-tools
Url:       http://www.scalasca.org/software/cube-4.x/download.html
Source0:   http://apps.fz-juelich.de/scalasca/releases/cube/4.3/dist/cube-%{version}.tar.gz
Provides:  lib%PNAME.so()(64bit)
Provides:  perl(ebs2otf)
Conflicts: lib%pname < %version-%release
Obsoletes: lib%pname < %version-%release
DocDir:    %{OHPC_PUB}/doc/contrib


BuildRequires: qt4-devel 
Requires: qt4
BuildRequires: qt4-devel zlib-devel

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
 Cube, which is used as performance report explorer for Scalasca and Score-P, 
is a generic tool for displaying a multi-dimensional performance space consisting 
of the dimensions (i) performance metric, (ii) call path, and (iii) system 
resource. Each dimension can be represented as a tree, where non-leaf nodes 
of the tree can be collapsed or expanded to achieve the desired level of 
granularity. In addition, Cube can display multi-dimensional Cartesian 
process topologies.

The Cube 4.x series report explorer and the associated Cube4 data format is 
provided for Cube files produced with the Score-P performance instrumentation 
and measurement infrastructure or the Scalasca version 2.x trace analyzer 
(and other compatible tools). However, for backwards compatibility, 
Cube 4.x can also read and display Cube 3.x data. 


%prep
%setup -q -n %{pname}-%{version}


%build
# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi
module load qt

export BUILDROOT=%buildroot%{install_path}
./configure \
    -prefix=/tmp/%{install_path} \

make install
make exports


rm -rf %buildroot
mkdir -p %buildroot%{install_path}
pushd /tmp
export tmp_path=%{install_path}
mv ${tmp_path#*/} %buildroot%{install_path}/..
popd
pushd %{buildroot}%{install_path}/bin
sed -i 's|/tmp/||g' $(egrep -IR '/tmp/' ./|awk -F : '{print $1}')
popd


rm -rf %{install_path}/examples
rm -rf %buildroot%{install_path}/examples
rm -f %{install_path}/.last_config
rm -f %{install_path}/.all_configs
rm -f %{install_path}/.active_stub*


# clean libs
pushd %buildroot%{install_path}/lib
popd


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
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_MAKEFILE   %{install_path}/include/Makefile

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded papi]  } {
        module load papi
    }
    if {  ![is-loaded pdtoolkit]  } {
        module load pdtoolkit
    }
}

if [ module-info mode remove ] {
    module unload pdtoolkit
}

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc Changes COPYRIGHT CREDITS INSTALL LICENSE README*

%changelog

