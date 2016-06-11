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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define compiler_family intel

%{!?major_ver:  %define major_ver 16}
%{!?update_num: %define update_num 3}
%{!?build_id:   %define build_id 210}

Summary:   OpenHPC compatability package for Intel(R) MPI Library
Name:      intel-mpi-devel%{PROJ_DELIM}
Version:   2016
Source1:   OHPC_macros
Source2:   OHPC_mod_generator.sh
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no

BuildRequires: intel-comp-l-all-vars-223
BuildRequires: intel-compxe-pset
BuildRequires: intel-imb-223
BuildRequires: intel-mpi-doc
BuildRequires: intel-mpi-psxe-068
BuildRequires: intel-mpi-rt-core-223
BuildRequires: intel-mpi-rt-mic-223
BuildRequires: intel-mpi-sdk-core-223
BuildRequires: intel-mpi-sdk-mic-223
BuildRequires: intel-psxe-common
BuildRequires: intel-psxe-doc

Requires: prun%{PROJ_DELIM}
Requires: intel-psxe-common
Requires: intel-mpi-doc
Requires: intel-compilers-devel%{PROJ_DELIM}
%if 0%{?OHPC_BUILD}
Requires: intel-mpi-sdk-core-%{build_id}
%endif


%description

Provides OpenHPC-style compatible modules for use with the Intel(R) MPI Library
suite.

%prep

%build

%install

%post

### # Parse provided shell script to derive appropriate module settings
### %{__chmod} +x %{SOURCE2}
### %{SOURCE2} %{package_target}/%{composer_release}/linux/bin/compilervars.sh -arch intel64 -platform linux > modfile-ohpc.input
### 
### # OpenHPC module file for Intel compiler toolchain
### %{__mkdir} -p %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi
### %{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi/%{version}
### #%Module1.0#####################################################################
### proc ModulesHelp { } {
### 
### puts stderr " "
### puts stderr "This module loads the Intel MPI environment"
### puts stderr " "
### puts stderr "mpiifort     (Fortran source)"
### puts stderr "mpiicc       (C   source)"
### puts stderr "mpiicpc      (C++ source)"
### puts stderr " "
### puts stderr "Version %{version}"
### puts stderr " "
### 
### }
### 
### module-whatis "Name: Intel MPI"
### module-whatis "Version: %{version}"
### module-whatis "Category: library, runtime support"
### module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
### module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library/"
### 
### set     version                 %{version}
### 
### setenv          I_MPI_ROOT      %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi
### setenv          MPI_DIR         %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/intel64
### prepend-path    PATH            %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/intel64/bin
### prepend-path    MANPATH         %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/man
### prepend-path    LD_LIBRARY_PATH %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/intel64/lib
### 
### prepend-path    MODULEPATH      %{OHPC_MODULEDEPS}/intel-impi
### 
### # Prefer bin_ohpc to allow developers to use standard mpicc, mpif90,
### # etc to access Intel toolchain.
### 
### prepend-path    PATH            %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/intel64/bin_ohpc
### 
### 
### family "MPI"
### EOF
### 
### %{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi/.version.%{version}
### #%Module1.0#####################################################################
### set     ModulesVersion      "%{version}"
### EOF
### 
### # OpenHPC module file for GNU compiler toolchain
### mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi
### %{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi/%{version}
### #%Module1.0#####################################################################
### proc ModulesHelp { } {
### 
### puts stderr " "
### puts stderr "This module loads the Intel MPI environment for use with the GNU"
### puts stderr "compiler toolchain"
### puts stderr " "
### puts stderr "mpif90       (Fortran source)"
### puts stderr "mpicc        (C   source)"
### puts stderr "mpicxx       (C++ source)"
### puts stderr " "
### puts stderr "Version %{version}"
### puts stderr " "
### 
### }
### 
### module-whatis "Name: Intel MPI"
### module-whatis "Version: %{version}"
### module-whatis "Category: library, runtime support"
### module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
### module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library/"
### 
### set     version                 %{version}
### 
### setenv          I_MPI_ROOT      %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi
### setenv          MPI_DIR         %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/intel64
### prepend-path    PATH            %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/intel64/bin
### prepend-path    MANPATH         %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/man
### prepend-path    LD_LIBRARY_PATH %{package_target}/compilers_and_libraries_%{pstudio_ver}/linux/mpi/intel64/lib
### 
### prepend-path    MODULEPATH      %{OHPC_MODULEDEPS}/gnu-impi
### 
### family "MPI"
### EOF
### 
### %{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi/.version.%{version}
### #%Module1.0#####################################################################
### set     ModulesVersion      "%{version}"
### EOF
### 

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
#%{OHPC_HOME}

%changelog

