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

Summary:   OpenHPC compatability package for Intel(R) MPI Library
Name:      intel-mpi-devel%{PROJ_DELIM}
Version:   2016
Source1:   OHPC_macros
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no

Requires: prun%{PROJ_DELIM}
Requires: intel-psxe-common
Requires: intel-mpi-doc
Requires: intel-compilers-devel%{PROJ_DELIM}
%if 0%{?OHPC_BUILD}
BuildRequires: intel-compilers-devel%{PROJ_DELIM}
%endif

%description

Provides OpenHPC-style compatible modules for use with the Intel(R) MPI Library
suite.

%prep

%build

%install
%{__mkdir} -p %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi

%post

topDir=`rpm -q --qf '%{FILENAMES}\n' intel-mpi-doc` || exit 1

echo " "
echo "Scanning top-level dir = $topDir"

if [ -d ${topDir} ];then
    versions=`find ${topDir} -maxdepth 1 -type d -name "compilers_and_libraries_*" -printf "%f "` || exit 1

    scanner=%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh

    for dir in ${versions}; do
	if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc ];then
	    version=`grep "^MPIVERSION=" ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc | cut -d '"' -f2`
	    if [ -z "${version}" ];then
		echo "Error: unable to determine MPI version"
		exit 1
	    fi
	    
	    echo "--> Installing OpenHPC-style modulefile for MPI version=${version}"

	    # Create soft links for standard MPI wrapper usage

	    ohpc_path=${topDir}/${dir}/linux/mpi/intel64/bin_ohpc

	    %{__mkdir_p} ${ohpc_path} || exit 1
	    if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc ];then
		%{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc ${ohpc_path}/mpicc
	    fi
	    if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicpc ];then
		%{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicpc ${ohpc_path}/mpicxx
	    fi
	    if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiifort ];then
		%{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiifort ${ohpc_path}/mpif90
		%{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiifort ${ohpc_path}/mpif77
	    fi
	    
	    # Module header

	    %{__cat} << EOF > %{OHPC_MODULEDEPS}/intel/impi/${version}
#%Module1.0#####################################################################
proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the Intel MPI environment"
puts stderr " "
puts stderr "mpiifort     (Fortran source)"
puts stderr "mpiicc       (C   source)"
puts stderr "mpiicpc      (C++ source)"
puts stderr " "
puts stderr "Version %{version}"
puts stderr " "

}

module-whatis "Name: Intel MPI"
module-whatis "Version: %{version}"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library/"

set     version                 ${version}

prepend-path    MODULEPATH      %{OHPC_MODULEDEPS}/intel-impi
 
# Prefer bin_ohpc to allow developers to use standard mpicc, mpif90,
# etc to access Intel toolchain.
 
prepend-path    PATH            ${topDir}/${dir}/linux/mpi/intel64/bin_ohpc

family "MPI"
EOF

	    # Append with environment vars parsed directlry from mpivars.sh
	    ${scanner} ${topDir}/${dir}/linux/mpi/intel64/bin/mpivars.sh  >> %{OHPC_MODULEDEPS}/intel/impi/${version} || exit 1
	    
	    # Version file
	    %{__cat} << EOF > %{OHPC_MODULEDEPS}/intel/impi/.version.${version}
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF
	    
	fi
    done
fi

# OpenHPC module file for GNU compiler toolchain
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi
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
%{OHPC_MODULEDEPS}

%changelog

