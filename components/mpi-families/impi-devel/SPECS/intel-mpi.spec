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
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

%define pname intel-mpi-devel
%define year 2017

Summary:   OpenHPC compatibility package for Intel(R) MPI Library
Name:      %{pname}%{PROJ_DELIM}
Version:   %{year}
Source1:   OHPC_macros
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no

BuildRequires:-post-build-checks

Requires: prun%{PROJ_DELIM}
Requires: intel-mpi-doc >= %{year}
Requires: intel-compilers-devel%{PROJ_DELIM}
Provides: %{pname}%{PROJ_DELIM}

Provides: libmpi.so.12()(64bit)
Provides: libmpifort.so.12()(64bit)
Provides: libmpicxx.so.12()(64bit) 

%description

Provides OpenHPC-style compatible modules for use with the Intel(R) MPI Library
suite.

%prep

%build

%install

%post

topDir=`rpm -q --qf '%{FILENAMES}\n' intel-mpi-doc | head -1` || exit 1

echo " "
echo "Scanning top-level dir = $topDir"

if [ -d ${topDir} ];then
    versions=`find -L ${topDir} -maxdepth 1 -type d -name "compilers_and_libraries_*.*" -printf "%f "` || exit 1

    scanner=%{OHPC_ADMIN}/compat/modulegen/mod_generator.sh

    for dir in ${versions}; do
	if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc ];then
	    version=`grep "^MPIVERSION=" ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc | cut -d '"' -f2 | sed 's| Update |\.|'`
	    if [ -z "${version}" ];then
		echo "Error: unable to determine MPI version"
		exit 1
	    fi
	    
	    echo "--> Installing OpenHPC-style modulefile for MPI version=${version}"

	    # Create soft links for standard MPI wrapper usage

	    ohpc_path=${topDir}/${dir}/linux/mpi/intel64/bin_ohpc

	    %{__mkdir_p} ${ohpc_path} || exit 1
	    if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc ];then
		if [ ! -e ${ohpc_path}/mpicc ];then
		    %{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicc ${ohpc_path}/mpicc
		fi
	    fi
	    if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicpc ];then
		if [ ! -e ${ohpc_path}/mpicxx ];then
		    %{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiicpc ${ohpc_path}/mpicxx
		fi
	    fi
	    if [ -e ${topDir}/${dir}/linux/mpi/intel64/bin/mpiifort ];then
		if [ ! -e ${ohpc_path}/mpif90 ];then
		    %{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiifort ${ohpc_path}/mpif90
		fi
		if [ ! -e ${ohpc_path}/mpif77 ];then
		    %{__ln_s} ${topDir}/${dir}/linux/mpi/intel64/bin/mpiifort ${ohpc_path}/mpif77
		fi
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
puts stderr "Version ${version}"
puts stderr " "

}

module-whatis "Name: Intel MPI"
module-whatis "Version: ${version}"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library/"

set     version                 ${version}

prepend-path    MODULEPATH      %{OHPC_MODULEDEPS}/intel-impi

family "MPI"
EOF

	    # Append with environment vars parsed directlry from mpivars.sh
	    ${scanner} ${topDir}/${dir}/linux/mpi/intel64/bin/mpivars.sh  >> %{OHPC_MODULEDEPS}/intel/impi/${version} || exit 1

	    # Prepend bin_ohpc
	    %{__cat} << EOF >> %{OHPC_MODULEDEPS}/intel/impi/${version}
#
# Prefer bin_ohpc to allow developers to use standard mpicc, mpif90,
# etc to access Intel toolchain.
 
prepend-path    PATH            ${topDir}/${dir}/linux/mpi/intel64/bin_ohpc
EOF

	    # Version file
	    %{__cat} << EOF > %{OHPC_MODULEDEPS}/intel/impi/.version.${version}
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF
	
	    # OpenHPC module file for GNU compiler toolchain
	    %{__cat} << EOF > %{OHPC_MODULEDEPS}/gnu/impi/${version}
#%Module1.0#####################################################################
proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the Intel MPI environment for use with the GNU"
puts stderr "compiler toolchain"
puts stderr " "
puts stderr "mpif90       (Fortran source)"
puts stderr "mpicc        (C   source)"
puts stderr "mpicxx       (C++ source)"
puts stderr " "
puts stderr "Version ${version}"
puts stderr " "

}

module-whatis "Name: Intel MPI"
module-whatis "Version: ${version}"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library/"

set     version                 ${version}

prepend-path    MODULEPATH      %{OHPC_MODULEDEPS}/gnu-impi

family "MPI"
EOF

	    # Append with environment vars parsed directly from mpivars.sh
	    ${scanner} ${topDir}/${dir}/linux/mpi/intel64/bin/mpivars.sh  >> %{OHPC_MODULEDEPS}/gnu/impi/${version} || exit 1

	    # Version file
	    %{__cat} << EOF > %{OHPC_MODULEDEPS}/gnu/impi/.version.${version}
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF

	    # support for additional gnu variants
	    %{__cp} %{OHPC_MODULEDEPS}/gnu/impi/${version} %{OHPC_MODULEDEPS}/gnu7/impi/${version}
	    %{__cp} %{OHPC_MODULEDEPS}/gnu/impi/.version.${version} %{OHPC_MODULEDEPS}/gnu7/impi/.version.${version}

	    # Inventory for later removal
	    echo "%{OHPC_MODULEDEPS}/intel/impi/${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
	    echo "%{OHPC_MODULEDEPS}/intel/impi/.version.${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest   
	    echo "%{OHPC_MODULEDEPS}/gnu/impi/${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
	    echo "%{OHPC_MODULEDEPS}/gnu/impi/.version.${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
	    echo "%{OHPC_MODULEDEPS}/gnu7/impi/${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
	    echo "%{OHPC_MODULEDEPS}/gnu7/impi/.version.${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest   
	fi
    done
fi



%postun
if [ "$1" = 0 ]; then
    topDir=`rpm -q --qf '%{FILENAMES}\n' intel-mpi-doc` || exit 1

    if [ -d ${topDir} ];then
	versions=`find -L ${topDir} -maxdepth 1 -type d -name "compilers_and_libraries_*" -printf "%f "` || exit 1

	for dir in ${versions}; do
	    if [ -d ${topDir}/${dir}/linux/mpi/intel64/bin_ohpc ];then
		rm -rf ${topDir}/${dir}/linux/mpi/intel64/bin_ohpc
	    fi
	done
    fi

    for file in `cat %{OHPC_MODULEDEPS}/intel/impi/.manifest`; do
        rm $file
    done
    rm -f %{OHPC_MODULEDEPS}/intel/impi/.manifest
fi

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)

%changelog

