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

%define pname intel-mpi-devel
%define year 2020
%global gnu_major_ver 9

Summary:   OpenHPC compatibility package for Intel(R) MPI Library
Name:      %{pname}%{PROJ_DELIM}
Version:   %{year}
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
Source1:   mod_generator_impi.sh
AutoReq:   no

#!BuildIgnore: post-build-checks

Requires: prun%{PROJ_DELIM}

%description

Provides OpenHPC-style compatible modules for use with the Intel(R) MPI Library
suite.

%prep

%build

%install
install -D -m755 %{SOURCE1}  $RPM_BUILD_ROOT/%{OHPC_ADMIN}/compat/modulegen/mod_generator_impi.sh
%{__mkdir} -p %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi
%{__mkdir} -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi
%{__mkdir} -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi

%pre

# Verify psxe mpi stack is installed. Punt if not detected.

mpicc_subpath="linux/mpi/intel64/bin/mpicc$"

echo "Checking for local PSXE MPI installation(s)."

versions_all=`rpm -qal | grep ${mpicc_subpath}`

if [ $? -eq 1 ];then
    echo ""
    echo "Error: Unable to detect local Parallel Studio installation. The toolchain"
    echo "       providing ${mpicc_subpath} must be installed prior to this compatability package"
    echo " "
    exit 1
fi

# Verify min version expectations
min_ver="2019"
versions=""
for file in ${versions_all}; do 
    version=`rpm -q --qf '%%{VERSION}.%%{RELEASE}\n' -f ${file}`
    echo "--> Version ${version} detected"
    echo -e "${version}\n${min_ver}" | sort -V | head -n 1 | grep -q "^${min_ver}"
    if [ $? -ne 0 ];then
        echo "Warning: skipping version ${version}"
    else
        versions="${versions} ${version}"
    fi
done
if [ -z "${versions}" ]; then
    echo ""
    echo "Error: local PSXE compatability support is for versions > ${min_ver}"
    echo ""
    exit 1
fi

%post

mpicc_subpath="linux/mpi/intel64/bin/mpicc"
scanner=%{OHPC_ADMIN}/compat/modulegen/mod_generator_impi.sh

versions=`rpm -qal | grep ${mpicc_subpath}$`

if [ $? -eq 1 ];then
    echo ""
    echo "Error: Unable to detect local Parallel Studio installation. The toolchain"
    echo "       providing ${mpicc_subpath} must be installed prior to this compatability package"
    exit 1
fi

echo "Creating OpenHPC-style modulefiles for local PSXE MPI installation(s)."

# Create modulefiles for each locally detected installation.

for file in ${versions}; do

    version=`rpm -q --qf '%%{VERSION}.%%{RELEASE}\n' -f ${file}`
    topDir=`echo $file | sed "s|$mpicc_subpath||"`
    echo "--> Installing modulefile for MPI version=${version}"
	    
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
    ${scanner} ${topDir}/linux/mpi/intel64/bin/mpivars.sh  >> %{OHPC_MODULEDEPS}/intel/impi/${version} || exit 1

    # Also define MPI_DIR based on $I_MPI_ROOT
    IMPI_DIR=`egrep "^setenv\s+I_MPI_ROOT"  %{OHPC_MODULEDEPS}/intel/impi/${version} | awk '{print $3}'`
    if [ -d "$IMPI_DIR/intel64" ];then
	echo "setenv          MPI_DIR        $IMPI_DIR/intel64" >> %{OHPC_MODULEDEPS}/intel/impi/${version}
    fi

    # Change typical MPI compiler commands to use the Intel compilers by default

    %{__cat} << EOF >> %{OHPC_MODULEDEPS}/intel/impi/${version}
setenv          I_MPI_CC        "icc"
setenv          I_MPI_CXX       "icpc"
setenv          I_MPI_FC        "ifort"
setenv          I_MPI_F77       "ifort"
setenv          I_MPI_F90       "ifort"
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
    ${scanner} ${topDir}/linux/mpi/intel64/bin/mpivars.sh  >> %{OHPC_MODULEDEPS}/gnu/impi/${version} || exit 1

    # Version file
    %{__cat} << EOF > %{OHPC_MODULEDEPS}/gnu/impi/.version.${version}
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF

    # Also define MPI_DIR based on $I_MPI_ROOT
    IMPI_DIR=`egrep "^setenv\s+I_MPI_ROOT"  %{OHPC_MODULEDEPS}/intel/impi/${version} | awk '{print $3}'`
    if [ -d "$IMPI_DIR/intel64" ];then
	echo "setenv          MPI_DIR        $IMPI_DIR/intel64" >> %{OHPC_MODULEDEPS}/gnu/impi/${version}
    fi

    # support for additional gnu variants
    %{__cp} %{OHPC_MODULEDEPS}/gnu/impi/${version} %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/${version}
    %{__cp} %{OHPC_MODULEDEPS}/gnu/impi/.version.${version} %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/.version.${version}
    perl -pi -e 's!moduledeps/gnu-impi!moduledeps/gnu%{gnu_major_ver}-impi!' %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/${version}
    
    # Inventory for later removal
    echo "%{OHPC_MODULEDEPS}/intel/impi/${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
    echo "%{OHPC_MODULEDEPS}/intel/impi/.version.${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest   
    echo "%{OHPC_MODULEDEPS}/gnu/impi/${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
    echo "%{OHPC_MODULEDEPS}/gnu/impi/.version.${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
    echo "%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest
    echo "%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/.version.${version}" >> %{OHPC_MODULEDEPS}/intel/impi/.manifest   

done


%postun
if [ "$1" = 0 ]; then

    mpicc_subpath="linux/mpi/intel64/bin/mpicc"
    versions=`rpm -qal | grep ${mpicc_subpath}$`

    for file in ${versions}; do
	version=`rpm -q --qf '%%{VERSION}.%%{RELEASE}\n' -f ${file}`
	topDir=`echo $file | sed "s|$mpicc_subpath||"`
    done

    if [ -s %{OHPC_MODULEDEPS}/intel/impi/.manifest ];then
	for file in `cat %{OHPC_MODULEDEPS}/intel/impi/.manifest`; do
	   if [ -e $file ];then
               rm $file
	   fi
	done
	rm -f %{OHPC_MODULEDEPS}/intel/impi/.manifest
    fi
fi

%files
%{OHPC_ADMIN}/compat/modulegen/mod_generator_impi.sh
%{OHPC_MODULEDEPS}
