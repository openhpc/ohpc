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
%define gnu_major_ver 14
%define oneapi_manifest %{OHPC_MODULEDEPS}/intel/impi/.rpm-manifest
%define psxe_manifest %{OHPC_MODULEDEPS}/intel/impi/.manifest
# Using a minimum version has been problematic as DNF will happily
# install newer versions during build time. If the user has the minimum
# version, but the build system was already using a newer version, then
# the resulting binaries might rely on symbols which are not present
# in the minimum version. Newer versions may still be installed in parallel.
%define exact_mpi_ver 2021.11
%define exact_mkl_ver 2024.0
%define exact_deps compiler/2024.0.0 mkl/%{exact_mkl_ver} oclfpga/2024.0.0 compiler-rt/2024.0.0 debugger/2024.0.0 tbb/2021.11

Summary:   OpenHPC compatibility package for Intel(R) oneAPI MPI Library
Name:      %{pname}%{PROJ_DELIM}
Version:   2024.0
Release:   %{?dist}.2
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
AutoReq:   no

Source1:   mod_generator_impi.sh
Source2:   ohpc-update-modules-mpi

#!BuildIgnore: post-build-checks

Requires: sed
Requires(pre): intel-compilers-devel%{PROJ_DELIM} = %{version}
Requires(pre): intel-oneapi-mpi-devel-%{exact_mpi_ver}
Requires: intel-oneapi-mpi-devel-%{exact_mpi_ver}
Requires: intel-compilers-devel%{PROJ_DELIM} = %{version}
Requires: prun%{PROJ_DELIM}

%description
Provides OpenHPC-style compatible modules for use with the oneAPI
MPI Library.


%prep
%build


%install
# Mod generator for PSXE support
install -D -m755 %{SOURCE1}  %{buildroot}/%{OHPC_ADMIN}/compat/modulegen/mod_generator_impi.sh

# Mod generator for oneAPI support
sed -e 's|@@oneapi_manifest@@|%{oneapi_manifes}|' \
    -e 's|@@OHPC_ADMIN@@|%{OHPC_ADMIN}|' \
    -e 's|@@OHPC_MODULEDEPS@@|%{OHPC_MODULEDEPS}|' \
    -e 's|@@OHPC_MODULES@@|%{OHPC_MODULES}|' \
    -e 's|@@exact_deps@@|%{exact_deps}|' \
    -e 's|@@exact_mpi_ver@@|%{exact_mpi_ver}|' \
    -e 's|@@gnu_major_ver@@|%{gnu_major_ver}|' %{SOURCE3} > ohpc-update-modules-mpi
install -D -m755 ohpc-update-modules-mpi %{buildroot}/%{OHPC_BIN}/ohpc-update-modules-mpi

# Module directories
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi


# Regenerate module files when components are added or removed
%transfiletriggerin -- /opt/intel/oneapi/mpi
%{OHPC_BIN}/ohpc-update-modules-mpi


%pre
if ! [ -d /opt/intel/oneapi/mpi/%{exact_mpi_ver}/etc/modulefiles ]; then
    echo " "
    echo "Error: Unable to detect the oneAPI MPI installation at /opt/intel."
    echo " "
    exit 1
fi


%post
%{OHPC_BIN}/ohpc-update-modules-mpi

%preun -p /bin/bash
# Check current files against the manifest
# Remove files that match and backup files that don't
if [ -s %{oneapi_manifest} ]; then
    echo "Removing module files"
    while IFS= read -r line; do
       f=${line%:*}
       s=${line#*:}
       if [ "${s: -2}" = "OK" ]; then
           rm -f $f
       elif [ -f $f ]; then
           mv -T -f $f $f.rpmsave
       fi
    done <<< $(md5sum --check %{oneapi_manifest})
else
    # Don't touch any generated files if there's no manifest
    echo "WARNING: Manifest not found. Previously generated"
    echo "         modulefiles will not be removed or moved."
fi


%files
%dir %{OHPC_MODULEDEPS}/intel/impi
%dir %{OHPC_MODULEDEPS}/gnu/impi
%dir %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi
%ghost %{oneapi_manifest}

###############################################################################

%define psxemod intel-psxe-mpi-devel%{PROJ_DELIM}

%package -n %{psxemod}
Summary: OpenHPC compatibility package for Intel(R) MPI Library

Obsoletes: %pname%{PROJ_DELIM} < %{version}
Requires: intel-psxe-compilers-devel%{PROJ_DELIM} = %{version}
Requires: prun%{PROJ_DELIM}
Requires: sed, gawk

%description -n %{psxemod}
Provides OpenHPC-style compatible modules for use with MPI Library in the
Intel(R) Parallel Studio suite.


%pre -n %{psxemod} -p /bin/bash
# Verify psxe mpi stack is installed. Punt if not detected.
echo "Checking for local PSXE MPI installation(s)."
icc_subpath="linux/mpi/intel64/bin/mpicc$"
versions_all=$(rpm -qal | grep "${icc_subpath}" | grep -v "oneapi")

if [ $? -eq 1 ];then
    echo ""
    echo "Error: Unable to detect local Parallel Studio MPI installation. The toolchain"
    echo "       providing ${icc_subpath} must be installed prior to this compatibility package"
    echo " "
    exit 1
fi

# Verify min version expectations
min_ver="2019"
declare -a versions=()
declare -a topDirs=()
declare -a mpiDirs=()
for file in ${versions_all}; do
    version=$(rpm -q --qf '%%{VERSION}.%%{RELEASE}\n' -f ${file})
    echo "--> Version ${version} detected"
    if [ "$min_ver" = "$(printf '${version}\n${min_ver}' | sort -V | head -n 1)" ]; then
        echo "Warning: ${version} < $min_ver, skipping"
    else
        versions+="${version}"
        topDirs+="$(echo $file | sed "s,/${icc_subpath},,")"
        mpiDirs+="${file%/bin/*}"
    fi
done

if [ -z "${versions}" ]; then
    echo ""
    echo "Error: local PSXE compatibility support is for versions > ${min_ver}"
    echo " "
    exit 1
fi

rm -rf %{_localstatedir}/lib/rpm-state/%{name}
mkdir -p %{_localstatedir}/lib/rpm-state/%{name}/
printf "%s\n" ${versions[@]} > %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-versions
printf "%s\n" ${topDirs[@]} > %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-dirs
printf "%s\n" ${mpiDirs[@]} > %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-mpis


%post -n %{psxemod} -p /bin/bash
scanner=%{OHPC_ADMIN}/compat/modulegen/mod_generator_impi.sh
declare -a versions=()
declare -a topDirs=()
declare -a mpiDirs=()
readarray -t versions < %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-versions
readarray -t topDirs < %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-dirs
readarray -t mpiDirs < %{_localstatedir}/lib/rpm-state/%{name}/rpm-install-mpis
rm -rf %{_localstatedir}/lib/rpm-state/%{name}

echo "Creating OpenHPC-style modulefiles for local PSXE MPI installation(s)."

# Create modulefiles for each locally detected installation.
rm -f %{psxe_manifest}

for (( x=0; x < ${#versions[@]}; x++ )); do
    topDir=${topDirs[$x]}
    mpiDir=${mpiDirs[$x]}
    version=${versions[$x]}
    echo "--> Installing modulefile for version=${version}"

    # Check for compilervars before writing a file
    if [ ! -e ${topDir}/linux/mpi/intel64/bin/mpivars.sh ]; then
	   echo "Error: Unable to access mpivars.sh"
           echo "       Skipping modules for (${topDir})"
	   break
    fi

    # Create alternate bin directory links
    ohpcDir=${mpiDir}/bin_ohpc
    mkdir -p ${ohpcDir}
    if [ -e ${mpiDir}/bin/mpiicc ]; then
	    ln -sf ${mpiDir}/bin/mpiicc ${ohpcDir}/mpicc
    fi
    if [ -e ${mpiDir}/bin/mpiicpc ]; then
	    ln -sf ${mpiDir}/bin/mpiicpc ${ohpcDir}/mpicxx
    fi
    if [ -e ${mpiDir}/bin/mpiifort ]; then
	    ln -sf ${mpiDir}/bin/mpiifort ${ohpcDir}/mpif90
	    ln -sf ${mpiDir}/bin/mpiifort ${ohpcDir}/mpif77
    fi

    echo "${ohpcDir}/" >> %{psxe_manifest}

    # Main module
    cat << EOF > %{OHPC_MODULEDEPS}/intel/impi/${version}
#%Module1.0#####################################################################

set version "${version}"

proc ModulesHelp { } {
global version
puts stderr "\nThis module loads the Intel MPI environment\n"
puts stderr "mpiifort     (Fortran source)"
puts stderr "mpiicc       (C   source)"
puts stderr "mpiicpc      (C++ source)"
puts stderr "\nVersion \$version\n"
}

module-whatis "Name: Intel MPI"
module-whatis "Version: \$version"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library/"

prepend-path   MODULEPATH      %{OHPC_MODULEDEPS}/intel-impi

setenv         MPI_DIR         $mpiDir

family "MPI"
EOF

    echo "%{OHPC_MODULEDEPS}/intel/impi/${version}" >> %{psxe_manifest}

    # Append with environment vars parsed directlry from mpivars.sh
    ${scanner} ${topDir}/linux/mpi/intel64/bin/mpivars.sh  >> %{OHPC_MODULEDEPS}/intel/impi/${version}
    if [ $? -ne 0 ]; then
        echo "ERROR: Could not generate content for %{OHPC_MODULEDEPS}/intel/impi/${version}"
        break
    fi

    # Prepend bin_ohpc
    cat << EOF >> %{OHPC_MODULEDEPS}/intel/impi/${version}

# Prefer bin_ohpc to allow developers to use standard mpicc, mpif90,
# etc to access Intel toolchain.
prepend-path    PATH            ${ohpcDir}

EOF

    # Version file
    cat << EOF > %{OHPC_MODULEDEPS}/intel/impi/.version.$version
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF

    echo "%{OHPC_MODULEDEPS}/intel/impi/.version.${version}" >> %{psxe_manifest}

    # OpenHPC module file for GNU compiler toolchain
    cat << EOF > %{OHPC_MODULEDEPS}/gnu/impi/${version}
#%Module1.0#####################################################################

set version "${version}"

proc ModulesHelp { } {
global version
puts stderr "\nThis module loads the Intel MPI environment for use with the GNU"
puts stderr "compiler toolchain\n"
puts stderr "mpif90       (Fortran source)"
puts stderr "mpicc        (C   source)"
puts stderr "mpicxx       (C++ source)"
puts stderr "\nVersion \$version\n"
}

module-whatis "Name: Intel MPI"
module-whatis "Version: \$version"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library/"

prepend-path   MODULEPATH      %{OHPC_MODULEDEPS}/gnu-impi
setenv         MPI_DIR         ${mpiDir}

family "MPI"
EOF

    echo "%{OHPC_MODULEDEPS}/gnu/impi/${version}" >> %{psxe_manifest}

    # Append with environment vars parsed directly from mpivars.sh
    ${scanner} ${topDir}/linux/mpi/intel64/bin/mpivars.sh  >> %{OHPC_MODULEDEPS}/gnu/impi/${version}
    if [ $? -ne 0 ]; then
        echo "ERROR: Could not generate content for %{OHPC_MODULEDEPS}/gnu/impi/${version}"
        break
    fi

    # Version file
    cat << EOF > %{OHPC_MODULEDEPS}/gnu/impi/.version.${version}
#%Module1.0#####################################################################
set     ModulesVersion      "${version}"
EOF

    echo "%{OHPC_MODULEDEPS}/gnu/impi/.version.${version}" >> %{psxe_manifest}

    # support for additional gnu variants
    cp %{OHPC_MODULEDEPS}/gnu/impi/${version} %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/${version}
    cp %{OHPC_MODULEDEPS}/gnu/impi/.version.${version} %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/.version.${version}
    sed -i "s,%{OHPC_MODULEDEPS}/gnu-impi,%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}-impi," %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/${version}

    echo "%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/${version}" >> %{psxe_manifest}
    echo "%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/.version.${version}" >> %{psxe_manifest}

done


%preun -n %{psxemod} -p /bin/bash
if [ -s %{psxe_manifest} ]; then
    while IFS= read -r file; do
        if [[ "$file" =~ "/bin_ohpc/" ]]; then
            rm -f $file/*
            rmdir $file
        fi
        if [ -e $file ]; then
            rm -f $file
        fi
	done < %{psxe_manifest}
else
        echo "WARNING: Manifest not found"
fi


%files -n %{psxemod}
%{OHPC_ADMIN}/compat/modulegen/mod_generator_impi.sh
%{OHPC_BIN}/ohpc-update-modules-mpi
%dir %{OHPC_MODULEDEPS}/intel/impi
%dir %{OHPC_MODULEDEPS}/gnu/impi
%dir %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi
%ghost %{psxe_manifest}
