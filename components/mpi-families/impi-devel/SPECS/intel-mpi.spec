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
%define gnu_major_ver 12
%define oneapi_manifest %{OHPC_MODULEDEPS}/intel/impi/.rpm-manifest
# Using a minimum version has been problematic as DNF will happily
# install newer versions during build time. If the user has the minimum
# version, but the build system was already using a newer version, then
# the resulting binaries might rely on symbols which are not present
# in the minimum version. Newer versions may still be installed in parallel.
%define exact_intel_ver 2021.9.0

Summary:   OpenHPC compatibility package for Intel(R) oneAPI MPI Library
Name:      %{pname}%{PROJ_DELIM}
Version:   2023.1
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/mpi-families
BuildArch: x86_64
AutoReq:   no

#!BuildIgnore: post-build-checks

Requires: sed
Requires(pre): intel-compilers-devel%{PROJ_DELIM} = %{version}
Requires(pre): intel-oneapi-mpi-devel-%{exact_intel_ver}
Requires: intel-oneapi-mpi-devel-%{exact_intel_ver}
Requires: intel-compilers-devel%{PROJ_DELIM} = %{version}
Requires: prun%{PROJ_DELIM}

%description
Provides OpenHPC-style compatible modules for use with the oneAPI
MPI Library.


%prep
%build


%install
# Module directories
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/intel/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu/impi
mkdir -p %{buildroot}/%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi

%pre
if ! [ -d /opt/intel/oneapi/mpi/latest/modulefiles ]; then
    echo " "
    echo "Error: Unable to detect the oneAPI MPI installation at /opt/intel."
    echo " "
    exit 1
fi


%post
# Don't clobber/overwrite existing files
# Write the new file .rpmnew
testfile () {
    if [ -e $1 ]; then
       echo "$1.rpmnew"
    else
       echo "$1"
    fi
}

# Create an OpenHPC module file for each version found in compilers
rm -f %{oneapi_manifest}

# Regenerate the oneAPI modules directory (since MPI may have just been added)
echo "Generating new oneAPI modulefiles"
/opt/intel/oneapi/modulefiles-setup.sh --ignore-latest --force --output-dir=%{OHPC_MODULEDEPS}/oneapi/ > /dev/null

# Create an OpenHPC module file for each MPI version found
echo "Creating OpenHPC-style modulefiles for local oneAPI MPI installation(s)."
for mpis in %{OHPC_MODULEDEPS}/oneapi/mpi/2*; do
    ver=$(basename "$mpis")
    echo "--> Installing modulefile for version=${ver}"
    modname=$(testfile %{OHPC_MODULEDEPS}/intel/impi/$ver)

    # Get value for MPI_DIR
    eval $(%{OHPC_ADMIN}/lmod/lmod/libexec/lmod --expert use %{OHPC_MODULEDEPS}/oneapi)
    eval $(%{OHPC_ADMIN}/lmod/lmod/libexec/lmod --expert load mpi/$ver)
    MPIDIR=$I_MPI_ROOT
    eval $(%{OHPC_ADMIN}/lmod/lmod/libexec/lmod --expert unload mpi/$ver)
    eval $(%{OHPC_ADMIN}/lmod/lmod/libexec/lmod --expert unuse %{OHPC_MODULEDEPS}/oneapi)

    cat << EOF > ${modname}
#%Module1.0#####################################################################

set version "$ver"

proc ModulesHelp { } {
global version
puts stderr "\nThis module loads the Intel MPI environment.\n"
puts stderr "   mpiifort  (Fortran source)"
puts stderr "   mpiicc    (C   source)"
puts stderr "   mpiicpc   (C++ source)"
puts stderr "\nVersion \$version\n"
}

module-whatis "Name: Intel MPI"
module-whatis "Version: \$version"
module-whatis "Category: library, runtime support"
module-whatis "Description: Intel MPI Library (C/C++/Fortran for x86_64)"
module-whatis "URL: http://software.intel.com/en-us/articles/intel-mpi-library"

# For convenience, redirect standard mpicc/mpicxx/mpifort
# to use oneAPI icc/icpc/ifort instead of gcc/g++/gfortran
setenv I_MPI_CC   icx
setenv I_MPI_CXX  icpx
setenv I_MPI_FC   ifx
setenv I_MPI_F77  ifx
setenv I_MPI_F90  ifx

setenv MPI_DIR    "$MPIDIR"

prepend-path      MODULEPATH       %{OHPC_MODULEDEPS}/oneapi
prepend-path      MODULEPATH       %{OHPC_MODULEDEPS}/intel-impi

module load "mpi/\$version"

family "MPI"
EOF

    md5sum ${modname} >> %{oneapi_manifest}

    modname=$(testfile %{OHPC_MODULEDEPS}/intel/impi/.version.$ver)

    cat << EOF > ${modname}
#%Module1.0#####################################################################
set     ModulesVersion      "$ver"
EOF

    md5sum ${modname} >> %{oneapi_manifest}

    modname=$(testfile %{OHPC_MODULEDEPS}/gnu/impi/$ver)

    cat << EOF > ${modname}
#%Module1.0#####################################################################

set version "$ver"

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

setenv MPI_DIR    "$MPIDIR"

prepend-path    MODULEPATH      %{OHPC_MODULEDEPS}/oneapi
prepend-path    MODULEPATH      %{OHPC_MODULEDEPS}/gnu-impi

module load "mpi/\$version"

family "MPI"
EOF

    md5sum ${modname} >> %{oneapi_manifest}

    # support for gnu major version
    orig_modname=$modname
    modname=$(testfile  %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/$ver)
    cp ${orig_modname} ${modname}
    sed -i "s,%{OHPC_MODULEDEPS}/gnu-impi,%{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}-impi," ${modname}
    md5sum ${modname} >> %{oneapi_manifest}

    modname=$(testfile %{OHPC_MODULEDEPS}/gnu/impi/.version.$ver)

    cat << EOF > ${modname}
#%Module1.0#####################################################################
set     ModulesVersion      "$ver"
EOF

    md5sum ${modname} >> %{oneapi_manifest}

    # support for gnu major version
    orig_modname=$modname
    modname=$(testfile  %{OHPC_MODULEDEPS}/gnu%{gnu_major_ver}/impi/.version.$ver)
    cp ${orig_modname} ${modname}
    md5sum ${modname} >> %{oneapi_manifest}
done


%preun -p /bin/bash
# Check current files against the manifest
# Remove files that match and backup files that don't
if [ -s %{oneapi_manifest} ]; then
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
