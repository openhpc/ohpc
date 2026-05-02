#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname tau

Name: %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

Version:   2.35.1
Release:   1%{?dist}
Summary:   Tuning and Analysis Utilities Profiling Package
License:   Tuning and Analysis Utilities License
Group:     %{PROJ_NAME}/perf-tools
Url:       http://www.cs.uoregon.edu/research/tau/home.php
# upstream https certificate is broken, use http
Source0:   http://www.cs.uoregon.edu/research/tau/tau_releases/tau-%{version}.tar.gz
Patch1:    tau-2.34.1.patch

Provides:  lib%{PNAME}.so()(64bit)(%{PROJ_NAME})
Provides:  perl(ebs2otf)
Conflicts: lib%{pname} < %{version}-%{release}
Obsoletes: lib%{pname} < %{version}-%{release}

%if 0%{?sle_version}
BuildRequires: libgomp1
%else
BuildRequires: libgomp
%endif

%ifarch x86_64
BuildRequires: papi%{PROJ_DELIM}
Requires: papi%{PROJ_DELIM}
%endif

BuildRequires: curl
BuildRequires: chrpath sed grep which make
BuildRequires: postgresql-devel binutils-devel
BuildRequires: zlib-devel python3-devel
BuildRequires: otf2-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}

# Exclude requires that breaks install
%if "0%{?__requires_exclude}" == "0"
%global __requires_exclude ^libCg.*$|.*SUNWprivate.*
%else
%global __requires_exclude %{__requires_exclude}|^libCg.*$|.*SUNWprivate.*
%endif

Requires: lmod%{PROJ_DELIM} >= 7.6.1
Requires: otf2-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}
Requires: binutils-devel
Requires: java
Requires: python3
Requires: perl

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version
%define module_path %{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}

%description
TAU is a program and performance analysis tool framework being developed for
the DOE and ASC program at University of Oregon. TAU provides a suite of
static and dynamic tools that provide graphical user
interaction and interoperation to form an integrated analysis environment for
parallel Fortran 95, C and C++ applications.  In particular, a robust
performance profiling facility available in TAU has been applied extensively in
the ACTS toolkit.  Also, recent advancements in TAU's code analysis
capabilities have allowed new static tools to be developed, such as an
automatic instrumentation tool.


%prep
%setup -q -n %{pname}-%{version}
%patch -P 1 -p1

%ifarch x86_64
sed -i -e 's/^BITS.*/BITS = 64/' src/Profile/Makefile.skel
%endif

# Fix errors with unversioned python shebangs
# and change others to preferred usage
# Need to also pre-capture generated files
for f in $(grep -Ilr "#! */usr/bin/env" *); do
   sed -i "s,n/env python\b,n/python3,g" $f
   sed -i "s,n/env python3,n/python3,g" $f
   sed -i "s,n/env sh,n/sh,g" $f
   sed -i "s,n/env perl,n/perl,g" $f
done

# Fix to use the correct compilers
sed -e "s,/usr/bin/g++,g++,g" -i utils/include/Makefile.skel
sed -e "s,/usr/bin/gcc,gcc,g" -i utils/include/Makefile.skel


%build
export TAUROOT=$(pwd)
export INSTALLROOT=${TAUROOT}/TAUBUILD%{install_path}

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
%ifarch x86_64
module load papi
%endif
module load otf2
module load pdtoolkit

export OMPI_LDFLAGS="-Wl,--as-needed -L${MPI_LIB_DIR}"
export FFLAGS="$FFLAGS -I${MPI_INCLUDE_DIR}"

%if "%{compiler_family}" == "arm1"
export CFLAGS="${CFLAGS} -fsimdmath"
%endif
%if "%{compiler_family}" == "intel"
export CFLAGS="${CFLAGS} -Wno-register"
%endif




# Set up ccache via PATH masquerade if enabled.
# TAU's configure validates -cc/-c++ against a whitelist, so we cannot
# prepend ccache directly. Instead, create symlinks for the MPI compiler
# wrappers in PATH so that TAU's build invocations go through ccache.
%if "%{?OHPC_USE_CCACHE}" == "yes"
if command -v ccache >/dev/null 2>&1; then
    CCACHE_WRAP_DIR=$(mktemp -d /tmp/ccache-wrap.XXXXXX)
    ln -s "$(command -v ccache)" "${CCACHE_WRAP_DIR}/mpicc"
    ln -s "$(command -v ccache)" "${CCACHE_WRAP_DIR}/mpicxx"
    export PATH="${CCACHE_WRAP_DIR}:${PATH}"
fi
%endif

# Try and figure out architecture
if [ -n "$detectarch" ]; then
   CONFIG_ARCH="${detectarch}"
else
   CONFIG_ARCH=$(./utils/archfind)
   if [ "x${CONFIG_ARCH}" = "x" ]; then
      CONFIG_ARCH="unknown"
   fi
fi
export CONFIG_ARCH

# Build the final package directly in the source root
mkdir -p ${TAUROOT}/TAUBUILD

./configure \
    -arch=${CONFIG_ARCH} \
    -prefix=${INSTALLROOT} \
    -exec-prefix= \
    -iowrapper \
    -mpi \
    -mpiinc=${MPI_INCLUDE_DIR} \
    -mpilib=${MPI_LIB_DIR} \
%if "%{compiler_family}" == "intel"
    -fortran=ifx \
    -oparicomp=oneapi \
%else
%if "%{compiler_family}" == "arm1"
    -fortran=armflang \
%else
    -fortran=gfortran \
%endif
%endif
    -opari \
    -c++=mpicxx \
    -cc=mpicc \
%ifarch x86_64
    -papi=${PAPI_DIR} \
%endif
    -otf=${OTF2_DIR} \
    -slog2 \
    -CPUTIME \
    -PROFILE \
    -PROFILECALLPATH \
    -PROFILEPARAM \
    -pdt=${PDTOOLKIT_DIR} \
    -useropt="${CFLAGS} -I${MPI_INCLUDE_DIR} -fno-strict-aliasing" \
    -openmp \
    -pthread \
    -mpit \
    -COMPENSATE \
    -extrashlibopts="-fPIC -L${MPI_LIB_DIR} -lmpi -L${INSTALLROOT}/lib"

make %{?_smp_mflags}

%install
export TAUROOT=$(pwd)
export INSTALLROOT=${TAUROOT}/TAUBUILD%{install_path}

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
%ifarch x86_64
module load papi
%endif
module load otf2
module load pdtoolkit

make install

# remove static libs and directories
find ${INSTALLROOT}/lib -name '*.a' -delete
find ${INSTALLROOT}/lib -type d -name 'static-*' -delete

# Replace hard paths with env vars and remove absolute BUILDDIR path
# Use find/replace function -- easy to add more as needed
# Two stage, using much faster grep to target specific files
replace_all() {
    for f in $(grep -Ilr -- "$1" ${INSTALLROOT}); do
      sed -i "s|$1|$2|g" $f
    done
}

replace_all "ccache " ""
replace_all "${TAUROOT}/TAUBUILD" ""
replace_all "${TAUROOT}" ""
replace_all "OPARIINCDIR=-I/include" "OPARIINCDIR=-I%{install_path}/include"
replace_all "${MPI_DIR}" "\${MPI_DIR}"
replace_all "${OTF2_DIR}" "\${OTF2_DIR}"
replace_all "${PDTOOLKIT_DIR}" "\${PDTOOLKIT_DIR}"
replace_all "-fstack-protector-strong" ""
%if "%{mpi_family}" == "impi"
replace_all "${I_MPI_ROOT}" "\${I_MPI_ROOT}"
%endif
%ifarch x86_64
replace_all "${PAPI_DIR}" "\${PAPI_DIR}"
replace_all "/x86_64/lib" "/lib"
%endif

# Fix absolute symlinks that point into the build directory
find ${INSTALLROOT} -type l | while read link; do
    target=$(readlink "$link")
    case "$target" in
        ${TAUROOT}/TAUBUILD*)
            relTarget=$(basename "$target")
            ln -sf "$relTarget" "$link"
            ;;
    esac
done

# Remove RUNPATH entries. Use LMOD environment config instead.
find ${INSTALLROOT}/lib -type f -name '*.so' -exec chrpath -d {} \;

# Create shared-mpi binding symlink for tau_exec and libTauMpi convenience symlinks
cd ${INSTALLROOT}/lib
ln -s shared-callpath-* ${INSTALLROOT}/lib/shared-mpi
for f in libTAUsh-callpath-*.so; do
  ln -s $f ${INSTALLROOT}/lib/${f/libTAUsh/libTauMpi}
done


mkdir -p %{buildroot}/%{_docdir}

# Copy the install tree to BUILDROOT
cp -a ${TAUROOT}/TAUBUILD/* %{buildroot}

# OpenHPC module file
mkdir -p %{buildroot}%{module_path}
cat << EOF > %{buildroot}/%{module_path}/%{version}.lua
help([[
This module loads the %{pname} library built with the %{compiler_family}
compiler toolchain and the %{mpi_family} MPI stack.

Version %{version}
]])

whatis("Name: %{pname} built with %{compiler_family} compiler")
whatis("Version: %{version}")
whatis("Category: runtime library")
whatis("Description: %{summary}")
whatis("URL %{url}")

local version = "%{version}"

prepend_path("PATH",            "%{install_path}/bin")
prepend_path("MANPATH",         "%{install_path}/man")
prepend_path("INCLUDE",         "%{install_path}/include")
prepend_path("LD_LIBRARY_PATH", "%{install_path}/lib")

setenv("%{PNAME}_DIR",      "%{install_path}")
setenv("%{PNAME}_BIN",      "%{install_path}/bin")
setenv("%{PNAME}_LIB",      "%{install_path}/lib")
setenv("%{PNAME}_INC",      "%{install_path}/include")
setenv("%{PNAME}_MAKEFILE", "%{install_path}/include/Makefile")
setenv("%{PNAME}_OPTIONS",  "-optRevert -optShared -optNoTrackGOMP")

depends_on("otf2")
depends_on("pdtoolkit")
EOF

%ifarch x86_64
echo 'depends_on("papi")' >> %{buildroot}/%{module_path}/%{version}.lua
%endif

# Set default version
ln -s %{version}.lua %{buildroot}/%{module_path}/default


%files
%{install_path}
%{module_path}
%doc Changes CREDITS INSTALL README*
%license COPYRIGHT LICENSE
