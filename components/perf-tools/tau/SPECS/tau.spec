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

Version:   2.30.1
Release:   1%{?dist}
Summary:   Tuning and Analysis Utilities Profiling Package
License:   Tuning and Analysis Utilities License
Group:     %{PROJ_NAME}/perf-tools
Url:       http://www.cs.uoregon.edu/research/tau/home.php
Source0:   https://www.cs.uoregon.edu/research/tau/tau_releases/tau-%{version}.tar.gz
Patch1:    tau-2.30.1-add-explicit-linking-option.patch
Patch2:    tau-2.30.1-shared_libpdb.patch
Patch3:    tau-2.30.1-disable_examples.patch
Patch4:    tau-2.30.1-ucontext.patch
Patch5:    tau-2.30.1-testplugins_makefile.patch
Patch6:    tau-2.30.1-paraprof.patch

Provides:  lib%PNAME.so()(64bit)(%PROJ_NAME)
Provides:  perl(ebs2otf)
Conflicts: lib%pname < %version-%release
Obsoletes: lib%pname < %version-%release

%if 0%{?suse_version} || 0%{?sle_version}
BuildRequires: libgomp1
%else
BuildRequires: libgomp
%endif

BuildRequires: curl chrpath
BuildRequires: sed grep
BuildRequires: postgresql-devel binutils-devel
Requires: binutils-devel
BuildRequires: zlib-devel python2-devel
Requires:      lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}
%ifarch x86_64
BuildRequires: papi%{PROJ_DELIM}
Requires: papi%{PROJ_DELIM}
%endif
Requires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
TAU is a program and performance analysis tool framework being developed for
the DOE and ASC program at University of Oregon. TAU provides a suite of
static and dynamic tools that provide graphical user
interaction and interoperation to form an integrated analysis environment for
parallel Fortran 95, C and C++ applications.  In particular, a robust
performance profiling facility availible in TAU has been applied extensively in
the ACTS toolkit.  Also, recent advancements in TAU's code analysis
capabilities have allowed new static tools to be developed, such as an
automatic instrumentation tool.


%prep
%setup -q -n %{pname}-%{version}

%global _default_patch_fuzz 0

%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1
%patch5 -p1
%patch6 -p1

%ifarch x86_64
sed -i -e 's/^BITS.*/BITS = 64/' src/Profile/Makefile.skel
%endif

# Fix errors with unversioned python shebangs
find . -type f -name '*.py' -exec sed -e "s,/env python,/env python2,g" -i {} \;

# override with newer config.guess for aarch64
%ifarch aarch64 || ppc64le
cp /usr/lib/rpm/config.guess utils/opari2/build-config/.
cp /usr/lib/rpm/config.sub utils/opari2/build-config/.
%endif

# Fix to use the correct compiler
sed -e "s,/usr/bin/g++,g++,g" -i utils/include/Makefile.skel


%build
export TAUROOT=$(pwd)
export INSTALLROOT=${TAUROOT}/TAUBUILD%{install_path}

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
%ifarch x86_64
module load papi
%endif
module load pdtoolkit

%if "%{compiler_family}" == "intel"
export fcomp=ifort
%else
export fcomp=gfortran
%endif

export OMPI_LDFLAGS="-Wl,--as-needed -L$MPI_LIB_DIR"
export FFLAGS="$FFLAGS -I$MPI_INCLUDE_DIR"

# Try and figure out architecture
if [ -n "$detectarch" ]; then
   CONFIG_ARCH="$detectarch"
else
   CONFIG_ARCH=$(./utils/archfind)
   if [ "x$CONFIG_ARCH" = "x" ]; then
      CONFIG_ARCH="unknown"
   fi
fi
export CONFIG_ARCH

# Build the final package directly in the source root
mkdir -p $TAUROOT/TAUBUILD

./configure \
    -arch=$CONFIG_ARCH \
    -prefix=$INSTALLROOT \
    -exec-prefix= \
    -c++=mpicxx \
    -cc=mpicc \
    -fortran=$fcomp \
    -iowrapper \
    -mpi \
    -mpiinc=$MPI_INCLUDE_DIR \
    -mpilib=$MPI_LIB_DIR \
    -slog2 \
    -CPUTIME \
    -PROFILE \
    -PROFILECALLPATH \
    -PROFILEPARAM \
%ifarch x86_64
    -papi=$PAPI_DIR \
%endif
    -pdt=$PDTOOLKIT_DIR \
    -useropt="%optflags -I$MPI_INCLUDE_DIR -I$PWD/include -fno-strict-aliasing" \
    -openmp \
%if %{compiler_family} != intel
    -opari \
%endif
    -extrashlibopts="-fPIC -L$MPI_LIB_DIR -lmpi -L$INSTALLROOT/lib"

make all
make exports

# remove static libs and directories
find ${INSTALLROOT}/lib -name '*.a' -delete
find ${INSTALLROOT}/lib -type d -name 'static-*' -delete

# Replace hard paths with env vars and remove absolute BUILDDIR path
# Use find/replace array pairs -- easy to add more as needed
declare -a finds=("$TAUROOT/TAUBUILD")
declare -a replaces=("")
finds+=("$TAUROOT")
replaces+=("")
finds+=("$MPI_DIR")
replaces+=("\${MPI_DIR}")
finds+=("$PDTOOLKIT_DIR")
replaces+=("\${PDTOOLKIT_DIR}")
%if %{mpi_family} == impi
finds+=("$I_MPI_ROOT")
replaces+=("\${I_MPI_ROOT}")
%endif
%ifarch x86_64
finds+=("$PAPI_DIR")
replaces+=("\${PAPI_DIR}")
finds+=("/x86_64/lib")
replaces+=("/lib")
%endif
# Loop through each S+R pair, then through all files in tree.
# Two stage, using grep to target specific files
i=0
while [ $i -lt ${#finds[@]} ]; do
  for f in $(grep -Ilr "${finds[$i]}" ${INSTALLROOT}); do
    sed -i "s|${finds[$i]}|${replaces[$i]}|g" $f
  done
  let i+=1
done

# Remove RUNPATH entries. Use LMOD environment config instead.
find ${INSTALLROOT}/lib -type f -name '*.so' -exec chrpath -d {} \;


%install
ROOTPATH=%{buildroot}/%{install_path}

# Move the install tree to BUILDROOT
mv %{_builddir}/%{pname}-%{version}/TAUBUILD/* %{buildroot} 

# Link other bindings
ln -s $(basename ${ROOTPATH}/lib/shared-callpath-param-*-trace) ${ROOTPATH}/lib/shared-mpi
for f in $(basename ${ROOTPATH}/lib/libTAUsh-callpath-param-*-trace.so); do
  ln -s $f ${ROOTPATH}/lib/${f/libTAUsh/libTauMpi}
done

# OpenHPC module file
mkdir -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
cat << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
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
setenv          %{PNAME}_OPTIONS    "-optRevert -optShared -optNoTrackGOMP"

depends-on pdtoolkit

EOF

%ifarch x86_64
echo "depends-on papi" >> %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
%endif

cat << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

mkdir -p %{buildroot}/%{_docdir}

%files
%{install_path}
%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%doc Changes CREDITS INSTALL README*
%license COPYRIGHT LICENSE
