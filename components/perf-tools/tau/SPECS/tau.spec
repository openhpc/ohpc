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

Version:   2.28
Release:   1%{?dist}
Summary:   Tuning and Analysis Utilities Profiling Package
License:   Tuning and Analysis Utilities License
Group:     %{PROJ_NAME}/perf-tools
Url:       http://www.cs.uoregon.edu/research/tau/home.php
Source0:   https://www.cs.uoregon.edu/research/tau/tau_releases/tau-%{version}.tar.gz
Patch1:    tau-add-explicit-linking-option.patch
Patch2:    tau-shared_libpdb.patch
Patch3:    tau-disable_examples.patch
Patch4:    tau-ucontext.patch
Patch5:    tau-testplugins_makefile.patch

Provides:  lib%PNAME.so()(64bit)(%PROJ_NAME)
Provides:  perl(ebs2otf)
Conflicts: lib%pname < %version-%release
Obsoletes: lib%pname < %version-%release

%if 0%{?suse_version}
BuildRequires: libgomp1
%else
BuildRequires: libgomp
%endif

BuildRequires: curl
BuildRequires: postgresql-devel binutils-devel
Requires: binutils-devel
BuildRequires: libotf-devel zlib-devel python-devel
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
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1
%patch5 -p1

%ifarch x86_64
sed -i -e 's/^BITS.*/BITS = 64/' src/Profile/Makefile.skel
%endif

%install
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

%if %{mpi_family} == impi
export MPI_INCLUDE_DIR=$I_MPI_ROOT/intel64/include
export MPI_LIB_DIR=$I_MPI_ROOT/intel64/lib
%else
export MPI_INCLUDE_DIR=$MPI_DIR/include
export MPI_LIB_DIR=$MPI_DIR/lib
%endif

export OMPI_LDFLAGS="-Wl,--as-needed -L$MPI_LIB_DIR"
export BUILDROOT=%buildroot
export FFLAGS="$FFLAGS -I$MPI_INCLUDE_DIR"
export TAUROOT=`pwd`

# override with newer config.guess for aarch64
%ifarch aarch64 || ppc64le
cp /usr/lib/rpm/config.guess utils/opari2/build-config/.
cp /usr/lib/rpm/config.sub utils/opari2/build-config/.
%endif

# Try and figure out architecture
detectarch=unknown
detectarch=`./utils/archfind`
if [ "x$detectarch" = "x" ]
  then
    detectarch=unknown
fi
%global machine `echo $detectarch`
export CONFIG_ARCH=%{machine}

./configure \
    -arch=%{machine} \
    -prefix=/tmp%{install_path} \
    -exec-prefix= \
    -c++=$CXX \
    -cc=$CC \
    -fortran=$fcomp \
    -iowrapper \
    -slog2 \
    -CPUTIME \
    -PROFILE \
    -PROFILECALLPATH \
    -PROFILEPARAM \
%ifarch x86_64
    -papi=$PAPI_DIR \
%endif
    -pdt=$PDTOOLKIT_DIR \
    -useropt="%optflags -I$PWD/include -fno-strict-aliasing" \
    -openmp \
%if %{compiler_family} != intel
    -opari \
%endif
    -extrashlibopts="-fPIC -L/tmp%{install_path}/lib"

make install
make exports
make clean

./configure \
    -arch=%{machine} \
    -prefix=/tmp%{install_path} \
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
    -extrashlibopts="-fPIC -L$MPI_LIB_DIR -lmpi -L/tmp%{install_path}/lib"

make install
make exports


# move from tmp install dir to %install_path
# dirname removes the last directory
mkdir -p `dirname %{buildroot}%{install_path}`
pushd /tmp
export tmp_path=%{install_path}
mv ${tmp_path#*/} `dirname %{buildroot}%{install_path}`
popd

# clean up
pushd %{buildroot}%{install_path}/bin
sed -i 's|/tmp/opt|/opt|g' $(egrep -IR '/tmp/opt' ./|awk -F : '{print $1}')
popd
pushd %{buildroot}%{install_path}/lib
rm -f libjogl*
popd
sed -i 's|/tmp||g' %buildroot%{install_path}/include/*.h
sed -i 's|/tmp||g' %buildroot%{install_path}/include/Makefile*
sed -i 's|/tmp||g' %buildroot%{install_path}/lib/Makefile*
sed -i 's|/tmp||g' $(egrep -R '%buildroot' ./ |\
egrep -v 'Binary\ file.*matches' |awk -F : '{print $1}')
sed -i 's|%buildroot||g' $(egrep -R '%buildroot' ./ |\
egrep -v 'Binary\ file.*matches' |awk -F : '{print $1}')
sed -i "s|$TAUROOT|%{install_path}|g" $(egrep -IR "$TAUROOT" %buildroot%{install_path}|awk -F : '{print $1}')

# fix tau lib arch location
sed -i "s|/x86_64/lib|/lib|g" $(egrep -IR "/x86_64/lib" %buildroot%{install_path}|awk -F : '{print $1}')

# replace hard paths with env vars
%if %{mpi_family} == impi
sed -i "s|$I_MPI_ROOT|\$\{I_MPI_ROOT\}|g" $(egrep -IR "$I_MPI_ROOT" %buildroot%{install_path}|awk -F : '{print $1}')
%else
sed -i "s|$MPI_DIR|\$\{MPI_DIR\}|g" $(egrep -IR "$MPI_DIR" %buildroot%{install_path}|awk -F : '{print $1}')
%endif
%ifarch x86_64
sed -i "s|$PAPI_DIR|\$\{PAPI_DIR\}|g" $(egrep -IR "$PAPI_DIR" %buildroot%{install_path}|awk -F : '{print $1}')
%endif
sed -i "s|$PDTOOLKIT_DIR|\$\{PDTOOLKIT_DIR\}|g" $(egrep -IR "$PDTOOLKIT_DIR" %buildroot%{install_path}|awk -F : '{print $1}')

# link other bindings
pushd %{buildroot}%{install_path}/lib
%if %{compiler_family} == intel
ln -s shared-callpath-param-icpc-papi-mpi-pdt-openmp-profile-trace shared-mpi
ln -s libTAUsh-callpath-param-icpc-papi-mpi-pdt-openmp-profile-trace.so libTauMpi-callpath-param-icpc-papi-mpi-pdt-openmp-profile-trace.so
%else
%ifarch x86_64
ln -s shared-callpath-param-papi-mpi-pdt-openmp-opari-profile-trace shared-mpi
ln -s libTAUsh-callpath-param-papi-mpi-pdt-openmp-opari-profile-trace.so libTauMpi-callpath-param-papi-mpi-pdt-openmp-opari-profile-trace.so
%else
ln -s shared-callpath-param-mpi-pdt-openmp-opari-profile-trace shared-mpi
ln -s libTAUsh-callpath-param-mpi-pdt-openmp-opari-profile-trace.so libTauMpi-callpath-param-mpi-pdt-openmp-opari-profile-trace.so
%endif
%endif
popd

# remove static libs
pushd %{buildroot}%{install_path}/lib
rm -rf *\.a static-*
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
setenv          %{PNAME}_OPTIONS    "-optRevert -optShared -optNoTrackGOMP"

depends-on pdtoolkit

EOF

%ifarch x86_64
echo "depends-on papi" >> %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
%endif

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%{OHPC_PUB}
%doc Changes COPYRIGHT CREDITS INSTALL LICENSE README*
