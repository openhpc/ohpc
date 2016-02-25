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

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency.
%if !0%{?opensuse_bs}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
BuildRequires: coreutils
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

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi-devel%{PROJ_DELIM}
Requires:      intel-mpi-devel%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif

#-ohpc-header-comp-end------------------------------------------------

# Base package name
%define pname scorep
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name: %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

Version:   1.4.2
Release:   1%{?dist}
Summary:   Tuning and Analysis Utilities Profiling Package
License:   Tuning and Analysis Utilities License
Group:     ohpc/perf-tools
Url:       http://www.cs.uoregon.edu/research/scorep/home.php
Source0:   http://www.vi-hps.org/upload/packages/scorep/scorep-%{version}.tar.gz
Provides:  lib%PNAME.so()(64bit)
Provides:  perl(ebs2otf)
Conflicts: lib%pname < %version-%release
Obsoletes: lib%pname < %version-%release
DocDir:    %{OHPC_PUB}/doc/contrib

%if 0%{?suse_version}
BuildRequires: libgomp1
%else
BuildRequires: libgomp
%endif

BuildRequires: postgresql-devel binutils-devel
Requires: binutils-devel
BuildRequires: libotf-devel zlib-devel python-devel
BuildRequires: papi%{PROJ_DELIM}
BuildRequires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}
Requires: papi%{PROJ_DELIM}
Requires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Scalable Performance Measurement Infrastructure for Parallel Codes
The Score-P measurement infrastructure is a highly scalable and easy-to-use tool
suite for profiling, event tracing, and online analysis of HPC applications.
It has been created in the German BMBF project SILC and the US DOE project PRIMA
and will be maintained and enhanced in a number of follow-up projects such as
LMAC, Score-E, and HOPSA. Score-P is developed under a BSD 3-Clause License and
governed by a meritocratic governance model.

Score-P offers the user a maximum of convenience by supporting a number of
analysis tools. Currently, it works with Periscope, Scalasca, Vampir, and Tau
and is open for other tools. Score-P comes together with the new Open Trace
Format Version 2, the Cube4 profiling format and the Opari2 instrumenter.
Score-P is available under the New BSD Open Source license.


%prep
%setup -q -n %{pname}-%{version}

%ifarch x86_64
sed -i -e 's/^BITS.*/BITS = 64/' src/Profile/Makefile.skel
%endif

%build
# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_compiler
. %{_sourcedir}/OHPC_setup_mpi
module load papi
module load pdtoolkit

%if %{compiler_family} == gnu
export fcomp=gfortran
%endif
%if %{compiler_family} == intel
export fcomp=mpiifort
%endif

%if %{mpi_family} == impi
export MPI_DIR=$I_MPI_ROOT
export MPI_INCLUDE_DIR=$MPI_DIR/include64
export MPI_LIB_DIR=$MPI_DIR/lib64
%else
export MPI_INCLUDE_DIR=$MPI_DIR/include
export MPI_LIB_DIR=$MPI_DIR/lib
%endif

export OMPI_LDFLAGS="-Wl,--as-needed -L$MPI_LIB_DIR"
export BUILDROOT=%buildroot%{install_path}
export FFLAGS="$FFLAGS -I$MPI_INCLUDE_DIR"
./configure \
    -prefix=/tmp/%{install_path} \
    -exec-prefix= \
	-c++=mpicxx \
	-cc=mpicc \
	-fortran=$fcomp \
	-iowrapper \
	-mpi \
	-mpiinc=$MPI_INCLUDE_DIR \
	-mpilib=$MPI_LIB_DIR \
	-slog2 \
	-PROFILEPARAM \
    -papi=$PAPI_DIR \
	-pdt=$PDTOOLKIT_DIR \
	-CPUTIME \
	-useropt="%optflags -I$MPI_INCLUDE_DIR -I$PWD/include -fno-strict-aliasing" \
	-openmp \
	-extrashlibopts="-L$MPI_LIB_DIR -lmpi -L/tmp%{install_path}/lib"


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
rm -f scorep_java
popd

sed -i 's|/tmp||g' %buildroot%{install_path}/include/*.h
sed -i 's|/tmp||g' %buildroot%{install_path}/include/Makefile
#sed -i 's|/home/abuild/rpmbuild/BUILD/scorep-2.24|%{install_path}|g' %buildroot%{install_path}/include/Makefile*
#sed -i 's|/home/abuild/rpmbuild/BUILD/scorep-2.24|%{install_path}|g' %buildroot%{install_path}/lib/Makefile*

rm -rf %{install_path}/examples
rm -rf %buildroot%{install_path}/examples
rm -f %{install_path}/.last_config
rm -f %{install_path}/.all_configs
rm -f %{install_path}/.active_stub*


# clean libs
pushd %buildroot%{install_path}/lib
sed -i 's|/tmp||g' $(egrep -IR '/tmp/' ./|awk -F : '{print $1}')
rm -f libjogl*
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
