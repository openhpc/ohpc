%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family variable via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%define fcomp gfortran
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%define fcomp mpiifort
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi%{PROJ_DELIM}
Requires:      intel-mpi%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif

#-fsp-header-comp-end------------------------------------------------

# Base package name
%define pname tau
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name: %{pname}%{PROJ_DELIM}
Version: 2.24
Release: 1%{?dist}
Summary: Tuning and Analysis Utilities Profiling Package
License: Tuning and Analysis Utilities License
Group: fsp/perf-tools
Url: http://www.cs.uoregon.edu/research/tau/home.php
Source0: %{pname}-%{version}.tar.gz
Provides:  lib%PNAME.so()(64bit)
Conflicts: lib%pname < %version-%release
Obsoletes: lib%pname < %version-%release

%if 0%{?suse_version}
BuildRequires: libgomp1
%else
BuildRequires: libgomp
%endif

BuildRequires: postgresql-devel binutils-devel
BuildRequires: libotf-devel zlib-devel python-devel

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{pname}/%version

%description
TAU is a program and performance analysis tool framework being developed for
the DOE and ASC program at University of Oregon. TAU provides a suite of 
static and dynamic tools that provide graphical user
interaction and interoperation to form an integrated analysis environment for
parallel Fortran 95, C and C++ applications.  In particular, a robust
performance profiling facility availble in TAU has been applied extensively in
the ACTS toolkit.  Also, recent advancements in TAU's code analysis
capabilities have allowed new static tools to be developed, such as an
automatic instrumentation tool.


%prep
%setup -q -n %{pname}-%{version}
%ifarch x86_64
sed -i -e 's/^BITS.*/BITS = 64/' src/Profile/Makefile.skel
%endif

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

%if %{compiler_family} == gnu
export fcomp=gfortran
%endif
%if %{compiler_family} == intel
export fcomp=mpiifort
%endif
export OMPI_LDFLAGS="-Wl,--as-needed -L$MPI_DIR/lib"

export BUILDROOT=%buildroot%{install_path}
export FFLAGS="$FFLAGS -I$MPI_DIR/include"
./configure \
        -prefix=%buildroot%{install_path} \
	-c++=mpicxx \
	-cc=mpicc \
	-fortran=$fcomp \
	-iowrapper \
	-mpi \
	-slog2 \
	-PROFILEPARAM \
	-CPUTIME \
	-useropt="%optflags -I$MPI_DIR/include -I$PWD/include -fno-strict-aliasing" \
        -openmp \
	-extrashlibopts="-L$MPI_DIR/lib -lmpi -lgomp"


make install TOPDIR=$TOPDIR
make exports TOPDIR=$TOPDIR

pushd %{buildroot}%{install_path}/x86_64/bin
sed -i 's|%{buildroot}||g' $(egrep -IR '%{buildroot}' ./|awk -F : '{print $1}')
popd
mv %buildroot%{install_path}/x86_64/bin %buildroot%{install_path}/bin
mv %buildroot%{install_path}/x86_64/lib %buildroot%{install_path}/lib
install -d %buildroot%{install_path}/etc
install -d %buildroot%_datadir/%name
install -d %buildroot%{install_path}/include

rm -rf %buildroot%{install_path}/x86_64
rm -f  %buildroot%{install_path}/examples/gpu/cuda/unifmem/Makefile~
rm -f %buildroot%_includedir/include/Makefile*
rm -fR %buildroot%_includedir/include/makefiles
rm -f %buildroot%{install_path}/.last_config
rm -f %buildroot%{install_path}/.all_configs
rm -f %buildroot%{install_path}/.active_stub*

# clean libs
pushd %buildroot%{install_path}/lib
if [ -f  Makefile.tau-param-mpi-openmp-profile-trace ]
    then
        sed -i 's|%buildroot||g' Makefile.tau-param-mpi-openmp-profile-trace
fi
if [ -f libTAUsh-param-mpi-openmp-profile-trace.so ]
    then
        sed -i 's|%buildroot||g' libTAUsh-param-mpi-openmp-profile-trace.so
fi
if [ -f Makefile.tau-param-icpc-mpi-openmp-profile-trace ]
    then
        sed -i 's|%buildroot||g' Makefile.tau-param-icpc-mpi-openmp-profile-trace
fi
if [ -f libTAUsh-param-icpc-mpi-openmp-profile-trace.so ]
    then
        sed -i 's|%buildroot||g' libTAUsh-param-icpc-mpi-openmp-profile-trace.so
fi
rm -f libtau-param-mpi-openmp-profile-trace.a
rm -f libtau-param-icpc-mpi-openmp-profile-trace.a
rm -f libjogl*
popd


# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
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

EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF


%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog

