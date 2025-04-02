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
%define pname scorep

Summary:   Scalable Performance Measurement Infrastructure for Parallel Codes
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   9.0
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/perf-tools
URL:       http://www.vi-hps.org/projects/score-p/
Source0:   https://perftools.pages.jsc.fz-juelich.de/cicd/scorep/tags/scorep-%{version}/scorep-%{version}.tar.gz
%if "%{mpi_family}" == "impi"
Patch1:    scorep-9.0-build-Add-MPIF08-configure-check-for-Intel-MPI.patch
%endif
BuildRequires: automake
BuildRequires: bison
BuildRequires: binutils-devel
BuildRequires: chrpath
BuildRequires: cmake
BuildRequires: cubelib-%{compiler_family}%{PROJ_DELIM} >= 4.9
BuildRequires: cubew-%{compiler_family}%{PROJ_DELIM} >= 4.9
BuildRequires: fdupes
BuildRequires: gcc-c++
BuildRequires: gotcha-%{compiler_family}%{PROJ_DELIM}
BuildRequires: libunwind-devel
BuildRequires: make
BuildRequires: opari2-%{compiler_family}%{PROJ_DELIM} >= 2.0.9
BuildRequires: otf2-%{compiler_family}-%{mpi_family}%{PROJ_DELIM} >= 3.1
%ifarch x86_64
BuildRequires: papi%{PROJ_DELIM}
%endif
BuildRequires: which
Requires:      binutils-devel
Requires:      cubelib-%{compiler_family}%{PROJ_DELIM} >= 4.9
Requires:      cubew-%{compiler_family}%{PROJ_DELIM} >= 4.9
Requires:      gotcha-%{compiler_family}%{PROJ_DELIM}
Requires:      libunwind-devel
Requires:      lmod%{PROJ_DELIM} >= 7.6.1
Requires:      opari2-%{compiler_family}%{PROJ_DELIM} >= 2.0.9
Requires:      otf2-%{compiler_family}-%{mpi_family}%{PROJ_DELIM} >= 3.1
Requires:      papi%{PROJ_DELIM}

#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The Score-P (Scalable Performance Measurement Infrastructure for
Parallel Codes) measurement infrastructure is a highly scalable
and easy-to-use tool suite for profiling, event trace recording,
and online analysis of HPC applications.

This is the %{compiler_family}-%{mpi_family} version.


%prep

%setup -q -n %{pname}-%{version}
%if "%{mpi_family}" == "impi"
%patch -P 1 -p1
%endif

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%ifarch x86_64
module load papi
CONFIGURE_OPTIONS="${CONFIGURE_OPTIONS} --with-papi-header=${PAPI_INC} --with-papi-lib=${PAPI_LIB}"
%endif
module load cubew
module load cubelib
module load opari2
module load otf2
module load gotcha

%if "%{compiler_family}" == "intel"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-nocross-compiler-suite=oneapi "
%endif

%if "%{mpi_family}" == "impi"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=intel3 "
%endif

%if "%{mpi_family}" == "mpich"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=mpich3 "
%endif

%if "%{mpi_family}" == "mvapich2"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=mpich3 "
%endif

%if "%{mpi_family}" == "openmpi"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=openmpi "
%endif

%if "%{mpi_family}" == "openmpi4"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=openmpi "
%endif

%if "%{mpi_family}" == "openmpi5"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=openmpi "
%endif

# Work around static binutils in OpenSUSE Leap 15
%if 0%{?suse_version}
export LIBBFD_EXTRA_LIBS="-liberty -lz -ldl -lsframe"
CONFIGURE_OPTIONS="${CONFIGURE_OPTIONS}"
%endif

export CFLAGS="$CFLAGS"
export CXXFLAGS="$CFLAGS"
./configure --prefix=%{install_path} \
            --disable-static \
            --enable-shared \
            --disable-silent-rules \
            --enable-backend-test-runs \
            --with-platform=linux \
            --with-libgotcha=${GOTCHA_DIR} \
            CC="$CC" \
            CXX="$CXX" \
            F77="$F77" \
            F90="$F90" \
            CFLAGS="$CFLAGS" \
            CXXFLAGS="$CXXFLAGS" \
            LDFLAGS="$LDFLAGS" \
            ${CONFIGURE_OPTIONS}

%if "%{compiler_family}" == "arm1"
%{__sed} -i -e 's#wl=""#wl="-Wl,"#g' build-mpi/libtool
%{__sed} -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' build-mpi/libtool
%endif

%if "%{compiler_family}" == "intel"
%{__sed} -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' build-score/libtool
make -C build-score CC=$CC V=1 %{?_smp_mflags}
%endif

make V=1 %{?_smp_mflags}

%install

export NO_BRP_CHECK_RPATH=true

# OpenHPC compiler designation
%ohpc_setup_compiler

%ifarch x86_64
module load papi
%endif
module load cubew
module load cubelib
module load opari2
module load otf2
module load gotcha

make DESTDIR=$RPM_BUILD_ROOT install

# don't package static libs
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*.la
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*.a
rm -f $RPM_BUILD_ROOT%{install_path}/lib/scorep/*.la

%if 0%{?suse_version}
%fdupes -s %{buildroot}%{install_path}
%endif

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
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

depends-on cubelib
depends-on cubew
depends-on opari2
depends-on otf2
depends-on gotcha

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

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

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc AUTHORS CITATION.cff ChangeLog COPYING INSTALL OPEN_ISSUES README.md THANKS
