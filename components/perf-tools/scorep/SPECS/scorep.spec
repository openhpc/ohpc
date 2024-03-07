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
Version:   7.1
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/perf-tools
URL:       http://www.vi-hps.org/projects/score-p/
Source0:   https://perftools.pages.jsc.fz-juelich.de/cicd/scorep/tags/scorep-%{version}/scorep-%{version}.tar.gz

BuildRequires: fdupes
BuildRequires: automake make which
BuildRequires: binutils-devel
Requires:      binutils-devel
BuildRequires: libunwind-devel
Requires:      libunwind-devel
Requires:      lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires: zlib-devel gcc-c++
%ifarch x86_64
BuildRequires: papi%{PROJ_DELIM}
Requires:      papi%{PROJ_DELIM}
%endif
BuildRequires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}
Requires:      pdtoolkit-%{compiler_family}%{PROJ_DELIM}
BuildRequires: sionlib-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      sionlib-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
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

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%ifarch x86_64
module load papi
%endif
module load pdtoolkit
module load sionlib

%if "%{compiler_family}" == "intel"
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-nocross-compiler-suite=intel "
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

export CFLAGS="$CFLAGS"
export CXXFLAGS="$CFLAGS"
./configure --prefix=%{install_path} --disable-static --enable-shared $CONFIGURE_OPTIONS CC=$CC CXX=$CXX F77=$F77

%if "%{compiler_family}" == "arm1"
%{__sed} -i -e 's#wl=""#wl="-Wl,"#g' build-mpi/libtool
%{__sed} -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' build-mpi/libtool
%endif

%if "%{compiler_family}" == "intel"
sed -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' build-score/libtool
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

depends-on pdtoolkit
depends-on sionlib

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
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
%doc AUTHORS ChangeLog COPYING INSTALL OPEN_ISSUES README THANKS
