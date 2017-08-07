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
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   Scalable Performance Measurement Infrastructure for Parallel Codes
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   3.1
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/perf-tools
URL:       http://www.vi-hps.org/projects/score-p/
Source0:   http://www.vi-hps.org/upload/packages/scorep/scorep-%{version}.tar.gz
Source1:   OHPC_macros
Patch0:    scorep-gcc7.patch

%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
%endif
BuildRequires: automake
BuildRequires: binutils-devel
Requires     : binutils-devel
BuildRequires: libunwind-devel
Requires     : libunwind-devel
Requires     : lmod >= 7.6.1
BuildRequires: zlib-devel
BuildRequires: papi%{PROJ_DELIM}
Requires     : papi%{PROJ_DELIM}
BuildRequires: pdtoolkit-%{compiler_family}%{PROJ_DELIM}
Requires     : pdtoolkit-%{compiler_family}%{PROJ_DELIM}
BuildRequires: sionlib-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires     : sionlib-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}

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
%patch0 -p1

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load papi
module load pdtoolkit
module load sionlib

%if %{compiler_family} == intel
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-nocross-compiler-suite=intel "
%endif

%if %{mpi_family} == impi
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=intel3 "
%endif

%if %{mpi_family} == mpich
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=mpich3 "
%endif

%if %{mpi_family} == mvapich2
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=mpich3 "
%endif

%if %{mpi_family} == openmpi
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-mpi=openmpi "
%endif

./configure --prefix=%{install_path} --disable-static --enable-shared $CONFIGURE_OPTIONS

%install

export NO_BRP_CHECK_RPATH=true

# OpenHPC compiler designation
%ohpc_setup_compiler

module load papi

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

depends-on papi
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

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc AUTHORS ChangeLog COPYING INSTALL OPEN_ISSUES README THANKS

%changelog
* Tue May 23 2017 Adrian Reber <areber@redhat.com> - 3.0-2
- Remove separate mpi setup; it is part of the %%ohpc_compiler macro

* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 3.0-1
- switch to use of ohpc_compiler_dependent and ohpc_mpi_dependent flags

* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 3.0-1
- Switching to %%ohpc_compiler macro
