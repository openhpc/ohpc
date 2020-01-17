#---------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname likwid

Summary:   Performance tools for the Linux console
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   5.0.1
Release:   1%{?dist}
License:   GPL-3.0+
Group:     %{PROJ_NAME}/perf-tools
URL:       https://github.com/RRZE-HPC/likwid
Source0:   https://github.com/RRZE-HPC/likwid/archive/v%{version}.tar.gz

%if 0%{?rhel_version}
BuildRequires: gcc-gfortran
%endif

%if 0%{?sle_version}
BuildRequires: gcc-fortran
%endif

#BuildRequires: lua-devel
BuildRequires: perl
BuildRequires: lua
#BuildRequires: perl(Data::Dumper)
Requires(post): /sbin/ldconfig
Requires(postun): /sbin/ldconfig

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
Likwid is a simple to install and use toolsuite of command line applications
for performance oriented programmers. It works for Intel and AMD processors
on the Linux operating system.

It consists of:

 * likwid-topology:
     print thread, cache and NUMA topology
 * likwid-perfctr:
     configure and read out hardware performance counters on x86, ARM and POWER
     processors
 * likwid-powermeter:
     read out RAPL Energy information and get info about Turbo mode steps
 * likwid-pin:
     pin your threaded application (pthread, Intel and gcc OpenMP to dedicated
     processors)
 * likwid-genTopoCfg:
     Dumps topology information to a file
 * likwid-memsweeper:
     Sweep memory of NUMA domains and evict cachelines from the last level cache


%prep
%setup -q -n %{pname}-%{version}

%build

# OpenHPC compiler setup
%ohpc_setup_compiler

%if "%{compiler_family}" == "intel"
COMPILER="ICC"
FC="ifort"
FCFLAGS="-module ./"
%else
COMPILER="GCC"
FC="gfortran"
FCFLAGS="-J ./ -fsyntax-only"
%endif

%{__make} FORTRAN_INTERFACE="true" \
          COMPILER="$COMPILER" \
          FC="$FC" \
          FCFLAGS="$FCFLAGS" \
          PREFIX="%{install_path}" \
          LIBDIR="%{install_path}/lib" \
          MANPREFIX="%{install_path}/man" \
          OPTFLAGS="%{optflags}" \
          Q=""


%install

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "intel"
COMPILER="ICC"
FC="ifort"
FCFLAGS="-module ./"
%else
COMPILER="GCC"
FC="gfortran"
FCFLAGS="-J ./ -fsyntax-only"
%endif

%{__make} FORTRAN_INTERFACE="true" \
          COMPILER="$COMPILER" \
          FC="$FC" \
          FCFLAGS="$FCFLAGS" \
          PREFIX="%{buildroot}%{install_path}" \
          LIBDIR="%{buildroot}%{install_path}/lib" \
          MANPREFIX="%{buildroot}%{install_path}/man" \
          INSTALL_CHOWN="" \
          OPTFLAGS="%{optflags}" \
          Q="" install

chmod 755 $RPM_BUILD_ROOT/%{install_path}/sbin/likwid-accessD
chmod 755 $RPM_BUILD_ROOT/%{install_path}/sbin/likwid-setFreq

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

    puts stderr " "
    puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
    puts stderr "toolchain."
    puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
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

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%post
/sbin/ldconfig
chmod u+s $RPM_BUILD_ROOT/%{install_path}/sbin/likwid-accessD
chmod u+s $RPM_BUILD_ROOT/%{install_path}/sbin/likwid-setFreq

%postun
/sbin/ldconfig

%files
%{OHPC_PUB}
%doc INSTALL COPYING README.md
%doc %{install_path}/man/man1/*
