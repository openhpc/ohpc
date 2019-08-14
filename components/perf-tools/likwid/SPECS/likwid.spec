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
%global gnu_family gnu8

# Base package name
%define pname likwid

Summary:   Toolsuite of command line applications for performance oriented programmers
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   4.3.4
Release:   1%{?dist}
License:   GPLv3
Group:     %{PROJ_NAME}/perf-tools
URL:       https://github.com/RRZE-HPC/likwid
Source0:   https://github.com/RRZE-HPC/likwid/archive/%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Patch1:    likwid-gfortran.patch
BuildRequires: perl(Data::Dumper)

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
LIKWID stands for "Like I Knew What I'm Doing." It is an easy to use yet powerful
command line performance tool suite for the GNU/Linux operating system. While the
focus of LIKWID is on x86 processors, some of the tools are portable and not
limited to any specific architecture.

%prep
%setup -q -n %{pname}-%{version}
%if "%{compiler_family}" == "%{gnu_family}"
%patch1 -p1
%endif

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "%{gnu_family}"
%define compiler GCC
%else
%define compiler ICC
%endif

make \
    FORTRAN_INTERFACE="true" \
    COMPILER="%{compiler}" \
    PREFIX="%{install_path}" \
    LIBDIR="%{install_path}/lib" \
    MANPREFIX="%{install_path}/man" \
    OPTFLAGS="%{optflags}" \
    Q=""


%install

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "%{gnu_family}"
%define compiler GCC
%else
%define compiler ICC
%endif

make %{?_smp_mflags} \
    FORTRAN_INTERFACE="true" \
    PREFIX="%{buildroot}%{install_path}" \
    LIBDIR="%{buildroot}%{install_path}/lib" \
    MANPREFIX="%{buildroot}%{install_path}/man" \
    INSTALL_CHOWN="" \
    OPTFLAGS="%{optflags}" \
    Q="" \
    install

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

%files
%{OHPC_PUB}
%doc INSTALL COPYING README.md
