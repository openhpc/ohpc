#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
# Supporting only Python3 here, but a quick way to set python variables
%define ohpc_python_dependent 1
%define python_family python3
%define python_family_lib_dir /lib/python%{expand:%{%{python_family}_version}}/site-packages
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname geopm

# Install paths
%define docdir %{OHPC_PUB}/doc/contrib/%{pname}-%{compiler_family}-%{mpi_family}-%{version}
%define install_path  %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

Name:          %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:       Global Extensible Open Power Manager
Version:       1.1.0
Release:       1
License:       BSD-3-Clause
Group:         %{PROJ_NAME}/perf-tools
URL:           https://geopm.github.io
Source0:       https://github.com/geopm/geopm/releases/download/v%{version}/geopm-%{version}.tar.gz
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: libtool-ltdl-devel
BuildRequires: unzip

%if 0%{?suse_version}%{?sle_version}
BuildRequires: libelf-devel
%else
BuildRequires: elfutils-libelf-devel
%endif

%if 0%{?suse_version} >= 1320 || 0%{?sle_version} >= 1320
BuildRequires: openssh
%endif

%description
The Global Extensible Open Power Manager (GEOPM) is a framework for
exploring power and energy optimizations targeting high performance
computing.  The GEOPM package provides many built-in features.  A
simple use case is reading hardware counters and setting hardware
controls with platform independant syntax using a command line tool on
a particular compute node.  An advanced use case is dynamically
coordinating hardware settings across all compute nodes used by an
application in response to the application's behavior and requests
from the resource manager.  The dynamic coordination is implemented as
a hierarchical control system for scalable communication and
decentralized control. The hierarchical control system can optimize
for various objective functions including maximizing global
application performance within a power bound or minimizing energy
consumption with marginal degradation of application performance.  The
root of the control hierarchy tree can communicate with the system
resource manager to extend the hierarchy above the individual MPI
application and enable the management of system power resources for
multiple MPI jobs and multiple users by the system resource manager.

%prep

%setup -q -n %{pname}-%{version}

%build
%ohpc_setup_compiler
test -f configure || ./autogen.sh
./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --disable-doc \
            --with-python=%{python_family} \
            || ( cat config.log && false )
%{__make}


%install
%ohpc_setup_compiler
%{__make} DESTDIR=%{buildroot} install
rm -f $(find %{buildroot}/%{install_path} -name '*.a'; \
        find %{buildroot}/%{install_path} -name '*.la'; \
        find %{buildroot}/%{install_path} -name 'geopm_launcher.1*')

# Module file
# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
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
module-whatis "Category: runtime"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    PYTHONPATH          %{install_path}%{python_family_lib_dir}
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path    MANPATH             %{install_path}/share/man

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%files
%{OHPC_PUB}
%doc README COPYING VERSION
