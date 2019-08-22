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
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname geopm

# Install paths
%define docdir %{OHPC_PUB}/doc/contrib/%{pname}-%{compiler_family}-%{mpi_family}-%{version}
%define install_path  %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

Name:          %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:       Global Extensible Open Power Manager
Version:       1.0.0
Release:       1
License:       BSD-3-Clause
Group:         %{PROJ_NAME}/perf-tools
URL:           https://geopm.github.io
Source0:       https://github.com/geopm/geopm/releases/download/v%{version}/geopm-%{version}.tar.gz
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
BuildRequires: libtool-ltdl-devel
BuildRequires: python
BuildRequires: python-devel
BuildRequires: unzip
BuildRequires: zlib-devel

%if 0%{?suse_version} >= 1320
BuildRequires: openssh
%endif

%description
Global Extensible Open Power Manager (GEOPM) is an extensible power
management framework targeting high performance computing.  The library can be
extended to support new control algorithms and new hardware power management
features.  The GEOPM package provides built in features ranging from static
management of power policy for each individual compute node, to dynamic
coordination of power policy and performance across all of the compute nodes
hosting one MPI job on a portion of a distributed computing system.  The
dynamic coordination is implemented as a hierarchical control system for
scalable communication and decentralized control.  The hierarchical control
system can optimize for various objective functions including maximizing
global application performance within a power bound.  The root of the control
hierarchy tree can communicate through shared memory with the system resource
management daemon to extend the hierarchy above the individual MPI job level
and enable management of system power resources for multiple MPI jobs and
multiple users by the system resource manager.  The GEOPM package provides the
libgeopm library, the libgeopmpolicy library, the geopmctl application and the
geopmpolicy application.  The libgeopm library can be called within MPI
applications to enable application feedback for informing the control
decisions.  If modification of the target application is not desired then the
geopmctl application can be run concurrently with the target application.  In
this case, target application feedback is inferred by querying the hardware
through Model Specific Registers (MSRs).  With either method (libgeopm or
geopmctl), the control hierarchy tree writes processor power policy through
MSRs to enact policy decisions.  The libgeopmpolicy library is used by a
resource manager to set energy policy control parameters for MPI jobs.  Some
features of libgeopmpolicy are available through the geopmpolicy application
including support for static control.

%prep

%setup -q -n %{pname}-%{version}

%build
%ohpc_setup_compiler
test -f configure || ./autogen.sh
./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --disable-doc \
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
prepend-path    PYTHONPATH          %{install_path}/lib/python2.7/site-packages
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
