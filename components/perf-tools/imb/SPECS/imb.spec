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
%define pname imb

Summary:   Intel MPI Benchmarks (IMB)
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   2019.2
Release:   1%{?dist}
License:   CPL
Group:     %{PROJ_NAME}/perf-tools
URL:       https://software.intel.com/en-us/articles/intel-mpi-benchmarks
Source0:   https://github.com/intel/mpi-benchmarks/archive/IMB-v%{version}.tar.gz

# OpenHPC patches
Patch1: imb.cc.patch

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The Intel MPI Benchmarks (IMB) perform a set of MPI performance
measurements for point-to-point and global communication operations for
a range of message sizes.

%prep
%setup -n mpi-benchmarks-%{version}

# OpenHPC patches
%patch1 -p0

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

cd src
make all
cd -

%install

# OpenHPC compiler designation
%ohpc_setup_compiler

%{__mkdir} -p %{buildroot}%{install_path}/bin
cd src
cp IMB-EXT  %{buildroot}%{install_path}/bin/.
cp IMB-IO   %{buildroot}%{install_path}/bin/.
cp IMB-MPI1 %{buildroot}%{install_path}/bin/.
cp IMB-NBC  %{buildroot}%{install_path}/bin/.
cp IMB-RMA  %{buildroot}%{install_path}/bin/.
cd -



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
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin

setenv          %{PNAME}_DIR        %{install_path}

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
%{OHPC_PUB}
%doc license/license.txt license/use-of-trademark-license.txt ReadMe_IMB.txt
