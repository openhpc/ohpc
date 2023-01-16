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
%undefine _annotated_build

# Base package name
%define pname libfabric

Name: libfabric%{PROJ_DELIM}
Version: 1.13.0
Release: 1%{?dist}
Summary: User-space RDMA Fabric Interfaces
Group:   %{PROJ_NAME}/mpi-families
License: GPLv2 or BSD
Url:     http://www.github.com/ofiwg/libfabric
Source0: https://github.com/ofiwg/%{pname}/releases/download/v%{version}/%{pname}-%{version}.tar.bz2

BuildRequires: gcc make
%if 0%{?suse_version}
Buildrequires: ofed
BuildRequires: rdma-core-devel infiniband-diags-devel
%endif
%if 0%{?rhel} || 0%{?openEuler}
Buildrequires: rdma-core-devel libibmad-devel
%endif
%ifarch x86_64
BuildRequires: libpsm2-devel >= 10.2.0
%endif

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{pname}/%version

%description
libfabric provides a user-space API to access high-performance fabric
services, such as RDMA.

%package -n libfabric-devel%{PROJ_DELIM}
Summary: Development files for the libfabric library
Group: System Environment/Libraries
Requires: libfabric%{PROJ_DELIM} = %{version}

%description -n libfabric-devel%{PROJ_DELIM}
Development files for the libfabric library.

%prep
%setup -q -n %{pname}-%{version}

%build
./configure --prefix=%{install_path} \
  	    --libdir=%{install_path}/lib
make %{?_smp_mflags}

%install
rm -rf %{buildroot}

make DESTDIR=%{buildroot} install
# remove unpackaged files from the buildroot
rm -f %{buildroot}%{_libdir}/*.la
rm -f %{buildroot}%{install_path}/lib/*.a

# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL: %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%{OHPC_HOME}
%doc AUTHORS COPYING README
