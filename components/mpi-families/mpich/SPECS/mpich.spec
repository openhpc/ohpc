#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# MVAPICH2 MPI stack that is dependent on compiler toolchain

%define with_slurm 0
%define with_psm 0

#-fsp-header-comp-begin----------------------------------------------

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM   %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM}
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{OHPC_build}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end------------------------------------------------

%if 0%{with_slurm}
BuildRequires: slurm-devel%{PROJ_DELIM} slurm%{PROJ_DELIM}
%endif

%if %{with_psm}
BuildRequires:  infinipath-psm infinipath-psm-devel
%endif

%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname mpich

Summary:	A high-performance implementation of MPI
Name:		%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:	3.2b4
Release:	4%{?dist}
License:	MIT
Group:		fsp/mpi-families
URL:		http://www.mpich.org/

Source0:	%{pname}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
DocDir:    %{FSP_PUB}/doc/contrib

%define debug_package %{nil}

%if 0%{?sles_version} || 0%{?suse_version}
Buildrequires: ofed
%endif
%if 0%{?rhel_version} || 0%{?centos_version}
Buildrequires: rdma
Buildrequires: librdmacm
%endif

Requires: prun%{PROJ_DELIM}

BuildRequires: bison
BuildRequires: libibmad-devel libibverbs-devel
Buildrequires: libfabric-devel
Requires: libfabric-devel

# For python_sitearch
BuildRequires:  python-devel
Provides:	mpi
Provides:	mpich2 = 3.2b4
Obsoletes:	mpich2 < 3.2

# Default library install path
%define install_path %{FSP_MPI_STACKS}/%{name}/%version

%description
MPICH is a high-performance and widely portable implementation of the Message
Passing Interface (MPI) standard (MPI-1, MPI-2 and MPI-3). The goals of MPICH
are: (1) to provide an MPI implementation that efficiently supports different
computation and communication platforms including commodity clusters (desktop
systems, shared-memory systems, multicore architectures), high-speed networks
(10 Gigabit Ethernet, InfiniBand, Myrinet, Quadrics) and proprietary high-end
computing systems (Blue Gene, Cray) and (2) to enable cutting-edge research in
MPI through an easy-to-extend modular framework for other derived
implementations.

The mpich binaries in this RPM packages were configured to use the default
process manager (Hydra) using the default device (ch3). The ch3 device
was configured with support for the nemesis channel that allows for
shared-memory and TCP/IP sockets based communication.

This build also include support for using the 'module environment' to select
which MPI implementation to use when multiple implementations are installed.
If you want MPICH support to be automatically loaded, you need to install the
mpich-autoload package.

%prep
%setup -q -n %{pname}-%{version}

%build

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

./configure	\
    --prefix=%{install_path} \
	--enable-sharedlibs=gcc					\
	--enable-shared						\
	--enable-static=no						\
	--enable-lib-depend					\
	--disable-silent-rules					\
	--enable-fc						\
	--enable-cxx						\
	--enable-g=dbg						\
	--enable-romeo						\
	--enable-threads=runtime						\
    --disable-wrapper-rpath         \
	--with-hwloc-prefix=system				\
    --with-device=ch3:nemesis:ofi \
%if 0%{with_slurm}
    --with-pm=no --with-pmi=slurm \
%endif
    --enable-fast=O3 || { cat config.log && exit 1; }

# Remove rpaths
%{__sed} -i 's|^hardcode_libdir_flag_spec=.*|hardcode_libdir_flag_spec=""|g' libtool
%{__sed} -i 's|^runpath_var=LD_RUN_PATH|runpath_var=DIE_RPATH_DIE|g' libtool

make %{?_smp_mflags} VERBOSE=1

%install

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

make DESTDIR=%{buildroot} install


# FSP module file
%{__mkdir} -p %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL: %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/man
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib
prepend-path    MODULEPATH          %{FSP_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    MPI_DIR             %{install_path}
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

family "MPI"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{FSP_HOME}
%{FSP_PUB}
%doc CHANGES COPYRIGHT README README.envvar RELEASE_NOTES

%changelog
