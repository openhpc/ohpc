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
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Multiple permutations for this MPI stack are possible depending
# on the desired underlying resource manager and comm library support.

%{!?with_slurm: %global with_slurm 0}
%{!?with_pbs: %global with_pbs 0}
%{!?with_psm: %global with_psm 0}
%{!?with_psm2: %global with_psm2 0}
%{!?RMS_DELIM: %global RMS_DELIM %{nil}}
%{!?COMM_DELIM: %global COMM_DELIM %{nil}}

# Base package name/config
%define pname mvapich2

Summary:   OSU MVAPICH2 MPI implementation
Name:      %{pname}%{COMM_DELIM}-%{compiler_family}%{RMS_DELIM}%{PROJ_DELIM}
Version:   2.3.1
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/mpi-families
URL:       http://mvapich.cse.ohio-state.edu
Source0:   http://mvapich.cse.ohio-state.edu/download/mvapich/mv2/%{pname}-%{version}.tar.gz

# karl.w.schulz@intel.com (04/13/2016)
Patch0:    mvapich2-get_cycles.patch
# karl.w.schulz@intel.com (05/21/2017)
Patch1:     mpidimpl.opt.patch

%if 0%{with_slurm}
BuildRequires: slurm-devel%{PROJ_DELIM} slurm%{PROJ_DELIM}
Provides:      %{pname}-%{compiler_family}%{PROJ_DELIM}
%endif

%if 0%{with_psm}
BuildRequires:  infinipath-psm infinipath-psm-devel
Provides: %{pname}-%{compiler_family}%{PROJ_DELIM}
%endif

%if 0%{with_psm2}
BuildRequires:  libpsm2-devel >= 10.2.0
Requires:       libpsm2 >= 10.2.0
Provides: %{pname}-%{compiler_family}%{PROJ_DELIM}
Conflicts: %{pname}-%{compiler_family}%{PROJ_DELIM}
%endif

%if 0%{?sles_version} || 0%{?suse_version}
Buildrequires: ofed
BuildRequires: rdma-core-devel infiniband-diags-devel
%endif
%if 0%{?rhel}
Buildrequires: rdma-core-devel libibmad-devel
%endif

Requires: prun%{PROJ_DELIM}
BuildRequires: bison
BuildRequires: zlib-devel

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{pname}-%{compiler_family}/%version

%description

MVAPICH2 is a high performance MPI-2 implementation (with initial
support for MPI-3) for InfiniBand, 10GigE/iWARP and RoCE.  MVAPICH2
provides underlying support for several interfaces (such as OFA-IB,
OFA-iWARP, OFA-RoCE, PSM, Shared Memory, and TCP) for portability
across multiple networks.

%prep

%setup -q -n %{pname}-%{version}
%patch0 -p1
%patch1 -p1

%build
%ohpc_setup_compiler
./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
	    --enable-cxx \
	    --enable-g=dbg \
            --with-device=ch3:mrail \
%if 0%{?with_pwm} || 0%{?with_psm2}
            --with-device=ch3:psm \
%endif
%if 0%{with_slurm}
            --with-pm=no --with-pmi=slurm \
%endif
	    --enable-fast=O3 || { cat config.log && exit 1; }

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
%{__sed} -i -e 's#wl=""#wl="-Wl,"#g' libtool
%{__sed} -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' libtool
%endif

make %{?_smp_mflags}

%install
%ohpc_setup_compiler

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove .la files detected by rpm
rm $RPM_BUILD_ROOT/%{install_path}/lib/*.la


# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
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

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    MPI_DIR             %{install_path}
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

family "MPI"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_HOME}
%doc README.envvar
%doc COPYRIGHT
%doc CHANGELOG
%doc CHANGES
%doc README
