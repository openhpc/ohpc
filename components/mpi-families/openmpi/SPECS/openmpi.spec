#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# OpenMPI stack that is dependent on compiler toolchain (and possibly RMS)
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros
%{!?RMS_DELIM: %global RMS_DELIM %{nil}}

# Base package name/config
%define pname openmpi5

%ifarch aarch64 || ppc64le
%define with_psm 0
%define with_psm2 0
%else
%if 0%{?rhel}
%define with_psm 0
%else
%define with_psm 0
%endif
%define with_psm2 0
%endif

%{!?with_lustre: %define with_lustre 0}
%{!?with_slurm: %define with_slurm 0}
%{!?with_tm: %global with_tm 1}
%if "%{RMS_DELIM}" == "-pmix"
%define with_pmix 1
%else
%define with_pmix 0
%endif
%{!?with_ofi: %define with_ofi 1}
%{!?with_ucx: %define with_ucx 1}

Summary:   A powerful implementation of MPI/SHMEM

Name:      %{pname}%{RMS_DELIM}-%{compiler_family}%{PROJ_DELIM}

Version:   5.0.5
Release:   1%{?dist}
License:   BSD-3-Clause
Group:     %{PROJ_NAME}/mpi-families
URL:       http://www.open-mpi.org
Source0:   http://www.open-mpi.org/software/ompi/v5.0/downloads/openmpi-%{version}.tar.bz2
Source3:   pbs-config
Patch0:    openmpi-5.x-pbs-config.patch

%if "%{RMS_DELIM}" != "%{nil}"
Provides: %{pname}-%{compiler_family}%{PROJ_DELIM}
Conflicts: %{pname}-%{compiler_family}%{PROJ_DELIM}
%endif

BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  libtool
BuildRequires:  make
BuildRequires:  postfix
BuildRequires:  opensm
BuildRequires:  opensm-devel
BuildRequires:  numactl
BuildRequires:  libevent-devel
%if 0%{with_pmix}
BuildRequires:  pmix%{PROJ_DELIM}
%endif
%if 0%{with_ofi}
BuildRequires:  libfabric%{PROJ_DELIM}
%if 0%{?rhel} || 0%{?openEuler}
BuildRequires:  libibverbs-devel
%endif
%ifarch x86_64
BuildRequires:  libpsm2-devel
%endif
%endif
%if 0%{with_ucx}
BuildRequires:  ucx%{PROJ_DELIM}
Requires: ucx%{PROJ_DELIM}
Requires: ucx-ib%{PROJ_DELIM}
%endif
BuildRequires:  hwloc%{PROJ_DELIM}
%if 0%{?rhel} || 0%{?openEuler}
BuildRequires: libtool-ltdl
%endif
%if 0%{with_slurm}
BuildRequires:  slurm-devel%{PROJ_DELIM}
#!BuildIgnore:  slurm%{PROJ_DELIM}
%endif

%if 0%{?suse_version}
BuildRequires:  libnuma1
BuildRequires:  sysfsutils
%else
BuildRequires:  libsysfs-devel
BuildRequires:  numactl-devel
%endif

%if %{with_lustre}
BuildRequires:  lustre-client%{PROJ_DELIM}
%endif

BuildRequires:  rdma-core-devel

%if %{with_psm}
BuildRequires:  infinipath-psm infinipath-psm-devel
%endif

%if %{with_tm}
BuildRequires:  openpbs-server%{PROJ_DELIM} openpbs-devel%{PROJ_DELIM}
BuildRequires:  openssl-devel
%endif

%if "0%{?__requires_exclude}" == "0"
%global __requires_exclude ^libpbs.so.*$
%else
%global __requires_exclude %{__requires_exclude}|^libpbs.so.*$
%endif

%if %{with_psm2}
BuildRequires:  libpsm2-devel >= 10.2.0
%endif

Requires: prun%{PROJ_DELIM} >= 1.2
#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{pname}-%{compiler_family}/%version

%description
Open MPI is an open source implementation of the Message Passing
Interface specification (http://www.mpi-forum.org/) developed and
maintained by a consortium of research, academic, and industry
partners.

Open MPI also includes an implementation of the OpenSHMEM parallel
programming API (http://www.openshmem.org/).  OpenSHMEM is a
Partitioned Global Address Space (PGAS) abstraction layer, which
provides fast inter-process communication using one-sided
communication techniques.

%prep

%setup -q -n openmpi-%{version}
%patch -P0 -p1

%build
# OpenHPC compiler designation
%ohpc_setup_compiler

BASEFLAGS="--prefix=%{install_path} --disable-static --enable-builtin-atomics --with-sge --with-libevent=external"

# build against ohpc-variant of hwloc
BASEFLAGS="$BASEFLAGS --with-hwloc=%{OHPC_LIBS}/hwloc"

# build against external pmix
%if 0%{with_pmix}
module load pmix
BASEFLAGS="$BASEFLAGS --with-pmix=${PMIX_DIR}"
%endif

%if 0%{with_ofi}
module load libfabric
BASEFLAGS="$BASEFLAGS --with-libfabric=${LIBFABRIC_DIR}"
%endif

%if 0%{with_ucx}
module load ucx
BASEFLAGS="$BASEFLAGS --with-ucx=${UCX_DIR}"
%endif

%if %{with_psm}
  BASEFLAGS="$BASEFLAGS --with-psm"
%endif
%if %{with_psm2}
  BASEFLAGS="$BASEFLAGS --with-psm2"
%endif
%if %{with_tm}
  BASEFLAGS="$BASEFLAGS --with-tm"
%endif
%if %{with_lustre}
  BASEFLAGS="$BASEFLAGS --with-io-romio-flags=--with-file-system=testfs+ufs+nfs+lustre"
%endif

export BASEFLAGS

%if %{with_tm}
%{__cp} %{SOURCE3} .
%{__chmod} 700 pbs-config
export PATH="$PWD:$PATH"
%endif

./configure ${BASEFLAGS} || { cat config.log && exit 1; }

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
%{__sed} -i -e 's#wl=""#wl="-Wl,"#g' libtool
%{__sed} -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' libtool
%endif

make %{?_smp_mflags}

%install
# OpenHPC compiler designation
%ohpc_setup_compiler
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove any .la files that might exist
%{__rm} -f $RPM_BUILD_ROOT/%{install_path}/lib/*.la
%{__rm} -f $RPM_BUILD_ROOT/%{install_path}/lib/openmpi/*.la
%{__rm} -f $RPM_BUILD_ROOT/%{install_path}/lib/pmix/*.la
%{__rm} -f $RPM_BUILD_ROOT/%{install_path}/lib/prte/*.la

# rename to avoid name collision with OpenHPC's prun
mv $RPM_BUILD_ROOT/%{install_path}/bin/prun $RPM_BUILD_ROOT/%{install_path}/bin/prrte-prun

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

setenv          MPI_DIR             %{install_path}
setenv          OMPI_MCA_mca_base_component_show_load_errors 0
%if 0%{with_pmix}
setenv          OHPC_MPI_LAUNCHERS  pmix
%endif
prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

depends-on hwloc
%if 0%{with_ucx}
depends-on ucx
%endif
%if 0%{with_ofi}
depends-on libfabric
%endif
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
%{install_path}
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%doc README.md
%doc LICENSE
%doc AUTHORS
