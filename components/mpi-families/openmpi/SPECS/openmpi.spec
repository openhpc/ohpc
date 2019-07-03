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
%define pname openmpi3
%define with_openib 1

%ifarch aarch64 || ppc64le
%define with_psm 0
%define with_psm2 0
%else
%define with_psm 1
%define with_psm2 1
%endif

%{!?with_lustre: %define with_lustre 0}
%{!?with_slurm: %define with_slurm 0}
%{!?with_tm: %global with_tm 1}
%{!?with_pmix: %define with_pmix 0}

Summary:   A powerful implementation of MPI

Name:      %{pname}%{RMS_DELIM}-%{compiler_family}%{PROJ_DELIM}

Version:   3.1.4
Release:   1%{?dist}
License:   BSD-3-Clause
Group:     %{PROJ_NAME}/mpi-families
URL:       http://www.open-mpi.org
Source0:   http://www.open-mpi.org/software/ompi/v3.1/downloads/openmpi-%{version}.tar.bz2
Source3:   pbs-config
Patch0:    openmpi-3.0-pbs-config.patch

%if "%{RMS_DELIM}" != "%{nil}"
Provides: %{pname}-%{compiler_family}%{PROJ_DELIM}
Conflicts: %{pname}-%{compiler_family}%{PROJ_DELIM}
%endif

BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  libtool
BuildRequires:  postfix
BuildRequires:  opensm
BuildRequires:  opensm-devel
BuildRequires:  numactl
%if 0%{with_pmix}
BuildRequires:  pmix%{PROJ_DELIM}
BuildRequires:  libevent-devel
%endif
BuildRequires:  hwloc-devel
%if 0%{?rhel}
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

%if %{with_openib}
BuildRequires:  rdma-core-devel
%endif

%if %{with_psm}
BuildRequires:  infinipath-psm infinipath-psm-devel
%endif

%if %{with_tm}
BuildRequires:  pbspro-server%{PROJ_DELIM}
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

Open MPI is a project combining technologies and resources from several
other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to
build the best MPI library available.

This RPM contains all the tools necessary to compile, link, and run
Open MPI jobs.

%prep

%setup -q -n openmpi-%{version}
%patch0 -p1

%build
# OpenHPC compiler designation
%ohpc_setup_compiler


BASEFLAGS="--prefix=%{install_path} --libdir=%{install_path}/lib --disable-static --enable-builtin-atomics --with-sge --enable-mpi-cxx"

# build against external pmix and libevent
%if 0%{with_pmix}
module load pmix
BASEFLAGS="$BASEFLAGS --with-pmix=${PMIX_DIR}"
BASEFLAGS="$BASEFLAGS --with-libevent=external --with-hwloc=external"
%endif

%if %{with_psm}
  BASEFLAGS="$BASEFLAGS --with-psm"
%endif
%if %{with_psm2}
  BASEFLAGS="$BASEFLAGS --with-psm2"
%endif
%if %{with_tm}
  BASEFLAGS="$BASEFLAGS --with-tm=/opt/pbs/"
%endif
%if %{with_openib}
  BASEFLAGS="$BASEFLAGS --with-verbs"
%endif
%if %{with_lustre}
  BASEFLAGS="$BASEFLAGS --with-io-romio-flags=--with-file-system=testfs+ufs+nfs+lustre"
%endif

export BASEFLAGS

%if %{with_tm}
cp %{SOURCE3} .
%{__chmod} 700 pbs-config
export PATH="./:$PATH"
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
# Remove .la files detected by rpm

rm $RPM_BUILD_ROOT/%{install_path}/lib/*.la

# rename prun to avoid namespace conflict with ohpc
%{__mv} $RPM_BUILD_ROOT/%{install_path}/bin/prun $RPM_BUILD_ROOT/%{install_path}/bin/prun.ompi
%{__mv} $RPM_BUILD_ROOT/%{install_path}/share/man/man1/prun.1 $RPM_BUILD_ROOT/%{install_path}/share/man/man1/prun.ompi.1

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
%if 0%{with_pmix}
setenv          OHPC_MPI_LAUNCHERS  pmix
%endif
prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}-%{pname}
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
%{install_path}
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%doc NEWS
%doc README
%doc LICENSE
%doc AUTHORS
%doc README.JAVA.txt
