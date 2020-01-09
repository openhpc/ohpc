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
%define with_openib 0
%define with_mofed 1

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
%if %{with_mofed}
BuildRequires: ar_mgr >= 1.0-0.42.g750eb1e.46101
BuildRequires: cc_mgr >= 1.0-0.41.g750eb1e.46101
BuildRequires: dump_pr >= 1.0-0.37.g750eb1e.46101
BuildRequires: hcoll >= 4.3.2708-1.46101
BuildRequires: ibacm >= 41mlnx1-OFED.4.3.3.0.0.46101
BuildRequires: ibdump >= 5.0.0-3.46101
BuildRequires: ibsim >= 0.7mlnx1-0.11.g85c342b.46101
BuildRequires: ibutils >= 1.5.7.1-0.12.gdcaeae2.46101
BuildRequires: ibutils2 >= 2.1.1-0.104.MLNX20190408.gb55795e.46101
BuildRequires: infiniband-diags >= 5.4.0.MLNX20190422.d1468cd-0.1.46101
BuildRequires: infiniband-diags-compat >= 5.4.0.MLNX20190422.d1468cd-0.1.46101
BuildRequires: libibcm >= 41mlnx1-OFED.4.1.0.1.0.46101
BuildRequires: libibcm-devel >= 41mlnx1-OFED.4.1.0.1.0.46101
BuildRequires: libibmad >= 5.4.0.MLNX20190423.1d917ae-0.1.46101
BuildRequires: libibmad-devel >= 5.4.0.MLNX20190423.1d917ae-0.1.46101
BuildRequires: libibmad-static >= 5.4.0.MLNX20190423.1d917ae-0.1.46101
BuildRequires: libibumad >= 43.1.1.MLNX20190422.87b4d9b-0.1.46101
BuildRequires: libibumad-devel >= 43.1.1.MLNX20190422.87b4d9b-0.1.46101
BuildRequires: libibumad-static >= 43.1.1.MLNX20190422.87b4d9b-0.1.46101
BuildRequires: libibverbs >= 41mlnx1-OFED.4.6.0.4.1.46101
BuildRequires: libibverbs-devel >= 41mlnx1-OFED.4.6.0.4.1.46101
BuildRequires: libibverbs-devel-static >= 41mlnx1-OFED.4.6.0.4.1.46101
BuildRequires: libibverbs-utils >= 41mlnx1-OFED.4.6.0.4.1.46101
BuildRequires: libmlx4 >= 41mlnx1-OFED.4.5.0.0.3.46101
BuildRequires: libmlx4-devel >= 41mlnx1-OFED.4.5.0.0.3.46101
BuildRequires: libmlx5 >= 41mlnx1-OFED.4.6.0.0.4.46101
BuildRequires: libmlx5-devel >= 41mlnx1-OFED.4.6.0.0.4.46101
BuildRequires: librdmacm >= 41mlnx1-OFED.4.6.0.0.1.46101
BuildRequires: librdmacm-devel >= 41mlnx1-OFED.4.6.0.0.1.46101
BuildRequires: librdmacm-utils >= 41mlnx1-OFED.4.6.0.0.1.46101
BuildRequires: librxe >= 41mlnx1-OFED.4.4.2.4.6.46101
BuildRequires: librxe-devel-static >= 41mlnx1-OFED.4.4.2.4.6.46101
BuildRequires: mft >= 4.12.0-105
BuildRequires: mlnx-ethtool >= 4.19-1.46101
BuildRequires: mlnx-iproute2 >= 4.20.0-1.46101
BuildRequires: mstflint >= 4.11.0-1.14.g840c9c2.46101
BuildRequires: ofed-scripts >= 4.6-OFED.4.6.1.0.1
BuildRequires: opensm >= 5.4.0.MLNX20190422.ed81811-0.1.46101
BuildRequires: opensm-devel >= 5.4.0.MLNX20190422.ed81811-0.1.46101
BuildRequires: opensm-libs >= 5.4.0.MLNX20190422.ed81811-0.1.46101
BuildRequires: opensm-static >= 5.4.0.MLNX20190422.ed81811-0.1.46101
BuildRequires: perftest >= 4.4-0.5.g1ceab48.46101
BuildRequires: qperf >= 0.4.9-9.46101
BuildRequires: sharp >= 1.8.1.MLNX20190422.6c05a05-1.46101
BuildRequires: srptools >= 41mlnx1-5.46101
BuildRequires: knem
BuildRequires: systemd-devel
Requires: ar_mgr >= 1.0-0.42.g750eb1e.46101
Requires: cc_mgr >= 1.0-0.41.g750eb1e.46101
Requires: dump_pr >= 1.0-0.37.g750eb1e.46101
Requires: hcoll >= 4.3.2708-1.46101
Requires: ibacm >= 41mlnx1-OFED.4.3.3.0.0.46101
Requires: ibdump >= 5.0.0-3.46101
Requires: ibsim >= 0.7mlnx1-0.11.g85c342b.46101
Requires: ibutils >= 1.5.7.1-0.12.gdcaeae2.46101
Requires: ibutils2 >= 2.1.1-0.104.MLNX20190408.gb55795e.46101
Requires: infiniband-diags >= 5.4.0.MLNX20190422.d1468cd-0.1.46101
Requires: infiniband-diags-compat >= 5.4.0.MLNX20190422.d1468cd-0.1.46101
Requires: libibcm >= 41mlnx1-OFED.4.1.0.1.0.46101
Requires: libibcm-devel >= 41mlnx1-OFED.4.1.0.1.0.46101
Requires: libibmad >= 5.4.0.MLNX20190423.1d917ae-0.1.46101
Requires: libibmad-devel >= 5.4.0.MLNX20190423.1d917ae-0.1.46101
Requires: libibmad-static >= 5.4.0.MLNX20190423.1d917ae-0.1.46101
Requires: libibumad >= 43.1.1.MLNX20190422.87b4d9b-0.1.46101
Requires: libibumad-devel >= 43.1.1.MLNX20190422.87b4d9b-0.1.46101
Requires: libibumad-static >= 43.1.1.MLNX20190422.87b4d9b-0.1.46101
Requires: libibverbs >= 41mlnx1-OFED.4.6.0.4.1.46101
Requires: libibverbs-devel >= 41mlnx1-OFED.4.6.0.4.1.46101
Requires: libibverbs-devel-static >= 41mlnx1-OFED.4.6.0.4.1.46101
Requires: libibverbs-utils >= 41mlnx1-OFED.4.6.0.4.1.46101
Requires: libmlx4 >= 41mlnx1-OFED.4.5.0.0.3.46101
Requires: libmlx4-devel >= 41mlnx1-OFED.4.5.0.0.3.46101
Requires: libmlx5 >= 41mlnx1-OFED.4.6.0.0.4.46101
Requires: libmlx5-devel >= 41mlnx1-OFED.4.6.0.0.4.46101
Requires: librdmacm >= 41mlnx1-OFED.4.6.0.0.1.46101
Requires: librdmacm-devel >= 41mlnx1-OFED.4.6.0.0.1.46101
Requires: librdmacm-utils >= 41mlnx1-OFED.4.6.0.0.1.46101
Requires: librxe >= 41mlnx1-OFED.4.4.2.4.6.46101
Requires: librxe-devel-static >= 41mlnx1-OFED.4.4.2.4.6.46101
Requires: mft >= 4.12.0-105
Requires: mlnx-ethtool >= 4.19-1.46101
Requires: mlnx-iproute2 >= 4.20.0-1.46101
Requires: mstflint >= 4.11.0-1.14.g840c9c2.46101
Requires: ofed-scripts >= 4.6-OFED.4.6.1.0.1
Requires: opensm >= 5.4.0.MLNX20190422.ed81811-0.1.46101
Requires: opensm-devel >= 5.4.0.MLNX20190422.ed81811-0.1.46101
Requires: opensm-libs >= 5.4.0.MLNX20190422.ed81811-0.1.46101
Requires: opensm-static >= 5.4.0.MLNX20190422.ed81811-0.1.46101
Requires: perftest >= 4.4-0.5.g1ceab48.46101
Requires: qperf >= 0.4.9-9.46101
Requires: sharp >= 1.8.1.MLNX20190422.6c05a05-1.46101
Requires: srptools >= 41mlnx1-5.46101
Requires: knem
Requires: systemd-devel
%endif


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


BASEFLAGS="--prefix=%{install_path} --disable-static --enable-builtin-atomics --with-sge --enable-mpi-cxx"

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
%if %{with_mofed}
  KNEM_DIR=$(find /opt -maxdepth 1 -type d -name "knem*" -print0)
  HCOLL_DIR=/opt/mellanox/hcoll
  BASEFLAGS="$BASEFLAGS --with-hcoll=$HCOLL_DIR --with-knem=$KNEM_DIR"
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
