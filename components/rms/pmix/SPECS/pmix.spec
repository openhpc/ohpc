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
%global pname openpmix
%global rt_pname prrte

# Temp SHAs until formal release
%global pmix_sha 8c457aa469d87b76477da74e66622bda6d066b9e
%global pmix_sha7 8c457aa
%global prrte_sha 04f63ba18598925a68b1b7b7db2fe1cc931a6da2
%global prrte_sha7 04f63ba

%global rt_version 1.0.%{prrte_sha7}

Summary: An extended/exascale implementation of PMI
Name: %{pname}%{PROJ_DELIM}
Version: 3.1.%{pmix_sha7}
Release: 1%{?dist}
License: BSD
URL: https://openpmix.github.io/openpmix/
Group: %{PROJ_NAME}/rms
Source0: https://github.com/openpmix/openpmix/archive/%{pmix_sha}.tar.gz
Source1: https://github.com/openpmix/prrte/archive/%{prrte_sha}.tar.gz

Obsoletes: pmix%{PROJ_DELIM}

BuildRequires: libevent-devel
BuildRequires: gcc-c++
BuildRequires: cmake, automake-ohpc, autoconf-ohpc
BuildRequires: slurm-ohpc
BuildRequires: pbspro-server-ohpc

#!BuildIgnore: post-build-checks

%global install_path %{OHPC_ADMIN}/%{pname}
%global rt_install_path %{OHPC_ADMIN}/%{rt_pname}

%description
The Process Management Interface (PMI) has been used for quite some time as a
means of exchanging wireup information needed for interprocess communication. Two
versions (PMI-1 and PMI-2) have been released as part of the MPICH effort. While
PMI-2 demonstrates better scaling properties than its PMI-1 predecessor, attaining
rapid launch and wireup of the roughly 1M processes executing across 100k nodes
expected for exascale operations remains challenging.

PMI Exascale (PMIx) represents an attempt to resolve these questions by providing
an extended version of the PMI standard specifically designed to support clusters
up to and including exascale sizes. The overall objective of the project is not to
branch the existing pseudo-standard definitions - in fact, PMIx fully supports both
of the existing PMI-1 and PMI-2 APIs - but rather to (a) augment and extend those
APIs to eliminate some current restrictions that impact scalability, and (b) provide
a reference implementation of the PMI-server that demonstrates the desired level of
scalability.

This RPM contains all the tools necessary to compile and link against PMIx.


%prep
%setup -q -n %{pname}-%{pmix_sha} -b 0 -a 1
%{__mv} %{rt_pname}-%{prrte_sha} %{rt_pname}
%{__ln_s} src/.libs lib64


%build
module load cmake
PMIX_DIR=$(pwd)

./autogen.pl
CFLAGS="%{optflags}" ./configure --prefix=%{install_path} || { cat config.log && exit 1; }
%{__make} %{?_smp_mflags}

cd $PMIX_DIR/%{rt_pname}
./autogen.pl
CFLAGS="%{optflags} -L${PMIX_DIR}/lib64" ./configure --prefix=%{rt_install_path} \
                                 --with-slurm \
                                 --with-tm=/opt/pbs \
                                 --with-pmix=$PMIX_DIR || { cat config.log && exit 1; }
%{__make} %{?_smp_mflags}


%install
module load cmake
PMIX_DIR=$(pwd)

%{__make} install DESTDIR=${RPM_BUILD_ROOT}
cd $PMIX_DIR/%{rt_pname}
%{__make} install DESTDIR=${RPM_BUILD_ROOT}

%{__mkdir_p} ${RPM_BUILD_ROOT}%{OHPC_MODULES}/%{pname}
%{__cat} <<EOF > ${RPM_BUILD_ROOT}%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {
    puts stderr "This module loads the %{pname} library."
}

module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"

set     version                     %{version}

prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{pname}_DIR        %{install_path}
setenv          %{pname}_LIB        %{install_path}/lib
setenv          %{pname}_INC        %{install_path}/include

family("PMIx")

EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}%{OHPC_MODULES}/%{rt_pname}
%{__cat} <<EOF > ${RPM_BUILD_ROOT}%{OHPC_MODULES}/%{rt_pname}/%{rt_version}
#%Module1.0#####################################################################

proc ModulesHelp { } {
    puts stderr "This module loads the %{rt_pname} library."
}

module-whatis "Name: %{rt_pname}"
module-whatis "Version: %{rt_version}"

set     version                     %{rt_version}

prepend-path    MANPATH             %{rt_install_path}/share/man
prepend-path    INCLUDE             %{rt_install_path}/include
prepend-path    LD_LIBRARY_PATH     %{rt_install_path}/lib

setenv          %{rt_pname}_DIR        %{rt_install_path}
setenv          %{rt_pname}_LIB        %{rt_install_path}/lib
setenv          %{rt_pname}_INC        %{rt_install_path}/include

family("PMIx")

EOF


%files
%{install_path}
%{OHPC_MODULES}/%{pname}


##################################################################

%package -n %{rt_pname}%{PROJ_DELIM}
Summary: Reference RunTime Environment for PMIx
Version: 1.0.%{prrte_sha7}

Requires: %{pname}%{PROJ_DELIM} = %{version}
Provides: %{pname}-runtime%{PROJ_DELIM} = %{version}

%Description -n %{rt_pname}%{PROJ_DELIM}
Open MPI is an open source implementation of the Message Passing
Interface specification (https://www.mpi-forum.org/) developed and
maintained by a consortium of research, academic, and industry
partners.

Open MPI also includes an implementation of the OpenSHMEM parallel
programming API (http://www.openshmem.org/).  OpenSHMEM is a
Partitioned Global Address Space (PGAS) abstraction layer, which
provides fast inter-process communication using one-sided
communication techniques.

This RPM contains all the tools necessary to compile, link, and run
Open MPI and OpenSHMEM jobs.

%files -n %{rt_pname}%{PROJ_DELIM}
%{rt_install_path}
%{OHPC_MODULES}/%{rt_pname}
