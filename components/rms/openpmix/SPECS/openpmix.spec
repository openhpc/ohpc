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
%global sname pmix
%global SNAME PMIX

Summary: An extended/exascale implementation of the PMIx Standard
Name: %{pname}%{PROJ_DELIM}
Version: 5.0.1
Release: 1%{?dist}
License: BSD
URL: https://openpmix.github.io/openpmix/
Group: %{PROJ_NAME}/rms
Source0: https://github.com/openpmix/openpmix/releases/download/v%{version}/%{sname}-%{version}.tar.gz
Source1: openpmix.lua

Obsoletes: pmix-%{PROJ_DELIM}

Conflicts: libev

BuildRequires: libevent-devel
BuildRequires: perl
BuildRequires: gcc
BuildRequires: gcc-c++
BuildRequires: python3-devel
BuildRequires: pandoc > 1.12
BuildRequires: libpsm2-devel
BuildRequires: zlib-devel
BuildRequires: (opa-libopamgt-devel or libopamgt-devel)
BuildRequires: hwloc%{PROJ_DELIM}
BuildRequires: munge-devel
BuildRequires: automake
BuildRequires: autoconf

#!BuildIgnore: post-build-checks

%global install_path %{OHPC_ADMIN}/%{pname}
%global module_path %{OHPC_MODULES}/%{pname}

%description
The Process Management Interface (PMI) has been used for quite some time as a
means of exchanging wireup information needed for interprocess communication. Two
versions (PMI-1 and PMI-2) have been released as part of the MPICH effort. While
PMI-2 demonstrates better scaling properties than its PMI-1 predecessor, attaining
rapid launch and wireup of the roughly 1M processes executing across 100k nodes
expected for exascale operations remains challenging.

PMI Exascale (PMIx) represents an attempt to resolve these questions by providing
an extended version of the PMI standard specifically designed to support clusters
up to and including exascale sizes. The overall objective of the project is to
eliminate some current restrictions that impact scalability, and provide
a reference implementation of the PMIx-server that demonstrates the desired level of
scalability.

This RPM contains all the tools necessary to compile and link against PMIx.


%prep
%setup -q -n %{sname}-%{version}


%build
module load hwloc

export CFLAGS="%{?cflags:%{cflags}}%{!?cflags:$RPM_OPT_FLAGS}" 
export CXXFLAGS="%{?cxxflags:%{cxxflags}}%{!?cxxflags:$RPM_OPT_FLAGS}"
export FCFLAGS="%{?fcflags:%{fcflags}}%{!?fcflags:$RPM_OPT_FLAGS}"
./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --enable-shared \
            --disable-static \
            --with-psm2 \
            --enable-sphinx=no \
            --with-opamgt \
            --with-munge \
            --with-libevent \
            --with-hwloc=$HWLOC_DIR #|| { cat config.log && exit 1; }
make %{?_smp_mflags}


%install
export ROOTDIR=$(pwd)

make install DESTDIR=${RPM_BUILD_ROOT}

# OpenPMIx Module File
mkdir -p ${RPM_BUILD_ROOT}%{module_path}
cat <<EOF > ${RPM_BUILD_ROOT}%{module_path}/%{version}.lua
%include %{SOURCE1}
EOF


%files
%{install_path}
%{module_path}
%doc README.md
%doc VERSION
%license LICENSE
