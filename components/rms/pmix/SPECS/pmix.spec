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
%global pname pmix

Summary: An extended/exascale implementation of PMI
Name: %{pname}%{PROJ_DELIM}
Version: 2.2.2
Release: 1%{?dist}
License: BSD
URL: https://pmix.github.io/pmix/
Group: %{PROJ_NAME}/rms
Source: https://github.com/pmix/pmix/releases/download/v%{version}/pmix-%{version}.tar.bz2

BuildRequires: libevent-devel
BuildRequires: lmod-ohpc libtool-ohpc
BuildRequires: gcc-c++
#!BuildIgnore: post-build-checks

%global install_path %{OHPC_ADMIN}/%{pname}

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
%setup -q -n %{pname}-%{version}

%build
CFLAGS="%{optflags}" ./configure --prefix=%{install_path} \
         --libdir=%{install_path}/lib \
         || { cat config.log && exit 1; }
%{__make} %{?_smp_mflags}

%install
%{__make} install DESTDIR=${RPM_BUILD_ROOT}

%{__mkdir_p} ${RPM_BUILD_ROOT}%{OHPC_MODULES}/%{pname}
cat <<EOF > ${RPM_BUILD_ROOT}%{OHPC_MODULES}/%{pname}/%{version}
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

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%files
%{OHPC_ADMIN}
%{OHPC_MODULES}/%{pname}
