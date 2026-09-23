#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
# OpenHPC:check-updates:version-pin 0.7.1

%include %{_sourcedir}/OHPC_macros
%global pname flux-pmix

Summary:	PMIx support for the Flux resource manager
Name:		%{pname}%{PROJ_DELIM}
Version:	0.7.1
Release:	1%{?dist}
License:	LGPL-3.0
Group:		%{PROJ_NAME}/rms
URL:		https://github.com/flux-framework/flux-pmix
Source0:	https://github.com/flux-framework/flux-pmix/releases/download/v%{version}/%{pname}-%{version}.tar.gz

# flux-core's own -Werror build already disables -Wstrict-aliasing,
# but rpm's hardened-build GCC specs re-enable it; same workaround as
# flux-core.spec/flux-sched.spec.
%undefine _hardened_build

# This package installs under /usr, not %{OHPC_HOME}, so it falls
# outside ohpc-filesystem's fileattrs-based dependency "coloring"
# (%__ohpc_path in its /usr/lib/rpm/fileattrs/ohpc.attr, which only
# matches %{OHPC_HOME} plus a few explicitly special-cased /usr paths
# for slurm/openpbs binaries -- not this package's /usr/lib64/flux
# tree). Without that coloring, rpmbuild's plain elfdeps auto-Requires
# a bare "libpmix.so.2()(64bit)", which the base distro's own pmix
# package satisfies just as well as pmix-ohpc, and dnf is free to pick
# either one. Filter that bare auto-Requires out; the explicit
# "Requires: pmix-ohpc" below, an RPM name rather than a soname, is
# unambiguous and is meant to be the only way this dependency is
# expressed.
%global __requires_exclude ^libpmix\\.so.*$

BuildRequires:	make
BuildRequires:	gcc
BuildRequires:	pkgconfig
BuildRequires:	jansson-devel
BuildRequires:	pmix-ohpc
BuildRequires:	flux-core-ohpc

Requires:	pmix-ohpc
Requires:	flux-core-ohpc

#!BuildIgnore: post-build-checks

%description
flux-pmix adds PMIx support to Flux: a flux-shell plugin that lets
jobs bootstrap over PMIx instead of Flux's default simple PMI-1/2,
and a PMI client plugin for the reverse case of Flux itself being
launched by a foreign PMIx-based launcher.

Flux's own default simple PMI works for mpich, mvapich2, and Intel
MPI without this package. It is specifically needed for some
versions of OpenMPI, whose runtime does not correctly bootstrap over
Flux's default simple PMI and falls back to running disconnected
singleton processes instead of one real parallel job. Once installed,
the plugin is requested per job with "flux run -o pmi=pmix ...".

%prep
%setup -q -n %{pname}-%{version}

%build
# Like flux-core/flux-security/flux-sched, this builds with the
# system compiler, not any OpenHPC compiler-family toolchain; purge
# first so a build host's sticky default module collection (e.g. from
# lmod-defaults-*-ohpc) can't silently swap in one of those instead.
module purge
module load pmix
export PKG_CONFIG_PATH="${PMIX_LIB}/pkgconfig:${PKG_CONFIG_PATH}"

# Filtering the auto-Requires above only affects package-level
# dependency resolution; without this, the dynamic linker itself
# would still fall back to its normal search path at runtime and
# could load the base distro's libpmix.so.2 instead of pmix-ohpc's,
# e.g. if something else on the system pulls the distro package in.
# An explicit RPATH makes the actual loaded library unambiguous too,
# regardless of what else happens to be installed.
export LDFLAGS="${LDFLAGS} -Wl,-rpath,${PMIX_LIB}"

%configure --without-openmpi
%{__make} %{?_smp_mflags} CFLAGS="%{optflags} -Wno-error=strict-aliasing"

%install
%{__make} install DESTDIR=%{buildroot}

find %{buildroot} -name '*.la' -delete

%files
%{_libdir}/flux
%doc README.md NEWS.md
%license LICENSE
