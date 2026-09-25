#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
# OpenHPC:check-updates:version-pin 0.89.0

%include %{_sourcedir}/OHPC_macros
%global pname flux-core

Summary:	Core components of the Flux resource manager framework
Name:		%{pname}%{PROJ_DELIM}
Version:	0.89.0
Release:	1%{?dist}
License:	LGPL-3.0
Group:		%{PROJ_NAME}/rms
URL:		https://github.com/flux-framework/flux-core
Source0:	https://github.com/flux-framework/flux-core/releases/download/v%{version}/%{pname}-%{version}.tar.gz

# flux-core's own -Werror build already disables -Wstrict-aliasing,
# but rpm's hardened-build GCC specs re-enable it, which then breaks
# the build against the vendored libev headers. Same class of problem
# slurm.spec works around; disable the hardened build here too.
%undefine _hardened_build

# hwloc-ohpc's SONAME (libhwloc.so.15) matches the base distro's own
# hwloc-libs, and this package installs under /usr, not %{OHPC_HOME},
# so it falls outside ohpc-filesystem's fileattrs-based dependency
# coloring (same root cause as flux-pmix's libpmix.so situation).
# Without this, rpmbuild's auto-Requires would be a bare
# libhwloc.so.15()(64bit), satisfiable by either package. Filter it
# out; the explicit "Requires: hwloc-ohpc" below is the only way this
# dependency is meant to be expressed.
%global __requires_exclude ^libhwloc\\.so.*$

BuildRequires:	make
BuildRequires:	gcc
BuildRequires:	gcc-c++
BuildRequires:	pkgconfig
BuildRequires:	systemd-devel
BuildRequires:	zeromq-devel
BuildRequires:	jansson-devel
BuildRequires:	libuuid-devel
BuildRequires:	lz4-devel
BuildRequires:	libarchive-devel
BuildRequires:	hwloc-ohpc
BuildRequires:	sqlite-devel
BuildRequires:	ncurses-devel
BuildRequires:	lua
BuildRequires:	lua-devel
BuildRequires:	python3-devel
BuildRequires:	python3-cffi
BuildRequires:	python3-ply
BuildRequires:	python3-pyyaml
BuildRequires:	python3-setuptools
BuildRequires:	python3-sphinx
BuildRequires:	flux-security-ohpc
BuildRequires:	systemd-rpm-macros
%{?systemd_requires}

Requires:	munge
Requires:	lua
Requires:	python3
Requires:	python3-cffi
Requires:	python3-pyyaml
Requires:	hwloc-ohpc
Requires:	flux-security-ohpc >= 0.13.0

#!BuildIgnore: post-build-checks

%description
flux-core implements the lowest level services and interfaces for the
Flux resource manager framework: the flux-broker, the content store,
the KVS, and the default (non-Fluxion) FIFO scheduler. It is the
first building block used to compose a Flux resource manager and is
intended here as an alternative to Slurm.

This build links against flux-security (--with-flux-security) so
flux-core can start job processes as the submitting user rather than
only as the user running the Flux instance, which is required for a
shared, multi-user cluster deployment. flux-sched (Fluxion), which
replaces the default FIFO scheduler with a graph-based one, is a
separate, optional package.

%prep
%setup -q -n %{pname}-%{version}

%build
# hwloc-ohpc, not the base distro's hwloc-devel: like slurm.spec,
# this avoids a real dependency-resolution hazard on distros whose
# hwloc-libs was built with optional OpenCL device detection, which
# makes it Require the OpenCL loader capability, satisfiable by
# either the tiny distro ocl-icd package or, if a build host also has
# an Intel oneAPI repo enabled (e.g. for building this project's
# other Intel-toolchain components), several hundred MB of Intel's
# own OpenCL runtime; dnf's solver isn't guaranteed to prefer the
# former. hwloc-ohpc's own build has no OpenCL detection at all, so
# it never has this Requires in the first place. Purge first so a
# build host's sticky default module collection (e.g. from
# lmod-defaults-*-ohpc) can't silently swap in an OpenHPC compiler
# toolchain instead of the system compiler this still builds with.
module purge
module load hwloc
export PKG_CONFIG_PATH="${HWLOC_LIB}/pkgconfig:${PKG_CONFIG_PATH}"

# hwloc-ohpc's library lives outside the default runtime linker
# search path; without an RPATH, the built flux-core binaries would
# only find libhwloc.so at run time if "module load hwloc" happened
# to already be active, which nothing in a real deployment does.
export LDFLAGS="${LDFLAGS} -Wl,-rpath,${HWLOC_LIB}"

%configure \
	--with-flux-security \
	--with-systemdsystemunitdir=%{_unitdir}

# flux-core's own -Werror build sets -Wno-strict-aliasing, but rpm's
# _FORTIFY_SOURCE=3 and annobin build flags re-trigger strict-aliasing
# diagnostics in its vendored libev headers, turning them back into
# hard errors. Demote just that diagnostic back to a warning; this
# should really be fixed upstream in the vendored libev copy.
%{__make} %{?_smp_mflags} CFLAGS="%{optflags} -Wno-error=strict-aliasing"

%install
%{__make} install DESTDIR=%{buildroot}

# don't package static libs or libtool archives
find %{buildroot} -name '*.la' -o -name '*.a' -delete

# The Python cffi extension modules (_flux/_core.so etc.) embed a
# standard, non-security "/usr/lib" runpath. That is rpm's lowest QA
# severity bucket (redundant, not insecure); allow just that one.
export QA_RPATHS=$(( 0x0001 ))

# upstream ships no default site conf.d/cron.d content, so "make
# install" never creates these directories
%{__mkdir_p} %{buildroot}%{_sysconfdir}/flux/system/conf.d
%{__mkdir_p} %{buildroot}%{_sysconfdir}/flux/system/cron.d

# Make the installed python module symlinks relative instead of
# absolute, same reasoning as the extrae-uncore fix. The directory is
# named "pythonX.Y", not just "X.Y".
ln -sfn ../../python%{python3_version}/site-packages/_flux \
	%{buildroot}%{_libdir}/flux/python%{python3_version}/_flux
ln -sfn ../../python%{python3_version}/site-packages/flux \
	%{buildroot}%{_libdir}/flux/python%{python3_version}/flux

%pre
# provide specific uid/gid to ensure that it is the same across the cluster
#
# Unlike slurm's own system user, flux needs a real shell, not
# /sbin/nologin: with nologin, rc1's "flux modprobe rc1" step (which
# loads the Fluxion scheduler modules, among others) fails immediately
# with "This account is currently not available" (nologin's own
# message), which aborts that broker rank's startup entirely.
/usr/bin/getent group flux >/dev/null 2>&1 || \
  /usr/sbin/groupadd -r flux -g 359
/usr/bin/getent passwd flux >/dev/null 2>&1 || \
  /usr/sbin/useradd -c "Flux resource manager" \
  -d %{_sysconfdir} -g flux -s /bin/bash -r flux -u 359

exit 0

%post
%systemd_post flux.service

%preun
%systemd_preun flux.service

%postun
%systemd_postun_with_restart flux.service

%files
%{_bindir}/flux*
%{_libdir}/libflux-*.so*
%{_libdir}/flux
%{_libdir}/lua
%{_libdir}/pkgconfig/flux*.pc
%{_libexecdir}/flux
%{_includedir}/flux
%{_mandir}/man*/*
%{_datadir}/flux
%{_datadir}/lua
%{_datadir}/bash-completion
%{python3_sitearch}/flux
%{python3_sitearch}/_flux
%{_unitdir}/flux.service
%{_unitdir}/flux-prolog@.service
%{_unitdir}/flux-epilog@.service
%{_unitdir}/flux-housekeeping@.service
%{_tmpfilesdir}/flux.conf
%{_sysconfdir}/cron.daily/50-flux-dump
%{_sysconfdir}/cron.daily/51-flux-gc
%dir %{_sysconfdir}/flux
%{_sysconfdir}/flux/shell
%dir %{_sysconfdir}/flux/system
%dir %{_sysconfdir}/flux/system/conf.d
%dir %{_sysconfdir}/flux/system/cron.d
%doc README.md NEWS.md
%license LICENSE
