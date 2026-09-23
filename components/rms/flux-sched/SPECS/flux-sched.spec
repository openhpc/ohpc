#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
# OpenHPC:check-updates:version-pin 0.55.0

%include %{_sourcedir}/OHPC_macros
%global pname flux-sched

Summary:	Fluxion graph-based scheduler for the Flux resource manager
Name:		%{pname}%{PROJ_DELIM}
Version:	0.55.0
Release:	1%{?dist}
License:	LGPL-3.0
Group:		%{PROJ_NAME}/rms
URL:		https://github.com/flux-framework/flux-sched
Source0:	https://github.com/flux-framework/flux-sched/releases/download/v%{version}/%{pname}-%{version}.tar.gz

# flux-core's own -Werror build already disables -Wstrict-aliasing,
# but rpm's hardened-build GCC specs re-enable it. flux-sched shares
# common sources with flux-core, so apply the same workaround as
# flux-core.spec here too.
%undefine _hardened_build

# hwloc-ohpc's SONAME (libhwloc.so.15) matches the base distro's own
# hwloc-libs, and this package installs under /usr, not %{OHPC_HOME},
# so it falls outside ohpc-filesystem's fileattrs-based dependency
# coloring (same root cause as flux-pmix's libpmix.so situation, and
# the same fix already applied to flux-core.spec). Without this,
# rpmbuild's auto-Requires would be a bare libhwloc.so.15()(64bit),
# satisfiable by either package. Filter it out; the explicit
# "Requires: hwloc-ohpc" below is the only way this dependency is
# meant to be expressed.
%global __requires_exclude ^libhwloc\\.so.*$

BuildRequires:	cmake
BuildRequires:	make
BuildRequires:	gcc
BuildRequires:	gcc-c++
BuildRequires:	pkgconfig
BuildRequires:	boost-devel
BuildRequires:	boost-graph
BuildRequires:	libedit-devel
BuildRequires:	hwloc-ohpc
BuildRequires:	jansson-devel
BuildRequires:	libuuid-devel
BuildRequires:	yaml-cpp-devel
BuildRequires:	python3-devel
BuildRequires:	python3-pyyaml
BuildRequires:	python3-jsonschema
BuildRequires:	python3-sphinx
BuildRequires:	flux-core-ohpc

Requires:	flux-core-ohpc >= 0.78.0
Requires:	python3-pyyaml
Requires:	python3-jsonschema
Requires:	hwloc-ohpc

#!BuildIgnore: post-build-checks

%description
Fluxion is a graph-based scheduler for the Flux resource manager
framework. It replaces flux-core's default first-come-first-served
scheduler with a more sophisticated one that supports configurable
queuing policies and matches resource requests against a graph model
of the cluster's hardware.

It installs two modules loaded by the flux broker:
  - sched-fluxion-resource, which matches resource requests to
    available resources using Fluxion's graph-based algorithm
  - sched-fluxion-qmanager, which manages one or more prioritized job
    queues

This package requires flux-core (>= 0.78.0, the minimum version
Fluxion's own build enforces) to already be installed, since its
CMake-based build discovers flux-core via pkg-config and by running
the installed flux binary.

%prep
%setup -q -n %{pname}-%{version}

%build
# hwloc-ohpc, not the base distro's hwloc-devel; same rationale as
# flux-core.spec. Purge first so a build host's sticky default module
# collection (e.g. from lmod-defaults-*-ohpc) can't silently swap in
# an OpenHPC compiler toolchain instead of the system compiler this
# still builds with.
module purge
module load hwloc
export PKG_CONFIG_PATH="${HWLOC_LIB}/pkgconfig:${PKG_CONFIG_PATH}"

# hwloc-ohpc's library lives outside the default runtime linker
# search path; without an RPATH, the built binaries would only find
# libhwloc.so at run time if "module load hwloc" happened to already
# be active, which nothing in a real deployment does. CMake picks up
# LDFLAGS for its initial linker flags the same way autotools does.
export LDFLAGS="${LDFLAGS} -Wl,-rpath,${HWLOC_LIB}"

# flux-core's own -Werror build sets -Wno-strict-aliasing; same
# rationale as flux-core.spec's CFLAGS override.
#
# CMAKE_INSTALL_SYSCONFDIR is forced explicitly rather than relying
# on GNUInstallDirs' "prefix is exactly /usr" auto-absolutizing
# convention, which is what left the etc/rc1.d, etc/rc3.d and
# etc/modprobe install() rules writing under a path %files never
# found. ENABLE_DOCS=On turns the "Try find sphinx-build, silently
# skip docs if not found" default into a hard failure instead, so a
# missing man page shows the real reason instead of nothing.
cmake \
	-DCMAKE_INSTALL_PREFIX=%{_prefix} \
	-DCMAKE_INSTALL_SYSCONFDIR=%{_sysconfdir} \
	-DENABLE_DOCS=On \
	-DCMAKE_C_FLAGS="%{optflags} -Wno-error=strict-aliasing" \
	-DCMAKE_CXX_FLAGS="%{optflags} -Wno-error=strict-aliasing" \
	-B build
%{__make} %{?_smp_mflags} -C build

# doc/CMakeLists.txt's "manpages" target has no ALL keyword, so it is
# opt-in and is never built by the default "all" target above,
# regardless of ENABLE_DOCS; its install() rule exists but nothing
# populates the directory it copies from unless this runs too.
%{__make} -C build manpages

%install
%{__make} -C build install DESTDIR=%{buildroot}

# The Python cffi/CMake-installed extension modules may embed a
# standard, non-security "/usr/lib" runpath; same rationale as
# flux-core.spec.
export QA_RPATHS=$(( 0x0001 ))

# CMake installs this as an absolute symlink to the fluxion python
# package under site-packages (purelib, not sitearch -- fluxion is
# pure python); make it relative, same fix as flux-core's _flux/flux
# symlinks. Compute the relative path instead of hardcoding lib vs
# lib64, since guessing that wrong is exactly what broke this here.
FLUXION_PYLINK=%{buildroot}%{_libdir}/flux/python%{python3_version}/fluxion
if [ -L "$FLUXION_PYLINK" ]; then
	ABS_TARGET=$(readlink "$FLUXION_PYLINK")
	REL_TARGET=$(realpath --relative-to="$(dirname "$FLUXION_PYLINK")" "%{buildroot}${ABS_TARGET}")
	ln -sfn "$REL_TARGET" "$FLUXION_PYLINK"
fi

%files
%{_libdir}/flux
%{_libdir}/*.so*
%{_libexecdir}/flux
%{python3_sitelib}/fluxion
%{_mandir}/man*/*
%{_sysconfdir}/flux/rc1.d
%{_sysconfdir}/flux/rc3.d
%{_sysconfdir}/flux/modprobe/rc1.d
%{_sysconfdir}/flux/modprobe/modprobe.d
%doc README.md NEWS.md
%license LICENSE
