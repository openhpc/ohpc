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

%define pname golang

# OpenHPC-provided Go toolchain, built from upstream source. It exists for
# distributions whose packaged Go is too old to build current OpenHPC Go
# components (notably Warewulf): openEuler ships Go 1.21 by default and 1.24 at
# newest, below Warewulf's go.mod requirement. Enterprise Linux ships a current
# go-toolset, so this package is built and shipped only for openEuler.

Summary:   The Go programming language toolchain
Name:      %{pname}%{PROJ_DELIM}
Version:   1.26.8
Release:   %{?dist}.1
License:   BSD-3-Clause
Group:     %{PROJ_NAME}/dev-tools
URL:       https://go.dev/
Source0:   https://go.dev/dl/go%{version}.src.tar.gz

# Go 1.26 requires >= go1.24.6 as its bootstrap toolchain. openEuler ships that
# as the versioned "golang-1.24" package under a multiversion prefix.
%global bootstrap_goroot /usr/lib/golang-multiversion/golang-1.24
BuildRequires: golang-1.24 >= 1.24.6
BuildRequires: gcc
BuildRequires: glibc-static
BuildRequires: hostname
BuildRequires: rsync

ExclusiveArch: x86_64 aarch64

# Keep this deliberately separate from the distro Go: the package is named
# golang-ohpc and provides ONLY that capability. Auto-generated provides from
# the Go tree are suppressed below (__provides_exclude_from), so this never
# emits a bare "golang"/"go" that could satisfy or collide with the system
# package. State the capability explicitly for consumers (e.g. warewulf's
# openEuler build BuildRequires golang-ohpc and loads it as a module).
Provides: golang%{PROJ_DELIM} = %{version}-%{release}

%ifarch x86_64
%global gohostarch amd64
%endif
%ifarch aarch64
%global gohostarch arm64
%endif

%define install_path %{OHPC_UTILS}/%{pname}/%version
%define goroot        %{install_path}

# Go ships its own toolchain binaries and a full source tree under GOROOT.
# Keep RPM's dependency generator and binary post-processing away from it:
#  - do not strip (Go manages its own binaries; stripping can break the linker)
#  - do not auto-generate provides/requires from the shipped Go tree
%global __strip /bin/true
%global __os_install_post %{nil}
%global __requires_exclude_from ^%{install_path}/.*$
%global __provides_exclude_from ^%{install_path}/.*$

%description
The Go programming language toolchain (compiler, linker, standard library and
tools), packaged for OpenHPC under %{OHPC_HOME} with an environment module.
Provided for distributions whose native Go is too old to build current OpenHPC
Go components (notably Warewulf).

%prep
%setup -q -n go

%build
export GOROOT_BOOTSTRAP=%{bootstrap_goroot}
export GOROOT_FINAL=%{goroot}
export GOTOOLCHAIN=local
export GOHOSTOS=linux
export GOHOSTARCH=%{gohostarch}
export GOOS=linux
export GOARCH=%{gohostarch}
export CGO_ENABLED=1
export CC=gcc
export CFLAGS="$RPM_OPT_FLAGS"
export LDFLAGS="$RPM_LD_FLAGS"

cd src
./make.bash -v
cd ..

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}%{goroot}

# Install the built GOROOT tree. rsync -a (the OHPC convention for copying a
# prebuilt tree; Go has no "make install") preserves modes, symlinks and
# timestamps. Exclude the test suite and the build object cache, which are not
# needed at runtime; the rest of GOROOT (bin, pkg, src, lib, api, ...) ships as
# built, so no per-release file list to maintain.
rsync -a --exclude='test/' --exclude='pkg/obj/' ./ %{buildroot}%{goroot}/

# OpenHPC environment module
%{__mkdir_p} %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {
        puts stderr " "
        puts stderr "This module loads the %{pname} programming language toolchain."
        puts stderr "It sets GOROOT and prepends the Go bin directory to PATH."
        puts stderr "\nVersion %{version}\n"
}

module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: development tools"
module-whatis "Keywords: System, Utility, Compiler"
module-whatis "Description: %{summary}"
module-whatis "URL: %{url}"

set             version         %{version}

setenv          GOROOT          %{goroot}
prepend-path    PATH            %{goroot}/bin
# Keep builds offline/reproducible: never auto-download a toolchain.
setenv          GOTOOLCHAIN     local
EOF

%{__cat} << EOF > %{buildroot}%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%dir %{OHPC_UTILS}
%{OHPC_UTILS}/%{pname}
%{OHPC_MODULES}/%{pname}
