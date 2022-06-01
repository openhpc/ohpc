#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%global ohpc_bootstrap 1

%include %{_sourcedir}/OHPC_macros

%define pname lmod

Summary:   Lua based Modules (lmod)
Name:      %{pname}%{PROJ_DELIM}
Version:   8.7.3
Release:   1%{?dist}
License:   MIT
Group:     %{PROJ_NAME}/admin
Url:       https://github.com/TACC/Lmod
Source0:   https://github.com/TACC/Lmod/archive/%{version}.tar.gz#$/%{pname}-%{version}.tar.gz

BuildRequires: lua > 5.1
Requires: lua > 5.1
BuildRequires: tcl tcl-devel
Requires: tcl
BuildRequires: lua-devel
BuildRequires: rsync

%if 0%{?suse_version} || 0%{?sle_version}
BuildRequires: lua-luafilesystem
BuildRequires: lua-luaposix
BuildRequires: procps
Requires: lua-luafilesystem
Requires: lua-luaposix
Conflicts: Modules
%else
# If not SUSE, assume Fedora-based OS
BuildRequires: lua-libs
BuildRequires: lua-filesystem
BuildRequires: lua-posix
BuildRequires: procps-ng
Requires: lua-filesystem
Requires: lua-posix
Conflicts: Lmod
%if 0%{?rhel} > 7
# Starting with RHEL8, packages in RHEL8 depending on
# environment modules no longer depend on the package
# but on the virtual provide 'environment(modules)'.
# By extending the MODULEPATH of this lmod we can easily
# also use the OS provided modulesfiles.
Provides: environment(modules)
Obsoletes: environment-modules
Conflicts: Lmod
%else
Conflicts: environment-modules
%endif
%endif

Patch1: lmod.site.patch

%description
Lmod is program to manage the user environment. It is a new implementation
of environment modules, providing a convenient way to dynamically change 
users’ environments. 

Lmod is a Lua-based module system that easily manages hierarchical
toolchains. All popular shells are supported.


%prep
%setup -q -n Lmod-%{version}

# OpenHPC patches
%patch1 -p1

# Remove statements about submitting a support ticket from help files
sed -i "/ticket/d" messageDir/*.lua


%build
unset MODULEPATH
./configure --prefix=%{OHPC_ADMIN} --with-redirect=yes --with-autoSwap=no


%install
make DESTDIR=$RPM_BUILD_ROOT install

# Profile scipts
mkdir -p %{buildroot}/%{_sysconfdir}/profile.d
cat << EOF > %{buildroot}/%{_sysconfdir}/profile.d/lmod.sh
#!/bin/bash
# -*- shell-script -*-
########################################################################
#  This is the system wide source file for setting up modules
#########################################################################

# NOOP if running under known resource manager
if [ ! -z "\$SLURM_NODELIST" ] | [ ! -z "\$PBS_NODEFILE" ]; then
     return
fi

export LMOD_SETTARG_CMD=":"
export LMOD_FULL_SETTARG_SUPPORT=no
export LMOD_COLORIZE=no
export LMOD_PREPEND_BLOCK=normal

if [ \$EUID -eq 0 ]; then
    export MODULEPATH=%{OHPC_ADMIN}/modulefiles:%{OHPC_MODULES}
else
    export MODULEPATH=%{OHPC_MODULES}
fi

export BASH_ENV=%{OHPC_ADMIN}/lmod/lmod/init/bash

# Initialize modules system
. %{OHPC_ADMIN}/lmod/lmod/init/bash >/dev/null

# Load baseline OpenHPC environment
module try-add ohpc

EOF

%{__cat} << EOF > %{buildroot}/%{_sysconfdir}/profile.d/lmod.csh
#!/bin/csh
# -*- shell-script -*-
########################################################################
#  This is the system wide source file for setting up modules
########################################################################

# NOOP if running under known resource manager
if ( ! "\$?SLURM_NODELIST" == "" || ! "\$?PBS_NODEFILE" == "" ) then
    exit 0
endif

setenv LMOD_SETTARG_CMD ":"
setenv LMOD_FULL_SETTARG_SUPPORT "no"
setenv LMOD_COLORIZE "no"
setenv LMOD_PREPEND_BLOCK "normal"

if ( \`id -u\` == "0" ) then
   setenv MODULEPATH "%{OHPC_ADMIN}/modulefiles:%{OHPC_MODULES}"
else
   setenv MODULEPATH "%{OHPC_MODULES}"
endif

# Initialize modules system
source %{OHPC_ADMIN}/lmod/lmod/init/csh >/dev/null

# Load baseline OpenHPC environment
module try-add ohpc

EOF

# Starting with RHEL 8, load OS provided modules
mkdir -p %{buildroot}/%{OHPC_MODULES}
cat << EOF > %{buildroot}/%{OHPC_MODULES}/os.lua
help([[
Enable operating system provided modules
]])

whatis("Name: Operating System provided modules")

append_path("MODULEPATH", "/etc/modulefiles:/usr/share/modulefiles")

EOF

# Add symlink to modulecmd to allow use of scl-utils
mkdir -p %{buildroot}/%{_bindir}
ln -s %{OHPC_ADMIN}/lmod/lmod/libexec/lmod %{buildroot}/%{_bindir}/modulecmd


%files
%dir %{OHPC_HOME}
%dir %{OHPC_ADMIN}
%{OHPC_PUB}
%{OHPC_ADMIN}/lmod
%{_sysconfdir}/profile.d/lmod.sh
%{_sysconfdir}/profile.d/lmod.csh
%{OHPC_MODULES}/os.lua
%doc README.md README_lua_modulefiles.txt INSTALL
%license License
%{_bindir}/modulecmd
