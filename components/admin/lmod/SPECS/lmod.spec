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
Version:   8.7.14
Release:   %{?dist}.1
License:   MIT
Group:     %{PROJ_NAME}/admin
Url:       https://github.com/TACC/Lmod
Source0:   https://github.com/TACC/Lmod/archive/%{version}.tar.gz#$/%{pname}-%{version}.tar.gz

BuildRequires: lua-devel
BuildRequires: rsync
BuildRequires: tcl-devel
BuildRequires: gcc make bc

%if 0%{?rhel} || 0%{?openEuler}
BuildRequires: lua-libs
BuildRequires: lua-filesystem
BuildRequires: lua-posix
BuildRequires: procps-ng
Requires: lua-filesystem
Requires: lua-posix
%endif
%if 0%{?suse_version}
BuildRequires: lua-luafilesystem
BuildRequires: lua-luaposix
BuildRequires: procps
Requires: lua-luafilesystem
Requires: lua-luaposix
%endif

%if 0%{?suse_version}
Conflicts: Modules
%else
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

# 8/28/14 karl.w.schulz@intel.com - include patches to remove consulting notice and setting of TACC env variables
Patch1: lmod.consulting.patch
Patch2: lmod.site.patch
# 4/25/17 karl.w.schulz@intel.com - upping patch fuzz factor for newer lmod
%global _default_patch_fuzz 2

%description
Lmod: An Environment Module System based on Lua, Reads TCL Modules,
Supports a Software Hierarchy

%prep
%setup -q -n Lmod-%{version}

# OpenHPC patches
%patch1 -p1
%patch2 -p1

%build
unset MODULEPATH
./configure --prefix=%{OHPC_ADMIN} --with-redirect=yes --with-autoSwap=no

%install
make DESTDIR=$RPM_BUILD_ROOT install
# Customize startup script to suit

%{__mkdir_p} %{buildroot}/%{_sysconfdir}/profile.d
%{__cat} << EOF > %{buildroot}/%{_sysconfdir}/profile.d/lmod.sh
#!/bin/sh
# -*- shell-script -*-
########################################################################
#  This is the system wide source file for setting up
#  modules:
#
########################################################################

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
#  This is the system wide source file for setting up
#  modules:
#
########################################################################

if ( \$?SLURM_NODELIST ) then
    exit 0
endif

if ( \$?PBS_NODEFILE ) then
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

# Starting with RHEL 8 we can load OS provided modules
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/os
#%Module1.0#####################################################################

proc ModulesHelp { } { puts stderr "Enable operating system provided modules" }

module-whatis "Name: Operating System provided modules"

append-path MODULEPATH /etc/modulefiles:/usr/share/modulefiles
EOF


%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

#install a modulecmd soft link
# to allow use of scl-utils, among other dependencies
%{__mkdir_p} %{buildroot}/%{_bindir}

%{__ln_s} %{OHPC_ADMIN}/lmod/lmod/libexec/lmod %{buildroot}/%{_bindir}/modulecmd

%files
%dir %{OHPC_HOME}
%dir %{OHPC_ADMIN}
%{OHPC_ADMIN}/lmod
%config %{_sysconfdir}/profile.d/lmod.sh
%config %{_sysconfdir}/profile.d/lmod.csh
%{OHPC_PUB}
%doc License README.md README_lua_modulefiles.txt INSTALL
%{_bindir}/modulecmd
