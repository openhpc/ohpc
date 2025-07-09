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
Version:   8.7.64
Release:   %{?dist}.1
License:   MIT
Group:     %{PROJ_NAME}/admin
Url:       https://github.com/TACC/Lmod
Source0:   https://github.com/TACC/Lmod/archive/%{version}/%{pname}-%{version}.tar.gz

BuildRequires: rsync
BuildRequires: tcl-devel
BuildRequires: gcc make bc
BuildRequires: %{procps}

%if 0%{?rhel} || 0%{?openEuler}
BuildRequires: lua-devel
BuildRequires: lua-libs
BuildRequires: lua-filesystem
BuildRequires: lua-posix
Requires: lua-filesystem
Requires: lua-posix
Requires: lua
Provides: environment(modules)
Obsoletes: environment-modules
%endif
%if 0%{?sle_version}
BuildRequires: lua53-luafilesystem
BuildRequires: lua53-luaposix
BuildRequires: lua53-devel
Requires: lua53-luafilesystem
Requires: lua53-luaposix
Requires: (lmod-apparmor-abstractions%{PROJ_DELIM} if apparmor-abstractions)
Conflicts: Modules
%endif

# 8/28/14 karl.w.schulz@intel.com - include patches to remove consulting notice and setting of TACC env variables
Patch1: lmod.consulting.patch
Patch2: lmod.site.patch
# 4/25/17 karl.w.schulz@intel.com - upping patch fuzz factor for newer lmod
%global _default_patch_fuzz 2

%description
Lmod: An Environment Module System based on Lua, Reads TCL Modules,
Supports a Software Hierarchy

%if 0%{?sle_version}
%package -n %{pname}-apparmor-abstractions%{PROJ_DELIM}
Summary:        Apparmor bash Abstraction for Lmod
BuildRequires:  apparmor-abstractions
BuildRequires:  apparmor-rpm-macros
Requires:       apparmor-abstractions
BuildArch:      noarch

%description -n %{pname}-apparmor-abstractions%{PROJ_DELIM}
Profile for shell source scripts for lua-lmod
%endif

%prep
%setup -q -n Lmod-%{version}

# OpenHPC patches
%patch -P 1 -p1
%patch -P 2 -p1

%build
unset MODULEPATH
./configure --prefix=%{OHPC_ADMIN} --with-redirect=yes --with-autoSwap=no

%install
make DESTDIR=$RPM_BUILD_ROOT install
# Customize startup script to suit

%{__mkdir_p} %{buildroot}/%{_sysconfdir}/profile.d
%{__cat} << 'EOF' > %{buildroot}/%{_sysconfdir}/profile.d/lmod.sh
#!/bin/sh
# -*- shell-script -*-
########################################################################
#  This is the system wide source file for setting up
#  modules:
#
########################################################################

# NOOP if running under known resource manager
if [ ! -z "$SLURM_NODELIST" ] || [ ! -z "$PBS_NODEFILE" ]; then
     return
fi

export LMOD_SETTARG_CMD=":"
export LMOD_FULL_SETTARG_SUPPORT=no
export LMOD_COLORIZE=no
export LMOD_PREPEND_BLOCK=normal

if [ $EUID -eq 0 ]; then
    export MODULEPATH=%{OHPC_ADMIN}/modulefiles:%{OHPC_MODULES}
else
    export MODULEPATH=%{OHPC_MODULES}
fi

# Add : to MANPATH to not drop default search paths, then safely append Lmod's
# man directory using addto helper
export MANPATH="${MANPATH}:"

export MANPATH=$(%{OHPC_ADMIN}/lmod/lmod/libexec/addto MANPATH %{OHPC_ADMIN}/lmod/lmod/share/man)

# Set BASH_ENV for environment
export BASH_ENV=%{OHPC_ADMIN}/lmod/lmod/init/bash

# Initialize modules system
. %{OHPC_ADMIN}/lmod/lmod/init/bash >/dev/null

# Load baseline OpenHPC environment
module try-add ohpc

EOF

%{__cat} << 'EOF' > %{buildroot}/%{_sysconfdir}/profile.d/lmod.csh
#!/bin/csh
# -*- shell-script -*-
########################################################################
#  This is the system wide source file for setting up
#  modules:
#
########################################################################

if ( $?SLURM_NODELIST ) then
    exit 0
endif

if ( $?PBS_NODEFILE ) then
    exit 0
endif

setenv LMOD_SETTARG_CMD ":"
setenv LMOD_FULL_SETTARG_SUPPORT "no"
setenv LMOD_COLORIZE "no"
setenv LMOD_PREPEND_BLOCK "normal"


if ( `id -u` == "0" ) then
   setenv MODULEPATH "%{OHPC_ADMIN}/modulefiles:%{OHPC_MODULES}"
else
   setenv MODULEPATH "%{OHPC_MODULES}"
endif

# Add : to MANPATH to not drop default search paths, then safely append Lmod's
# man directory using addto helper
if ( $?MANPATH ) then
    setenv MANPATH "${MANPATH}:"
else
    setenv MANPATH ":"
endif
setenv MANPATH `%{OHPC_ADMIN}/lmod/lmod/libexec/addto MANPATH %{OHPC_ADMIN}/lmod/lmod/share/man`

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

%if 0%{?sle_version}
append-path MODULEPATH /etc/modulefiles:/usr/share/modules
%endif
%if 0%{?rhel} || 0%{?openEuler}
append-path MODULEPATH /etc/modulefiles:/usr/share/modulefiles
%endif
EOF


%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

#install a modulecmd soft link
# to allow use of scl-utils, among other dependencies
%{__mkdir_p} %{buildroot}/%{_bindir}

%{__ln_s} %{OHPC_ADMIN}/lmod/lmod/libexec/lmod %{buildroot}/%{_bindir}/modulecmd

%if 0%{?sle_version}
install -d -m755 %{buildroot}%{_sysconfdir}/apparmor.d/abstractions/bash.d
cat <<EOF > %{buildroot}%{_sysconfdir}/apparmor.d/abstractions/bash.d/lmod
   abi <abi/3.0>,

   %_datadir/lmod/%{version}/init/*    r,
   %_datadir/lmod/%{version}/libexec/addto ix,
EOF
%endif

%files
%dir %{OHPC_HOME}
%dir %{OHPC_ADMIN}
%{OHPC_ADMIN}/lmod
%config %{_sysconfdir}/profile.d/lmod.sh
%config %{_sysconfdir}/profile.d/lmod.csh
%{OHPC_PUB}
%doc License README.md README_lua_modulefiles.txt INSTALL
%{_bindir}/modulecmd

%if 0%{?sle_version}
%files -n %{pname}-apparmor-abstractions%{PROJ_DELIM}
%dir %{_sysconfdir}/apparmor.d/abstractions/bash.d
%{_sysconfdir}/apparmor.d/abstractions/bash.d/lmod
%endif
