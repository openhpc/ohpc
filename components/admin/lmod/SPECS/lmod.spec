%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define lualibdir %{_libdir}/lua/%{luaver}
%define luapkgdir %{_datadir}/lua/%{luaver}
%define LUA_CPATH ?.so;?/?.so;%{lualibdir}/?.so
%define LUA_PATH ?.lua;?/?.lua;%{luapkgdir}/?.lua

Summary:  Lua based Modules (lmod)
Name: lmod
Version: 5.8
Release: 1
License: MIT
Group: System Environment/Base
Url: https://github.com/TACC/Lmod
Source0: Lmod-%{version}.tar.bz2
Source1: fsp-paths.inc
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: lua >= %{luaver}
BuildRequires: lua-devel >= %{luaver}
%if 0%{?suse_version} <= 1220
BuildRequires: lua-bit
%endif
BuildRequires: lua-filesystem
BuildRequires: lua-posix
BuildRequires: rsync

# ks: disabling AutoReq to deal with /usr/bin/lua not being owned by an rpm in SLES11
%if 0%{?suse_verion} <= 1220
AutoReq: 0
%endif

# 8/28/14 karl.w.schulz@intel.com - include patches to remove consulting notice and setting of TACC env variables
Patch1: lmod.consulting.patch
Patch2: lmod.site.patch

# FSP defaults

%include %{SOURCE1}

# Known dependencies
Requires: lua >= %{luaver}
Requires: lua-filesystem
Requires: lua-posix
Requires: tcl

%define debug_package %{nil}

%description 
Lmod: An Environment Module System based on Lua, Reads TCL Modules,
Supports a Software Hierarchy

%prep
%setup -q -n Lmod-%{version}

# Intel FSP patches
%patch1 -p1
%patch2 -p1

%build
unset MODULEPATH
export LUA_CPATH="%{LUA_CPATH}"
export LUA_PATH="%{LUA_PATH}"
./configure --prefix=%{FSP_ADM_PATH} --libdir=%{lualibdir} --datadir=%{luapkgdir} --with-redirect=yes --with-autoSwap=no

%install
export LUA_CPATH="%{LUA_CPATH}"
export LUA_PATH="%{LUA_PATH}"
make DESTDIR=$RPM_BUILD_ROOT install
# Customize startup script to suit

mkdir -p %{buildroot}/%{_sysconfdir}/profile.d
%{__cat} << EOF > %{buildroot}/%{_sysconfdir}/profile.d/lmod.sh
#!/bin/sh
# -*- shell-script -*-
########################################################################
#  This is the system wide source file for setting up
#  modules:
#
########################################################################

if [ \$EUID -ne 0 ]; then

    export LMOD_SETTARG_CMD=":"
    export LMOD_FULL_SETTARG_SUPPORT=no
    export LMOD_COLORIZE=no
    export LMOD_PREPEND_BLOCK=normal
    export MODULEPATH=%{FSP_MODULE_PATH}

EOF

# Deal with SLES default lua not having /usr/lib64 in search path
%if 0%{?suse_version} <= 1220
%{__cat} << EOF >> %{buildroot}/%{_sysconfdir}/profile.d/lmod.sh
    if [ -n \$LUA_CPATH ];then
         export LUA_CPATH="%{LUA_CPATH}"
    fi
    if [ -n \$LUA_PATH ] ;then
         export LUA_PATH="%{LUA_PATH}"
    fi
EOF
%endif

%{__cat} << EOF >> %{buildroot}/%{_sysconfdir}/profile.d/lmod.sh

    export BASH_ENV=%{FSP_ADM_PATH}/lmod/lmod/init/bash

    . %{FSP_ADM_PATH}/lmod/lmod/init/bash >/dev/null # Module Support

    # Load baseline fsp environment

    if [ -z "$SLURM_NODELIST" ];then
       module try-add fsp
    fi

fi
EOF

%{__cat} << EOF > %{buildroot}/%{_sysconfdir}/profile.d/lmod.csh
#!/bin/sh
# -*- shell-script -*-
########################################################################
#  This is the system wide source file for setting up
#  modules:
#
########################################################################

if ( \`id -u\` != "0" ) then

    setenv LMOD_SETTARG_CMD ":"
    setenv LMOD_FULL_SETTARG_SUPPORT "no"
    setenv LMOD_COLORIZE "no"
    setenv LMOD_PREPEND_BLOCK "normal"
    setenv MODULEPATH "%{FSP_MODULE_PATH}"

EOF
# Deal with SLES default lua not having /usr/lib64 in search path
%if 0%{?suse_version} <= 1220
%{__cat} << EOF >> %{buildroot}/%{_sysconfdir}/profile.d/lmod.csh
    if ( ! \$?LUA_CPATH ) then
         setenv LUA_CPATH "%{LUA_CPATH}"
    endif
    if ( ! \$?LUA_PATH ) then
         setenv LUA_PATH "%{LUA_PATH}"
    endif
EOF
%endif

%{__cat} << EOF >> %{buildroot}/%{_sysconfdir}/profile.d/lmod.csh
###    setenv BASH_ENV %{FSP_ADM_PATH}/lmod/lmod/init/bash

    source %{FSP_ADM_PATH}/lmod/lmod/init/csh >/dev/null # Module Support

    # Load baseline fsp environment

    if ( ! \$?SLURM_NODELIST ) then
       module try-add fsp	    
    endif

endif
EOF

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%dir %{FSP_PATH}
%dir %{FSP_ADM_PATH}
%{FSP_ADM_PATH}/lmod
%config %{_sysconfdir}/profile.d/lmod.sh
%config %{_sysconfdir}/profile.d/lmod.csh
