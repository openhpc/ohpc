#---------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

%global pname pdsh

Summary:   Parallel remote shell program
Name:      %{pname}%{PROJ_DELIM}
Version:   2.33
Release:   1%{?dist}
License:   GPL
Url:       https://github.com/chaos/pdsh
Group:     %{PROJ_NAME}/admin
Source0:   https://github.com/chaos/%{pname}/releases/download/%{pname}-%{version}/%{pname}-%{version}.tar.gz
Patch1:    pdsh-slurm-list.patch

### karl.w.schulz@intel.com (11/07/14) - temporarily disabling rcmd requirement
### Requires: pdsh-rcmd

# Default library install path
%global install_path %{OHPC_HOME}/admin/%{pname}

#
# Enabling and disabling pdsh options
#  defaults:
#  enabled:  readline, rsh, ssh, dshgroup, netgroups, exec
#  disabled: rms, mrsh, qshell, mqshell, xcpu, genders, nodeattr, machines,
#            nodeupdown

#  To build the various module subpackages, pass --with <pkg> on
#   the rpmbuild command line (if your rpm is a recent enough version)
#  
#  Similarly, to disable various pdsh options pass --without <pkg> on
#   the rpmbuild command line.
#
#  This specfile also supports passing the --with and --without through
#   the environment variables PDSH_WITH_OPTIONS and PDSH_WITHOUT_OPTIONS.
#   e.g. PDSH_WITH_OPTIONS="qshell genders" rpmbuild ....
#

#
#  Definition of default packages to build on various platforms:
# 
%global _defaults ssh exec readline pam

#   LLNL system defaults
#%if 0%{?chaos}
#%global _default_with %{_defaults} mrsh nodeupdown genders slurm
#%else
#   All other defaults
%global _default_with %{_defaults} mrsh genders slurm
#%endif

#
#   Environment variables can be used to override defaults above:
#
#%global _env_without ${PDSH_WITHOUT_OPTIONS}
#%global _env_with    ${PDSH_WITH_OPTIONS}

#   Shortcut for % global expansion
%define dstr "%%%%"global

#    Check with/out env variables for any options
%define env() echo %_env_%{1}|grep -qw %%1 && echo %dstr _%{1}_%%1 --%{1}-%%1
#    Check defaults
%define def() echo %_default_with | grep -qw %%1 || w=out; echo %dstr _with${w}_%%1 --with${w}-%%1

#    Check env variables first. If they are not set use defaults.
%{expand: %%define pdsh_with() %%((%{env with})||(%{env without})||(%{def}))}

#    Only check environment and defaults if a --with or --without wasn't
#     used from the rpmbuild command line.
#
%define pdsh_opt() %%{!?_with_%1: %%{!?_without_%1: %%{expand: %%pdsh_with %1}}}


#
# Rcmd modules:
#
%{expand: %pdsh_opt exec}
%{expand: %pdsh_opt ssh}
%{expand: %pdsh_opt rsh}
%{expand: %pdsh_opt mrsh}
%{expand: %pdsh_opt qshell}
%{expand: %pdsh_opt mqshell}
%{expand: %pdsh_opt xcpu}

#
# Misc modules:
#
%{expand: %pdsh_opt netgroup}
%{expand: %pdsh_opt dshgroups}
%{expand: %pdsh_opt genders}
%{expand: %pdsh_opt nodeattr}
%{expand: %pdsh_opt nodeupdown}
%{expand: %pdsh_opt machines}
%{expand: %pdsh_opt slurm}
%{expand: %pdsh_opt torque}
%{expand: %pdsh_opt rms}

#
# Other options:
#
%{expand: %pdsh_opt readline}
%{expand: %pdsh_opt debug}
%{expand: %pdsh_opt pam}

#
# If "--with debug" is set compile with --enable-debug
#   and try not to strip binaries.
#
# (See /usr/share/doc/rpm-*/conditionalbuilds)
#
%if %{?_with_debug:1}%{!?_with_debug:0}
  %global _enable_debug --enable-debug
%endif


#%{?_with_mrsh:BuildRequires: munge-devel%{PROJ_DELIM}}
#BuildRequires: munge-devel%{PROJ_DELIM}
%{?_with_qshell:BuildRequires: qsnetlibs}
%{?_with_mqshell:BuildRequires: qsnetlibs}
%{?_with_readline:BuildRequires: readline-devel}
%{?_with_readline:BuildRequires: ncurses-devel}
%{?_with_nodeupdown:BuildRequires: whatsup}
#%{?_with_genders:BuildRequires: genders > 1.0}
#BuildRequires: genders > 1.0
%{?_with_pam:BuildRequires: pam-devel}
%{?_with_slurm:BuildRequires: slurm-devel%{PROJ_DELIM}}
%{?_with_torque:BuildRequires: torque-devel}


BuildRequires: ncurses-devel
BuildRequires: readline-devel
BuildRequires: pam-devel


##############################################################################
# Pdsh description

%description
Pdsh is a multithreaded remote shell client which executes commands
on multiple remote hosts in parallel.  Pdsh can use several different
remote shell services, including standard "rsh", Kerberos IV, and ssh.
##############################################################################

%package qshd
Summary: Remote shell daemon for pdsh/qshell/Quadrics QsNet
Group:   System Environment/Base
Requires:  xinetd
%description qshd
Remote shell service for running Quadrics QsNet jobs under pdsh.
Sets up Elan capabilities and environment variables needed by Quadrics
MPICH executables.
##############################################################################

%package mqshd
Summary: Remote shell daemon for pdsh/mqshell/Quadrics QsNet
Group:   System Environment/Base
Requires:  xinetd
%description mqshd
Remote shell service for running Quadrics QsNet jobs under pdsh with
mrsh authentication.  Sets up Elan capabilities and environment variables
needed by Quadrics MPICH executables.
##############################################################################

#
# Module packages:
#
%package   rcmd-rsh
Summary:   Provides bsd rcmd capability to pdsh
Group:     System Environment/Base
Provides:  pdsh-rcmd
%description rcmd-rsh
Pdsh module for bsd rcmd functionality. Note: This module
requires that the pdsh binary be installed setuid root.

%package   rcmd-ssh
Summary:   Provides ssh rcmd capability to pdsh
Group:     System Environment/Base
Provides:  pdsh-rcmd
%description rcmd-ssh
Pdsh module for ssh rcmd functionality.

%package   rcmd-qshell
Summary:   Provides qshell rcmd capability to pdsh
Group:     System Environment/Base
Provides:  pdsh-rcmd
Conflicts: pdsh-rcmd-mqshell
%description rcmd-qshell
Pdsh module for running QsNet MPI jobs. Note: This module
requires that the pdsh binary be installed setuid root.

%package   rcmd-mrsh
Summary:   Provides mrsh rcmd capability to pdsh
Group:     System Environment/Base
Provides:  pdsh-rcmd
%description rcmd-mrsh
Pdsh module for mrsh rcmd functionality.

%package   rcmd-mqshell
Summary:   Provides mqshell rcmd capability to pdsh
Group:     System Environment/Base
Provides:  pdsh-rcmd
Conflicts: pdsh-rcmd-qshell
%description rcmd-mqshell
Pdsh module for mqshell rcmd functionality.

%package   rcmd-xcpu
Summary:   Provides xcpu rcmd capability to pdsh
Group:     System Environment/Base
Provides:  pdsh-xcpu
%description rcmd-xcpu
Pdsh module for xcpu rcmd functionality.

%package   rcmd-exec
Summary:   Provides arbitrary command execution "rcmd" method to pdsh
Group:     System Environment/Base
Provides:  pdsh-rcmd
%description rcmd-exec
Pdsh module for generic exec capability. This module allows
execution of an arbitrary command line for each target host in
place of a more specific rcmd connect method (i.e. ssh, rsh, etc.).
The command executed for each host is built from the pdsh
"remote" command line: The first remote argument is the command
to execute, followed by any arguments including "%h", "%u", and
"%n", which are the remote target, username, and rank respectively.

%package   -n pdsh-mod-genders%{PROJ_DELIM}
Summary:   Provides libgenders support for pdsh
Group:     System Environment/Base
Requires:  genders%{PROJ_DELIM} >= 1.1
BuildRequires: genders%{PROJ_DELIM}
Conflicts: pdsh-mod-nodeattr
Conflicts: pdsh-mod-machines
%description -n pdsh-mod-genders%{PROJ_DELIM}
Pdsh module for libgenders functionality.

%package   mod-nodeattr
Summary:   Provides genders support for pdsh using the nodeattr program
Group:     System Environment/Base
Requires:  genders
Conflicts: pdsh-mod-genders
Conflicts: pdsh-mod-machines
%description mod-nodeattr
Pdsh module for genders functionality using the nodeattr program.

%package   mod-nodeupdown
Summary:   Provides libnodeupdown support for pdsh
Group:     System Environment/Base
Requires:  whatsup
%description mod-nodeupdown
Pdsh module providing -v functionality using libnodeupdown.

%package   mod-rms
Summary:   Provides RMS support for pdsh
Group:     System Environment/Base
Requires:  qsrmslibs
%description mod-rms
Pdsh module providing support for gathering the list of target nodes
from an allocated RMS resource.

%package   mod-machines
Summary:   Pdsh module for gathering list of target nodes from a machines file
Group:     System Environment/Base
%description mod-machines
Pdsh module for gathering list of all target nodes from a machines file.

%package   mod-dshgroup
Summary:   Provides dsh-style group file support for pdsh
Group:     System Environment/Base
%description mod-dshgroup
Pdsh module providing dsh (Dancer's shell) style "group" file support.
Provides -g groupname and -X groupname options to pdsh.

%package   mod-netgroup
Summary:   Provides netgroup support for pdsh
Group:     System Environment/Base
%description mod-netgroup
Pdsh module providing support for targeting hosts based on netgroup.
Provides -g groupname and -X groupname options to pdsh.

%package   -n pdsh-mod-slurm%{PROJ_DELIM}
Summary:   Provides support for running pdsh under SLURM allocations
Group:     System Environment/Base
Requires:  slurm%{PROJ_DELIM}
BuildRequires: slurm-devel%{PROJ_DELIM}
%description -n pdsh-mod-slurm%{PROJ_DELIM}
Pdsh module providing support for gathering the list of target nodes
from an allocated SLURM job.

%package   mod-torque
Summary:   Provides support for running pdsh under Torque allocations
Group:     System Environment/Base
Requires:  torque
%description mod-torque
Pdsh module providing support for gathering the list of target nodes
from an allocated Torque job.



##############################################################################

%prep
%setup  -q -n %{pname}-%{version}
%patch1 -p1
%dump
##############################################################################

%build

# work around old config.guess on aarch64 systems
%ifarch aarch64 || ppc64le
cp /usr/lib/rpm/config.guess config
%endif

./configure --prefix=%{install_path} \
    --libdir=%{install_path}/lib \
    --with-rcmd-rank-list="ssh mrsh rsh krb4 exec xcpu" \
    %{?_enable_debug}       \
    %{?_with_rsh}           \
    %{?_without_rsh}        \
    %{?_with_ssh}           \
    %{?_without_ssh}        \
    %{?_with_exec}          \
    %{?_without_exec}       \
    %{?_with_readline}      \
    %{?_without_readline}   \
    %{?_with_machines}      \
    %{?_without_machines}   \
    %{?_with_genders}       \
    %{?_without_genders}    \
    %{?_with_nodeupdown}    \
    %{?_without_nodeupdown} \
    %{?_with_mrsh}          \
    %{?_without_mrsh}       \
    %{?_with_xcpu}       \
    %{?_without_xcpu}    \
    %{?_with_slurm}         \
    %{?_without_slurm}      \
    %{?_with_torque}        \
    %{?_without_torque}     \
    %{?_with_dshgroups}     \
    %{?_without_dshgroups}  \
    %{?_with_netgroup}      \
    %{?_without_netgroup}


# FIXME: build fails when trying to build with _smp_mflags if qsnet is enabled
make %{_smp_mflags} CFLAGS="$RPM_OPT_FLAGS"

##############################################################################

%install
%{__mkdir_p} $RPM_BUILD_ROOT
DESTDIR="$RPM_BUILD_ROOT" make install
if [ -x $RPM_BUILD_ROOT/%{_sbindir}/in.qshd ]; then
   install -D -m644 etc/qshell.xinetd $RPM_BUILD_ROOT/%{_sysconfdir}/xinetd.d/qshell
fi
if [ -x $RPM_BUILD_ROOT/%{_sbindir}/in.mqshd ]; then
   install -D -m644 etc/mqshell.xinetd $RPM_BUILD_ROOT/%{_sysconfdir}/xinetd.d/mqshell
fi

%if 0%{?OHPC_BUILD}
# install_doc_files
%endif

#
# Remove all module .a's as they are not needed on any known RPM platform.
find "%buildroot" -type f -name "*.a" | xargs rm -f

# Add soft link to pdsh binaries in default path

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_bindir}
ln -sf %{install_path}/bin/pdsh ${RPM_BUILD_ROOT}/%{_bindir}
ln -sf %{install_path}/bin/dshbak ${RPM_BUILD_ROOT}/%{_bindir}
ln -sf %{install_path}/bin/pdcp ${RPM_BUILD_ROOT}/%{_bindir}
ln -sf %{install_path}/bin/rpdcp ${RPM_BUILD_ROOT}/%{_bindir}

find ${RPM_BUILD_ROOT}

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%doc COPYING README NEWS DISCLAIMER.LLNS DISCLAIMER.UC
%doc README.KRB4 README.modules
%{OHPC_HOME}
%{OHPC_PUB}
%{_bindir}/pdsh
%{_bindir}/dshbak
%{_bindir}/pdcp
%{_bindir}/rpdcp
%exclude %{install_path}/lib/pdsh/genders.*
%exclude %{install_path}/lib/pdsh/slurm.*

%if 0%{?OHPC_BUILD}
# dir %{OHPC_PUB}/share/doc
# {OHPC_PUB}/share/doc/%{pname}
%doc AUTHORS

%endif

%if %{?_with_genders:1}%{!?_with_genders:0}
%files -n pdsh-mod-genders%{PROJ_DELIM}
%{install_path}/lib/pdsh/genders.*
%endif

%if %{?_with_slurm:1}%{!?_with_slurm:0}
%files -n pdsh-mod-slurm%{PROJ_DELIM}
%{install_path}/lib/pdsh/slurm.*
%endif
