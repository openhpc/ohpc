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
%define pname environment-modules

Name:           %{pname}%{PROJ_DELIM}
Version:        5.7.0
Release:        1%{?dist}
Summary:        Provides dynamic modification of a user's environment

License:        GPL-2.0-or-later
Group:          %{PROJ_NAME}/admin
URL:            https://envmodules.io
Source0:        http://downloads.sourceforge.net/modules/modules-%{version}.tar.bz2

BuildRequires:  tcl
BuildRequires:  dejagnu
BuildRequires:  make
BuildRequires:  sed
BuildRequires:  less
%if (0%{?rhel} && 0%{?rhel} <= 8) || 0%{?openEuler} || 0%{?sle_version}
BuildRequires:  util-linux
%else
BuildRequires:  util-linux-core
%endif
BuildRequires:  hostname
BuildRequires:  %{procps}
# specific requirements to build extension library
BuildRequires:  gcc
BuildRequires:  tcl-devel
Requires:       tcl
Requires:       sed
Requires:       less
%if (0%{?rhel} && 0%{?rhel} <= 8) || 0%{?openEuler} || 0%{?sle_version}
Requires:       util-linux
%else
Requires:       util-linux-core
%endif
Requires:       %{procps}
%if 0%{?sle_version}
Requires:       man
%else
Requires:       man-db
%endif
Requires(post): coreutils
Requires(post): %{_sbindir}/update-alternatives
Requires(postun): %{_sbindir}/update-alternatives
Provides:       environment(modules)
Provides:       environment(modules)%{PROJ_DELIM}
Requires:       ohpc-filesystem
%if 0%{?sle_version}
Requires:       (%{pname}-apparmor-abstractions%{PROJ_DELIM} if apparmor-abstractions)
# Distribution package installs real files where alternatives links are set
Conflicts:      Modules
%endif

%description
The Environment Modules package provides for the dynamic modification of
a user's environment via modulefiles.

Each modulefile contains the information needed to configure the shell
for an application. Once the Modules package is initialized, the
environment can be modified on a per-module basis using the module
command which interprets modulefiles. Typically modulefiles instruct
the module command to alter or set shell environment variables such as
PATH, MANPATH, etc. modulefiles may be shared by many users on a system
and users may have their own collection to supplement or replace the
shared modulefiles.

Modules can be loaded and unloaded dynamically and atomically, in an
clean fashion. All popular shells are supported, including bash, ksh,
zsh, sh, csh, tcsh, as well as some scripting languages such as perl.

Modules are useful in managing different versions of applications.
Modules can also be bundled into meta-modules that will load an entire
suite of different applications.

NOTE: You will need to get a new shell after installing this package to
have access to the module alias.

%if 0%{?sle_version}
%package -n %{pname}-apparmor-abstractions%{PROJ_DELIM}
Summary:        AppArmor bash abstraction for Environment Modules
BuildRequires:  apparmor-abstractions
BuildRequires:  apparmor-rpm-macros
Requires:       apparmor-abstractions
BuildArch:      noarch

%description -n %{pname}-apparmor-abstractions%{PROJ_DELIM}
AppArmor bash abstraction allowing confined shells to initialize the
Environment Modules command from the system-wide profile scripts.
%endif


%prep
%setup -q -n modules-%{version}


%build
%configure --prefix=%{OHPC_ADMIN}/%{pname} \
           --bindir=%{OHPC_ADMIN}/%{pname}/bin \
           --libexecdir=%{OHPC_ADMIN}/%{pname}/libexec \
           --libdir=%{OHPC_ADMIN}/%{pname}/lib \
           --mandir=%{OHPC_ADMIN}/%{pname}/share/man \
           --etcdir=%{_sysconfdir}/%{name} \
           --disable-doc-install \
           --with-quarantine-vars='LD_LIBRARY_PATH LD_PRELOAD' \
           --with-init-envvars='MANPATH='

%make_build


%install
%make_install

# rpm checks the %%{OHPC_PUB} files entry before %%doc creates %%{_docdir}
%{__mkdir_p} %{buildroot}%{_docdir}

# setup for alternatives
%{__mkdir_p} %{buildroot}%{_sysconfdir}/profile.d
%{__mkdir_p} %{buildroot}%{_bindir}
touch %{buildroot}%{_sysconfdir}/profile.d/modules.{csh,sh}
touch %{buildroot}%{_bindir}/modulecmd

# remove modulecmd wrapper as it is directly handled as a link on modulecmd.tcl
rm -f %{buildroot}%{OHPC_ADMIN}/%{pname}/bin/modulecmd

mv {doc/build/,}NEWS.txt
mv {doc/build/,}MIGRATING.txt
mv {doc/build/,}CONTRIBUTING.txt
mv {doc/build/,}INSTALL.txt
mv {doc/build/,}changes.txt

# Customize startup configuration
%{__cat} << EOF > %{buildroot}/%{_sysconfdir}/%{name}/initrc
#%Module5.0
# ensure that module command is still defined in sub-shells
module config set_shell_startup 1

# enable environment variable quarantine mechanism
module config quarantine_support 1

# enable shell debugging properties silencing
module config silent_shell_debug 1

# abort when encountering an error during a multi-module operation
module config abort_on_error load:ml:reload:switch

# enable software hierarchy-related features
module config conflict_unload 1
module config require_via 1


module use %{OHPC_MODULES}
if {[module-info username root]} {
    module use %{OHPC_ADMIN}/modulefiles
}

# Load baseline OpenHPC environment
module try-add ohpc
EOF

%if 0%{?sle_version}
# Files read or executed when shell startup scripts initialize module command
install -d -m755 %{buildroot}%{_sysconfdir}/apparmor.d/abstractions/bash.d
%{__cat} << EOF > %{buildroot}%{_sysconfdir}/apparmor.d/abstractions/bash.d/%{pname}
  abi <abi/3.0>,

  %{OHPC_ADMIN}/%{pname}/init/* r,
  %{OHPC_ADMIN}/%{pname}/libexec/* r,
  %{OHPC_ADMIN}/%{pname}/lib/* mr,
  %{_bindir}/tclsh* ix,
  %{_sysconfdir}/%{name}/ r,
  %{_sysconfdir}/%{name}/* r,
  %{OHPC_MODULES}/ r,
  %{OHPC_MODULES}/** r,
  %{OHPC_ADMIN}/modulefiles/ r,
  %{OHPC_ADMIN}/modulefiles/** r,
EOF
%endif


%check
make test QUICKTEST=1


%post
# Cleanup from pre-alternatives
[ ! -L %{_sysconfdir}/profile.d/modules.sh ] &&  rm -f %{_sysconfdir}/profile.d/modules.sh
[ ! -L %{_sysconfdir}/profile.d/modules.csh ] &&  rm -f %{_sysconfdir}/profile.d/modules.csh
[ ! -L %{_bindir}/modulecmd ] &&  rm -f %{_bindir}/modulecmd

# Priority 60 takes precedence over distribution "module" packages and lmod-ohpc
%{_sbindir}/update-alternatives \
  --install %{_sysconfdir}/profile.d/modules.sh modules.sh %{OHPC_ADMIN}/%{pname}/init/profile.sh 60 \
  --slave %{_sysconfdir}/profile.d/modules.csh modules.csh %{OHPC_ADMIN}/%{pname}/init/profile.csh \
  --slave %{_bindir}/modulecmd modulecmd %{OHPC_ADMIN}/%{pname}/libexec/modulecmd.tcl

%postun
if [ $1 -eq 0 ] ; then
  %{_sbindir}/update-alternatives --remove modules.sh %{OHPC_ADMIN}/%{pname}/init/profile.sh
fi


%files
%dir %{OHPC_HOME}
%dir %{OHPC_ADMIN}
%{OHPC_ADMIN}/%{pname}
%license COPYING
%{OHPC_PUB}
%doc ChangeLog.gz README NEWS.txt MIGRATING.txt INSTALL.txt CONTRIBUTING.txt changes.txt
%ghost %{_sysconfdir}/profile.d/modules.csh
%ghost %{_sysconfdir}/profile.d/modules.sh
%ghost %{_bindir}/modulecmd
%dir %{_sysconfdir}/%{name}
%config(noreplace) %{_sysconfdir}/%{name}/initrc
%config(noreplace) %{_sysconfdir}/%{name}/siteconfig.tcl

%if 0%{?sle_version}
%files -n %{pname}-apparmor-abstractions%{PROJ_DELIM}
%dir %{_sysconfdir}/apparmor.d/abstractions/bash.d
%{_sysconfdir}/apparmor.d/abstractions/bash.d/%{pname}
%endif
