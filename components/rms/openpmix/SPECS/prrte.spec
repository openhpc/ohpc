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
%global pname prrte

Summary: Reference RunTime Environment for PMIx
Name: %{pname}%{PROJ_DELIM}
Version: 3.0.1rc2
Release: 1%{?dist}
License: BSD
URL: https://openpmix.github.io/openpmix/
Group: %{PROJ_NAME}/rms
Source0: https://github.com/openpmix/prrte/releases/download/v%{version}/%{pname}-%{version}.tar.gz

Conflicts: libev

BuildRequires: libevent-devel
BuildRequires: gcc
BuildRequires: gcc-c++
BuildRequires: zlib-devel
BuildRequires: hwloc%{PROJ_DELIM}
BuildRequires: munge-devel
BuildRequires: automake
BuildRequires: autoconf
BuildRequires: slurm%{PROJ_DELIM}
BuildRequires: openpbs-server%{PROJ_DELIM}
BuildRequires: openpmix%{PROJ_DELIM} > 4.2.0
BuildRequires: flex
Provides: openpmix-runtime%{PROJ_DELIM} = %{version}

#!BuildIgnore: post-build-checks

%global install_path %{OHPC_ADMIN}/%{pname}
%global module_path %{OHPC_MODULES}/%{pname}

%Description
PRRTE is the PMIx Reference Run Time Environment.

The project is formally referred to in documentation by "PRRTE", and
the GitHub repository is "prrte".

However, we have found that most users do not like typing the two
consecutive "r"s in the name. Hence, all of the internal API symbols,
environment variables, MCA frameworks, and CLI executables all use the
abbreviated "prte" (one "r", not two) for convenience.

This RPM contains all the tools necessary to compile, link, and run
the PRRTE system.


%prep
%setup -q -n %{pname}-%{version}


%build
module load hwloc
module load openpmix

export CFLAGS="%{?cflags:%{cflags}}%{!?cflags:$RPM_OPT_FLAGS}" 
export CXXFLAGS="%{?cxxflags:%{cxxflags}}%{!?cxxflags:$RPM_OPT_FLAGS}"
export FCFLAGS="%{?fcflags:%{fcflags}}%{!?fcflags:$RPM_OPT_FLAGS}"

./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --with-hwloc=$HWLOC_DIR \
            --enable-shared \
            --disable-static \
            --with-slurm \
            --with-tm=/opt/pbs \
            --with-pmix=$OPENPMIX_DIR \
            #|| { cat config.log && exit 1; }
make %{?_smp_mflags}


%install
make install DESTDIR=${RPM_BUILD_ROOT}

# PRRTE Module File
mkdir -p ${RPM_BUILD_ROOT}%{module_path}
cat <<EOF > ${RPM_BUILD_ROOT}%{module_path}/%{version}.lua
help([[
This module loads the %{pname} library.
]])

whatis("Name: %{pname}")
whatis("Version: %{version}")

local version = "%{version}"

prepend_path("MANPATH",      "%{install_path}/share/man")
prepend_path("INCLUDE",      "%{install_path}/include")
prepend_path("LIBRARY_PATH", "%{install_path}/lib")

setenv("%{PNAME}_DIR", "%{install_path}")
setenv("%{PNAME}_LIB", "%{install_path}/lib")
setenv("%{PNAME}_INC", "%{install_path}/include")

family("PMIx")
EOF


%files
%{install_path}
%{module_path}
%doc README.md
%doc VERSION
%license LICENSE