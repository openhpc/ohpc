#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# MPICH MPI stack that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros
%{!?RMS_DELIM: %global RMS_DELIM %{nil}}

%define with_slurm 0
%{!?with_slurm: %define with_slurm 0}
%if 0%{with_slurm}
BuildRequires: slurm-devel%{PROJ_DELIM} slurm%{PROJ_DELIM}
%endif

%{!?with_pmix: %define with_pmix 0}
%if 0%{with_pmix}
BuildRequires:  pmix%{PROJ_DELIM}
BuildRequires: libevent-devel
%endif

# Base package name
%define pname mpich

Summary:   MPICH MPI implementation
Name:      %{pname}%{RMS_DELIM}-%{compiler_family}%{PROJ_DELIM}
Version:   3.3.1
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/mpi-families
URL:       http://www.mpich.org
Source0:   http://www.mpich.org/static/downloads/%{version}/%{pname}-%{version}.tar.gz
Patch0:    config.pmix.patch
Patch1:    node.name.fix.patch
# 08/14/19 karl@ices.utexas.edu - upping patch fuzz factor for node.name patch
%global _default_patch_fuzz 2

Requires: prun%{PROJ_DELIM} >= 1.2
Requires: perl
BuildRequires: zlib-devel
%if 0%{?suse_version}
BuildRequires:  libnuma-devel
%else
BuildRequires: numactl-devel
%endif

%if "%{RMS_DELIM}" != "%{nil}"
Provides: %{pname}-%{compiler_family}%{PROJ_DELIM}
%endif

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{name}/%version

%description

MPICH is a high performance and widely portable implementation of the
Message Passing Interface (MPI) standard.

%prep

%setup -q -n %{pname}-%{version}
%patch0 -p0
%patch1 -p1

%build
# OpenHPC compiler designation
%ohpc_setup_compiler
%if 0%{with_pmix}
module load pmix
export CPATH=${PMIX_INC}
%endif

./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
%if 0%{with_slurm}
            --with-pm=no --with-pmi=slurm \
%endif
%if 0%{with_pmix}
            LIBS="-L%{OHPC_ADMIN}/pmix/pmix/lib -lpmix" --with-pm=none --with-pmi=slurm \
%endif
    || { cat config.log && exit 1; }

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
%{__sed} -i -e 's#wl=""#wl="-Wl,"#g' libtool
%{__sed} -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' libtool
%endif

#            --with-device=ch4:ofi,ucx \

make %{?_smp_mflags}

%install
# OpenHPC compiler designation
%ohpc_setup_compiler
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove .la files detected by rpm
rm $RPM_BUILD_ROOT/%{install_path}/lib/*.la


# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL: %{url}"

set     version			    %{version}

setenv          MPI_DIR             %{install_path}
%if 0%{with_pmix}
setenv          OHPC_MPI_LAUNCHERS  pmix
%endif
prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

family "MPI"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_HOME}
%doc README.envvar
%doc COPYRIGHT
%doc CHANGES
%doc README
%doc RELEASE_NOTES
