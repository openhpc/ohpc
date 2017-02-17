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

%define with_slurm 0

%include %{_sourcedir}/OHPC_macros

%ohpc_compiler

%if 0%{with_slurm}
BuildRequires: slurm-devel%{PROJ_DELIM} slurm%{PROJ_DELIM}
%endif

# Base package name
%define pname mpich

Summary:   MPICH MPI implementation
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   3.2
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/mpi-families
URL:       http://www.mpich.org
Source0:   http://www.mpich.org/static/downloads/%{version}/%{pname}-%{version}.tar.gz
Source1:   OHPC_macros

Requires: prun%{PROJ_DELIM}

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{name}/%version

%description

MPICH is a high performance and widely portable implementation of the
Message Passing Interface (MPI) standard.

%prep

%setup -q -n %{pname}-%{version}

%build
# OpenHPC compiler designation
%ohpc_setup_compiler

./configure --prefix=%{install_path} \
%if 0%{with_slurm}
            --with-pm=no --with-pmi=slurm \
%endif
    || { cat config.log && exit 1; }

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

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/man
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    MPI_DIR             %{install_path}
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

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%doc README.envvar
%doc COPYRIGHT
%doc CHANGES
%doc README
%doc RELEASE_NOTES


%changelog
* Fri Feb 17 2017 Adrian Reber <areber@redhat.com> - 3.2-1
- Switching to %%ohpc_compiler macro

* Thu Sep  1 2016  <raffenet@mcs.anl.gov>
- Initial build.
