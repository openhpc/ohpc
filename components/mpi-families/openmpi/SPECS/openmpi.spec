%define compiler_family gnu

#-fsp-header-comp-begin-----------------------------

# Compiler dependencies
BuildRequires: lmod
%if %{compiler_family} == gnu
BuildRequires: FSP-gnu-compilers
Requires: FSP-gnu-compilers
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ FSP-intel-compilers
Requires:      gcc-c++ FSP-intel-compilers
%if 0%{FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end-------------------------------

# Base package name
%define pname openmpi
%define with_openib 1
%define with_psm 0
%define with_lustre 1
%define with_slurm 1

Summary:   A powerful implementation of MPI
Name:      FSP-%{pname}-%{compiler_family}
Version:   1.8.3
Release:   1
License:   BSD-3-Clause
Group:     Development/Libraries/Parallel
URL:       http://www.open-mpi.org
Source0:   %{pname}-%{version}.tar.bz2
Source1:   FSP_macros
Source2:   FSP_setup_compiler
Patch1:    %{pname}-no_date_and_time.patch
Patch2:    %{pname}-no_network_in_build.patch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%include %{_sourcedir}/FSP_macros

%define debug_package %{nil}

BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  libtool
BuildRequires:  postfix
BuildRequires:  opensm
BuildRequires:  opensm-devel
BuildRequires:  numactl
%if 0%{with_slurm}
BuildRequires:  slurm-devel
#!BuildIgnore:  slurm
%endif

%if 0%{?suse_version}
BuildRequires:  libnuma1
BuildRequires:  sysfsutils
%else
BuildRequires:  libsysfs-devel
BuildRequires:  numactl-devel
%endif

%if %{with_lustre}
BuildRequires:  lustre-client
%endif

%if %{with_openib}
BuildRequires:  libibumad-devel
BuildRequires:  libibverbs-devel
%endif

%if %{with_psm}
BuildRequires:  infinipath-psm infinipath-psm-devel
%endif

# Default library install path
%define install_path %{FSP_MPI_STACKS}/%{name}/%version

%description 

Open MPI is a project combining technologies and resources from several
other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to
build the best MPI library available.

This RPM contains all the tools necessary to compile, link, and run
Open MPI jobs.

%prep

%setup -q -n %{pname}-%{version}

%build

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

BASEFLAGS="--prefix=%{install_path} --disable-static --enable-builtin-atomics"
%if %{with_psm}
  BASEFLAGS="$BASEFLAGS --with-psm"
%endif
%if %{with_openib}
  BASEFLAGS="$BASEFLAGS --with-verbs"
%endif
%if %{with_lustre}
  BASEFLAGS="$BASEFLAGS --with-io-romio-flags=--with-file-system=testfs+ufs+nfs+lustre"
%endif

./configure ${BASEFLAGS}

%install

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove .la files detected by rpm

rm $RPM_BUILD_ROOT/%{install_path}/lib/*.la


# FSP module file
%{__mkdir} -p %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
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
prepend-path    MODULEPATH          %{FSP_MODULEDEPS}/%{compiler_family}-%{pname}
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

family "MPI"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/ldconfig || exit 1

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{FSP_HOME}


%changelog
* Tue Aug  5 2014  <karl.w.schulz@intel.com> - 
- Initial build.

