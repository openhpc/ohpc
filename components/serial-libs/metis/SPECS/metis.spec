#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Serial metis build dependent on compiler toolchain

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

#-ohpc-header-comp-begin-----------------------------

# OpenHPC convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency.
%if !0%{?opensuse_bs}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
BuildRequires: coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM} 
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?OHPC_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-ohpc-header-comp-end-------------------------------

# Base package name
%define pname metis
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary: Serial Graph Partitioning and Fill-reducing Matrix Ordering
Version: 5.1.0
Release: 1
License: Apache License 2.0
Group:   ohpc/serial-libs
URL:     http://glaros.dtc.umn.edu/gkhome/metis/metis/overview
Source0: http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/metis-%{version}.tar.gz
Source1: OHPC_macros
Source2: OHPC_setup_compiler
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: make
BuildRequires: pkgconfig
BuildRequires: cmake
Requires:      libmetis0 = %{version}
Provides:      libmetis = %{version}
Provides:      libmetis0 = %{version}
DocDir:        %{OHPC_PUB}/doc/contrib

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
METIS is a family of programs for partitioning unstructured graphs and hypergraph
and computing fill-reducing orderings of sparse matrices. The underlying algorithms
used by METIS are based on the state-of-the-art multilevel paradigm that has been
shown to produce high quality results and scale to very large problems.

%package -n libmetis0
Summary:        Serial Graph Partitioning and Fill-reducing Matrix Ordering
Group:          System/Libraries

%package devel
License:         Free for non-commercial use
Requires:        %name = %version
Requires:	 pkgconfig
Summary:         Metis development files
Group:           Development/Libraries/C and C++

%description -n libmetis0
METIS is a family of programs for partitioning unstructured graphs and hypergraph
and computing fill-reducing orderings of sparse matrices. The underlying algorithms
used by METIS are based on the state-of-the-art multilevel paradigm that has been
shown to produce high quality results and scale to very large problems.

%description devel
METIS is a family of programs for partitioning unstructured graphs and hypergraph
and computing fill-reducing orderings of sparse matrices. The underlying algorithms
used by METIS are based on the state-of-the-art multilevel paradigm that has been
shown to produce high quality results and scale to very large problems.

%prep
%setup -q -n %{pname}-%{version}
%build

# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

make config shared=1 prefix=%{install_path}
make

%install

# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

make install DESTDIR=${RPM_BUILD_ROOT}

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "metis"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%post -n libmetis0
/sbin/ldconfig

%postun -n libmetis0
/sbin/ldconfig

%clean
rm -fr %buildroot

%files
%defattr(-,root,root)
%{OHPC_HOME}

%{OHPC_PUB}
%doc BUILD.txt Changelog Install.txt LICENSE.txt
