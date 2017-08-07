#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package scalapack
#
# Copyright (c) 2014 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

# Build that is is dependent on compiler toolchain and MPI
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname scalapack
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:        A subset of LAPACK routines redesigned for heterogenous computing
License:        netlib ScaLAPACK License
Group:          Development/Libraries/Parallel
Version:        2.0.2
Release:        1%{?dist}
# This is freely distributable without any restrictions.
Url:            http://www.netlib.org/lapack-dev/
Source0:        http://www.netlib.org/scalapack/scalapack-%{version}.tgz
Source1:        baselibs.conf
Source2:        OHPC_macros
Patch0:         scalapack-2.0.2-shared-lib.patch
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

%description
The ScaLAPACK (or Scalable LAPACK) library includes a subset
of LAPACK routines redesigned for distributed memory MIMD
parallel computers. It is currently written in a
Single-Program-Multiple-Data style using explicit message
passing for interprocessor communication. It assumes
matrices are laid out in a two-dimensional block cyclic
decomposition.

ScaLAPACK is designed for heterogeneous computing and is
portable on any computer that supports MPI or PVM.

Like LAPACK, the ScaLAPACK routines are based on
block-partitioned algorithms in order to minimize the frequency
of data movement between different levels of the memory hierarchy.
(For such machines, the memory hierarchy includes the off-processor
memory of other processors, in addition to the hierarchy of registers,
cache, and local memory on each processor.) The fundamental building
blocks of the ScaLAPACK library are distributed memory versions (PBLAS)
of the Level 1, 2 and 3 BLAS, and a set of Basic Linear Algebra
Communication Subprograms (BLACS) for communication tasks that arise
frequently in parallel linear algebra computations. In the ScaLAPACK
routines, all interprocessor communication occurs within the PBLAS and the
BLACS. One of the design goals of ScaLAPACK was to have the ScaLAPACK
routines resemble their LAPACK equivalents as much as possible.

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%prep
%setup -q -n %{pname}-%{version}
%patch0 -p2
cp SLmake.inc.example SLmake.inc

%build
%ohpc_setup_compiler
%if "%{compiler_family}" != "intel"
module load openblas
%endif

make lib

%install
%{__mkdir} -p %{buildroot}/%{_docdir}
%{__mkdir} -p ${RPM_BUILD_ROOT}%{install_path}/etc
%{__mkdir} -p ${RPM_BUILD_ROOT}%{install_path}/lib
install -m 644 SLmake.inc ${RPM_BUILD_ROOT}%{install_path}/etc
install -m 755 *so* ${RPM_BUILD_ROOT}%{install_path}/lib

pushd ${RPM_BUILD_ROOT}%{install_path}/lib
ln -fs libscalapack.so.2.0.2 libscalapack.so.2
ln -s libscalapack.so.2.0.2 libscalapack.so
popd

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the ScaLAPACK library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

if { ![is-loaded intel]  } {
    depends-on openblas
}

prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc README LICENSE

%changelog
* Tue May 23 2017 Adrian Reber <areber@redhat.com> - 2.0.2-13.2
- Remove separate mpi setup; it is part of the %%ohpc_compiler macro

* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 2.0.2-13.1
- switch to use of ohpc_compiler_dependent and ohpc_mpi_dependent flags

* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 2.0.2-13.1
- Switching to %%ohpc_compiler macro
