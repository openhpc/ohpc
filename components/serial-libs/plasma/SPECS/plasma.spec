#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# plasma - Parallel Linear Algebra Software for Multicore Architectures

%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname plasma

Name: %{pname}-%{compiler_family}%{PROJ_DELIM}
Version: 18.11.1
Release: 1%{?dist}
Summary: Parallel Linear Algebra Software for Multicore Architectures
License: BSD-3-Clause
Group: %{PROJ_NAME}/serial-libs
URL: https://bitbucket.org/icl/%{pname}
Source0: https://bitbucket.org/icl/plasma/downloads/plasma-%{version}.tar.gz
Source1: %{pname}-rpmlintrc
Requires: lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires: cmake%{PROJ_DELIM}


#!BuildIgnore: post-build-checks
# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
PLASMA is a software package for solving problems in dense linear algebra using
multicore processors and Xeon Phi coprocessors. PLASMA provides implementations
of state-of-the-art algorithms using cutting-edge task scheduling
techniques. PLASMA currently offers a collection of routines for solving linear
systems of equations, least squares problems, eigenvalue problems, and singular
value problems.

%prep
%setup -q -n %{pname}-%{version}

%build
# OpenHPC compiler designation
%ohpc_setup_compiler

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
module load openblas
%endif

module load cmake
export CFLAGS="$RPM_OPT_FLAGS"
cmake . \
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
	-DOpenBLAS_LIBRARIES=${OPENBLAS_LIB}/libopenblas.so \
%endif
	-DCMAKE_C_COMPILER=${CC} \
	-DCMAKE_INSTALL_PREFIX=%{install_path}

make VERBOSE=1 %{?_smp_mflags}

%install
make install DESTDIR=%{buildroot}

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
# Require openblas for gnu and llvm compiler families
depends-on openblas
%endif

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%check
%ohpc_setup_compiler
for i in `./plasmatest | tail -n +7` ; do ./plasmatest $i || exit 1; done

%files
%{OHPC_PUB}
%doc LICENSE README.md

