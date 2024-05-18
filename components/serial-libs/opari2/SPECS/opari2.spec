#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler toolchains
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname opari2

Summary:        An OpenMP runtime performance measurement instrumenter
Name:           %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        2.0.8
Release:        1%{?dist}
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/serial-libs
URL:            https://www.vi-hps.org/projects/score-p/
Source0:        http://perftools.pages.jsc.fz-juelich.de/cicd/opari2/tags/%{pname}-%{version}/%{pname}-%{version}.tar.gz
BuildRequires:  make
BuildRequires:  gcc-c++
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version
	
%description
OPARI2 is a source-to-source instrumentation tool for OpenMP and hybrid
codes.  It surrounds OpenMP directives and runtime library calls with calls
to the POMP2 measurement interface.

OPARI2 will provide you with a new initialization method that allows for
multi-directory and parallel builds as well as the usage of pre-instrumented
libraries. Furthermore, an efficient way of tracking parent-child
relationships was added. Additionally, we extended OPARI2 to support
instrumentation of OpenMP 3.0 tied tasks.

This is the %{compiler_family} version.

%prep

%setup -q -n %{pname}-%{version}

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "intel"
CONFIGURE_OPTIONS="--with-nocross-compiler-suite=oneapi "
%endif

%if "%{compiler_family}" == "arm1"
export CFLAGS="${CFLAGS} -fsimdmath"
export CXXFLAGS="${CXXFLAGS} -fsimdmath"
%endif

./configure --prefix=%{install_path} \
            --disable-static \
            --enable-shared \
            --disable-silent-rules \
            --enable-backend-test-runs \
            --with-platform=linux \
            CC="$CC" \
            CXX="$CXX" \
            CFLAGS="$CFLAGS" \
            CXXFLAGS="$CXXFLAGS" \
            LDFLAGS="$LDFLAGS" \
            ${CONFIGURE_OPTIONS}

make %{?_smp_mflags} V=1

%check

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

make check

%install

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

make DESTDIR=$RPM_BUILD_ROOT install

# don't package static libs
rm -rf $RPM_BUILD_ROOT%{install_path}/lib/*.a \
       $RPM_BUILD_ROOT%{install_path}/lib/*.la

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc ChangeLog COPYING INSTALL OPEN_ISSUES README
