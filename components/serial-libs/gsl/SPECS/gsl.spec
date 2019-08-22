#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Serial GSL library build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname gsl

Summary:   GNU Scientific Library (GSL)
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   2.5
Release:   1%{?dist}
License:   GPL
Group:     %{PROJ_NAME}/serial-libs
URL:       http://www.gnu.org/software/gsl
Source0:   https://ftp.gnu.org/gnu/%{pname}/%{pname}-%{version}.tar.gz

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description

The GNU Scientific Library (GSL) is a numerical library for C and C++
programmers.  The library provides a wide range of mathematical
routines such as random number generators, special functions and
least-squares fitting.  It contains over 1000 mathematical routines
written in ANSI C.  The library follows modern coding conventions, and
lends itself to being used in very high level languages (VHLLs).

%prep
%setup -q -n %{pname}-%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if %{compiler_family} == intel
export CFLAGS="-fp-model strict $CFLAGS"
%endif

./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --disable-static \
            || { cat config.log && exit 1; }
make %{?_smp_mflags}

%install
# OpenHPC compiler designation
%ohpc_setup_compiler
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f

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

set             version		    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
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

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%{OHPC_PUB}
%doc AUTHORS BUGS ChangeLog COPYING INSTALL NEWS README THANKS TODO
