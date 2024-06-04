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
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname otf2

Summary:        Open Trace Format 2 library
Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        3.0.3
Release:        1%{?dist}
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/io-libs
URL:            http://score-p.org
Source0:        http://perftools.pages.jsc.fz-juelich.de/cicd/otf2/tags/%{pname}-%{version}/%{pname}-%{version}.tar.gz
BuildRequires:  file
BuildRequires:  gcc-c++
BuildRequires:  make
BuildRequires:  sed
BuildRequires:  which
BuildRequires:  chrpath dos2unix
# Need a new py-compile for Python 3.12
BuildRequires:  libtool automake
BuildRequires:  sionlib-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
The Open Trace Format 2 (OTF2) is a highly scalable, memory efficient
event trace data format plus support library.

This is the %{compiler_family}-%{mpi_family} version.

%prep

%setup -q -n %{pname}-%{version}
dos2unix doc/examples/otf2_high_level_writer_example.py
rm build-config/py-compile
for d in . build-backend build-frontend	
do
  cd $d
  autoreconf -f -i -v
  cd -
done

%build

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load sionlib

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

module load sionlib

make check

%install

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load sionlib

make DESTDIR=$RPM_BUILD_ROOT install

# don't package static libs
rm -rf $RPM_BUILD_ROOT%{install_path}/lib/*.a \
       $RPM_BUILD_ROOT%{install_path}/lib/*.la

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

depends-on sionlib

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
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
