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
%define pname gotcha

Summary:        A library for wrapping function calls to shared libraries
Name:           %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        1.0.10
Release:        1%{?dist}
License:        LGPL-2.1-only
Group:          %{PROJ_NAME}/serial-libs
URL:            https://github.com/llnl/gotcha
Source0:        %{url}/archive/refs/tags/v%{version}.tar.gz
BuildRequires:  check-devel
BuildRequires:  cmake%{PROJ_DELIM}
BuildRequires:  make
BuildRequires:  gcc-c++
BuildRequires:  git
%if 0%{?suse_version}
BuildRequires:  python3-Sphinx
%else
BuildRequires:  python3dist(sphinx)
%endif
%if "%{compiler_family}" == "intel"
BuildRequires:  intel-oneapi-runtime-opencl
%endif
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
Gotcha is a library that wraps functions. Tools can use gotcha to install hooks
into other libraries, for example putting a wrapper function around libc's
malloc.  It is similar to LD_PRELOAD, but operates via a programmable API. This
enables easy methods of accomplishing tasks like code instrumentation or
wholesale replacement of mechanisms in programs without disrupting their source
code.

%prep
%autosetup -n GOTCHA-%{version}
# Use shared library for testing
sed -i 's/libcheck.a/libcheck.so/g' test/unit/CMakeLists.txt

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

module load cmake

mkdir gotcha-build

# CMAKE_CXX_FLAGS_DEBUG set to -O0 to prevent possible test failures
# due to -O3 set in OHPC_setup_compiler. Does not affect actual
# GOTCHA build.
# Strip ccache prefix from compiler variables; pass it via launcher flags
export CC=$(echo $CC | sed 's/^ccache //')
export CXX=$(echo $CXX | sed 's/^ccache //')

cmake \
    -DCMAKE_INSTALL_PREFIX=%{install_path} \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_POSITION_INDEPENDENT_CODE=TRUE \
    -DGOTCHA_ENABLE_TESTS=ON \
    -DCMAKE_C_COMPILER=${CC} \
    -DCMAKE_CXX_COMPILER=${CXX} \
%if "%{?OHPC_USE_CCACHE}" == "yes"
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
%endif
    -DCMAKE_CXX_FLAGS_DEBUG="-O0" \
    -DDEPENDENCIES_PREINSTALLED=TRUE \
    -S . \
    -B gotcha-build

cmake --build gotcha-build --parallel $(nproc) -- VERBOSE=1

# Build Documentation
pushd docs
sphinx-build . -b man man
popd

%check
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
module load cmake
%if "%{compiler_family}" != "intel"
ctest --output-on-failure --test-dir gotcha-build
%endif

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler
module load cmake

cmake --install gotcha-build --prefix %{buildroot}%{install_path}
# install documentation
mkdir -p %{buildroot}%{install_path}/share/man/man1
cp -p docs/man/gotcha.1 %{buildroot}%{install_path}/share/man/man1

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: serial libs"
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
%doc README.md COPYRIGHT LGPL
