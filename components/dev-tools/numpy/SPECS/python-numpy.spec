#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Numpy python library build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%define ohpc_python_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname numpy

Name:           %{python_prefix}-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        1.19.5
Release:        1%{?dist}
Url:            https://github.com/numpy/numpy
Summary:        NumPy array processing for numbers, strings, records and objects
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Source0:        https://github.com/numpy/numpy/releases/download/v%{version}/numpy-%{version}.tar.gz
Patch1:         numpy-buildfix.patch
Patch2:         numpy-intelccomp.patch
Patch3:         numpy-intelfcomp.patch
Patch4:         numpy-llvm-arm.patch
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  python3-Cython%{PROJ_DELIM}
%if 0%{?suse_version}
BuildRequires:  fdupes
#!BuildIgnore: post-build-checks
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
NumPy is a general-purpose array-processing package designed to
efficiently manipulate large multi-dimensional arrays of arbitrary
records without sacrificing too much speed for small multi-dimensional
arrays.  NumPy is built on the Numeric code base and adds features
introduced by numarray as well as an extended C-API and the ability to
create arrays of arbitrary type which also makes NumPy suitable for
interfacing with general-purpose data-base applications.

There are also basic facilities for discrete fourier transform,
basic linear algebra and random number generation.

%prep
%setup -q -n %{pname}-%{version}

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "intel"
COMPILER_FLAG="--compiler=intelem"
%endif

%if "%{compiler_family}" == "llvm"
COMPILER_FLAG="--fcompiler=flang --compiler=clang"
%endif

%if "%{compiler_family}" == "arm1"
COMPILER_FLAG="--fcompiler=armflang --compiler=armclang"
%endif

%if "%{compiler_family}" == "arm1"
cat > site.cfg << EOF
[openblas]
libraries = armpl
library_dirs = $ARMPL_LIBRARIES
include_dirs = $ARMPL_INCLUDES
EOF
%endif

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
cat > site.cfg << EOF
[openblas]
libraries = openblas
library_dirs = $OPENBLAS_LIB
include_dirs = $OPENBLAS_INC
EOF
%endif

CFLAGS="%{optflags} -fno-strict-aliasing" %__python setup.py build $COMPILER_FLAG


%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%__python setup.py install --root="%{buildroot}" --prefix="%{install_path}"

%if 0%{?suse_version}
%fdupes -s %{buildroot}%{install_path}
%endif

# OpenHPC module file
%{!?compiler_family: %global compiler_family gnu}
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{python_module_prefix}%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{python_module_prefix}%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with %{python_prefix}"
puts stderr "and the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{python_prefix}-%{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

family                      numpy
set     version             %{version}

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
# Require openblas for gnu and llvm compiler families
depends-on openblas
%endif

prepend-path    PATH                %{install_path}/bin
prepend-path    PYTHONPATH          %{install_path}/lib64/%{python_lib_dir}/site-packages

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{python_module_prefix}%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%exclude %{install_path}/bin/f2py
%{OHPC_PUB}
%doc INSTALL.rst.txt
%doc LICENSE.txt
%doc PKG-INFO
%doc THANKS.txt
