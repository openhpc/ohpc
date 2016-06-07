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

#-ohpc-header-comp-begin----------------------------------------------

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

# OpenHPC convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency).
%if !0%{?OHPC_BUILD}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
BuildRequires: coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
BuildRequires: openblas-gnu%{PROJ_DELIM}
Requires:      openblas-gnu%{PROJ_DELIM}
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
%define pname numpy
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           python-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        1.10.2
Release:        1
Url:            http://sourceforge.net/projects/numpy
DocDir:         %{OHPC_PUB}/doc/contrib
Summary:        NumPy array processing for numbers, strings, records and objects
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Source0:        http://sourceforge.net/projects/numpy/files/NumPy/1.10.2/numpy-1.10.2.tar.gz
Source1:        OHPC_macros
Source2:        OHPC_setup_compiler
Patch1:         numpy-buildfix.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRequires:  python-devel
Requires:       python >= %{py_ver}
Provides:       numpy = %{version}
%if 0%{?suse_version}
BuildRequires:  fdupes
#!BuildIgnore: post-build-checks
%endif
%if ! 0%{?fedora_version}
Provides:       python-numeric = %{version}
Obsoletes:      python-numeric < %{version}
%endif

%define debug_package %{nil}

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
%patch1 -p1

export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

%if %{compiler_family} == intel
cat > site.cfg << EOF
[mkl]
library_dirs = $MKLROOT/lib/intel64
include_dirs = $MKLROOT/include
mkl_libs = mkl_rt
lapack_libs =
EOF
%endif

%if %{compiler_family} == gnu
module load openblas

cat > site.cfg << EOF
[openblas]
libraries = openblas
library_dirs = $OPENBLAS_LIB
include_dirs = $OPENBLAS_INC
EOF
%endif

%build
# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

%if %{compiler_family} == gnu
module load openblas
%endif

%if %{compiler_family} == intel
COMPILER_FLAG="--compiler=intelem"
%endif
#CFLAGS="%{optflags} -fno-strict-aliasing" python setup.py build $COMPILER_FLAG
python setup.py build $COMPILER_FLAG


%install
# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

python setup.py install --root="%{buildroot}" --prefix="%{install_path}"
%if 0%{?suse_version}
%fdupes -s %{buildroot}%{install_path}
%endif

# OpenHPC module file
%{!?compiler_family: %define compiler_family gnu}
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
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
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    PYTHONPATH          %{install_path}/lib64/python2.7/site-packages

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%defattr(-,root,root)
%{OHPC_HOME}
%{OHPC_PUB}
%doc DEV_README.txt
%doc THANKS.txt
%doc LICENSE.txt
%doc README.txt

%changelog
