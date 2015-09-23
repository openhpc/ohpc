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

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/OHPC_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
# hack to install MKL for the moment
BuildRequires: intel-compilers-devel%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end-------------------------------

# Base package name
%define pname numpy
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           python-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        1.9.2
Release:        1
Url:            http://sourceforge.net/projects/numpy
DocDir:         %{FSP_PUB}/doc/contrib
Summary:        NumPy array processing for numbers, strings, records and objects
License:        BSD-3-Clause
Group:          fsp/dev-tools
Source0:        %{pname}-%{version}.tar.gz
Source1:        OHPC_macros
Source2:        FSP_setup_compiler
Patch1:         numpy-buildfix.patch
Patch2:         numpy-intelc.patch
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
%define install_path %{FSP_LIBS}/%{compiler_family}/%{pname}/%version

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
%patch2 -p1

export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

# Enable MKL linkage for blas/lapack with gnu builds
%if %{compiler_family} == gnu
module load mkl
%endif

cat > site.cfg << EOF
[mkl]
library_dirs = $MKLROOT/lib/intel64
include_dirs = $MKLROOT/include
mkl_libs = mkl_rt
lapack_libs =
EOF

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

# Enable MKL linkage for blas/lapack with gnu builds
%if %{compiler_family} == gnu
module load mkl
%endif


%if %{compiler_family} == intel
COMPILER_FLAG="--compiler=intelem"

%endif
#CFLAGS="%{optflags} -fno-strict-aliasing" python setup.py build $COMPILER_FLAG
python setup.py build $COMPILER_FLAG


%install
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

python setup.py install --root="%{buildroot}" --prefix="%{install_path}"
%if 0%{?suse_version}
%fdupes -s %{buildroot}%{install_path}
%endif

# FSP module file
%{!?compiler_family: %define compiler_family gnu}
%{__mkdir_p} %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
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

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%defattr(-,root,root)
%{FSP_HOME}
%{FSP_PUB}
%doc DEV_README.txt
%doc THANKS.txt
%doc LICENSE.txt
%doc README.txt

%changelog
