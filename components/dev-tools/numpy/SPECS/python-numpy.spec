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
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname numpy
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%if 0%{?sles_version} || 0%{?suse_version}
%define python3_prefix python3
%else
%define python3_prefix python34
%endif

Name:           %{python3_prefix}-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        1.13.3
Release:        1%{?dist}
Url:            http://sourceforge.net/projects/numpy
Summary:        NumPy array processing for numbers, strings, records and objects
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Source0:        https://github.com/numpy/numpy/releases/download/v%{version}/numpy-%{version}.tar.gz
Source1:        OHPC_macros
Patch1:         numpy-buildfix.patch
Patch2:         numpy-intelccomp.patch
Patch3:         numpy-intelfcomp.patch
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
Requires:       %{python3_prefix}
%if 0%{?suse_version}
BuildRequires:  fdupes
BuildRequires:  python-rpm-macros
#!BuildIgnore: post-build-checks
%endif
BuildRequires:  %{python3_prefix}-devel
BuildRequires:  %{python3_prefix}-setuptools

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
%patch2 -p1
%patch3 -p1

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "intel"
COMPILER_FLAG="--compiler=intelem"
%else
module load openblas
%endif

%if "%{compiler_family}" == "intel"
cat > site.cfg << EOF
[mkl]
include_dirs = $MKLROOT/include
library_dirs = $MKLROOT/lib/intel64
mkl_libs = mkl_rt
lapack_libs = mkl_rt
EOF
%else
cat > site.cfg << EOF
[openblas]
libraries = openblas
library_dirs = $OPENBLAS_LIB
include_dirs = $OPENBLAS_INC
EOF
%endif

CFLAGS="%{optflags} -fno-strict-aliasing" %__python3 setup.py build $COMPILER_FLAG


%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%__python3 setup.py install --root="%{buildroot}" --prefix="%{install_path}"

%if 0%{?suse_version}
%fdupes -s %{buildroot}%{install_path}
%endif

# OpenHPC module file
%{!?compiler_family: %global compiler_family gnu}
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{python3_prefix}-%{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{python3_prefix}-%{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

# Require openblas for gnu compiler families
if { ![is-loaded intel] } {
    depends-on openblas
}

prepend-path    PATH                %{install_path}/bin
prepend-path    PYTHONPATH          %{install_path}/lib64/python3.4/site-packages

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
%{OHPC_PUB}
%doc INSTALL.rst.txt
%doc LICENSE.txt
%doc PKG-INFO
%doc THANKS.txt

%changelog
* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 1.12.1-1
- switch to ohpc_compiler_dependent flag

* Tue Feb 21 2017 Adrian Reber <areber@redhat.com> - 1.11.1-1
- Switching to %%ohpc_compiler macro
