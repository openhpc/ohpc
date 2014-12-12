# Numpy python library build that is dependent on compiler toolchain

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu   }
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

%{!?compiler_family: %define compiler_family gnu   }
#-fsp-header-comp-end-------------------------------

# Base package name
%define pname numpy
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           python-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        1.9.1
Release:        1
Url:            http://sourceforge.net/projects/numpy
Summary:        NumPy array processing for numbers, strings, records and objects
License:        BSD-3-Clause
Group:          Development/Libraries/Python
Source0:         %{pname}-%{version}.tar.gz
Source1: FSP_macros
Source2: FSP_setup_compiler
Patch1:         numpy-buildfix.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRequires:  blas-devel
BuildRequires:  lapack-devel
BuildRequires:  python-devel
Requires:       python >= %{py_ver}
Provides:       numpy = %{version}
%if 0%{?suse_version}
BuildRequires:  fdupes
#!BuildIgnore: post-build-checks
# FIXME: atlas is horribly broken
# %if 0%{?suse_version} <= 1210
# BuildRequires:  libatlas3-devel
# %endif
%if 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib(1))")}
%else
%py_requires
%endif
%else
%if ! 0%{?fedora_version}
Provides:       python-numeric = %{version}
Obsoletes:      python-numeric < %{version}
%endif
%endif

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

%package devel
Summary:        Development files for %{pname} applications
Group:          Development/Libraries/Python
Requires:       %{name} = %{version}
Requires:       blas-devel
Requires:       lapack-devel
Requires:       python-devel
%if 0%{?suse_version}
Requires:       gcc-fortran
%py_requires -d
%else
Requires:       gcc-gfortran
%endif

%description devel
This package contains files for developing applications using %{pname}.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

CFLAGS="%{optflags} -fno-strict-aliasing" python setup.py build

%install
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

python setup.py install --root="%{buildroot}" --prefix="%{_prefix}"
rm -rf %{buildroot}%{python_sitearch}/%{pname}/{,core,distutils,f2py,fft,ma,matrixlib,oldnumeric,polynomial,random,testing}/tests # Don't package testsuite
%if 0%{?suse_version}
%fdupes -s %{buildroot}%{_prefix}
%endif

%files
%defattr(-,root,root)
%doc *.txt
%{_bindir}/f2py
%{python_sitearch}/%{pname}/
%{python_sitearch}/*.egg-info
%exclude %{python_sitearch}/%{pname}/*/*/*.c
%exclude %{python_sitearch}/%{pname}/*/*.h
%exclude %{python_sitearch}/%{pname}/*/*/*.h
%exclude %{python_sitearch}/%{pname}/*/*/*/*.h
%exclude %{python_sitearch}/%{pname}/core/lib/libnpymath.a

%files devel
%defattr(-,root,root)
%{python_sitearch}/%{pname}/*/*/*.c
%{python_sitearch}/%{pname}/*/*.h
%{python_sitearch}/%{pname}/*/*/*.h
%{python_sitearch}/%{pname}/*/*/*/*.h
%{python_sitearch}/%{pname}/core/lib/libnpymath.a

%changelog
