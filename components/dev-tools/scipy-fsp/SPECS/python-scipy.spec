#
# spec file for package python-scipy
#
# Copyright (c) 2013 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
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

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi%{PROJ_DELIM}
Requires:      intel-mpi%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif

#-fsp-header-comp-end-------------------------------

# Base package name
%define pname scipy
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name:           python-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        0.14.0
Release:        1
Summary:        Scientific Tools for Python
License:        BSD-3-Clause
Group:          Development/Libraries/Python
Url:            http://www.scipy.org
Source0:        %{pname}-%{version}.tar.gz
BuildRequires:  blas-devel
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
%endif
BuildRequires:  fftw-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  lapack-devel
BuildRequires:  python-devel
BuildRequires:  python-numpy-%{compiler_family}%{PROJ_DELIM}-devel
BuildRequires:  swig
#%if 0%{?suse_version} > 1140
#BuildRequires:  suitesparse-devel-static
#%endif
# FIXME: atlas is broken right now, do not use
# %if 0%{?suse_version} <= 1210
# BuildRequires:  libatlas3-devel
# %endif
%if 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print get_python_lib(1)")}
%endif
Requires:       python-numpy
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
Scipy is open-source software for mathematics, science, and
engineering. The core library is NumPy which provides convenient and
fast N-dimensional array manipulation. The SciPy library is built to
work with NumPy arrays, and provides many user-friendly and efficient
numerical routines such as routines for numerical integration and
optimization. Together, they run on all popular operating systems, are
quick to install, and are free of charge. NumPy and SciPy are easy to
use, but powerful enough to be depended upon by some of the world's
leading scientists and engineers.

%package weave
Summary:        Scientific Tools for Python - Weave
Group:          Development/Libraries/Python
Requires:       %{name} = %{version}
Provides:       python-scipy-devel = %{version}

%description weave
Scipy is open-source software for mathematics, science, and
engineering. The core library is NumPy which provides convenient and
fast N-dimensional array manipulation. The SciPy library is built to
work with NumPy arrays, and provides many user-friendly and efficient
numerical routines such as routines for numerical integration and
optimization. Together, they run on all popular operating systems, are
quick to install, and are free of charge. NumPy and SciPy are easy to
use, but powerful enough to be depended upon by some of the world's
leading scientists and engineers.

%prep
%setup -q -n %{pname}-%{version}
find . -type f -name "*.py" -exec sed -i "s|#!/usr/bin/env python||" {} \;

%if %{compiler_family} == intel
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

cat > site.cfg << EOF
[mkl]
library_dirs = $MKLROOT/lib/intel64
include_dirs = $MKLROOT/include
mkl_libs = mkl_rt
lapack_libs =
EOF
%endif


%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

CFLAGS="%{optflags} -fno-strict-aliasing" \
ATLAS=%{_libdir}/atlas \
FFTW=%{_libdir}
BLAS=%{_libdir} \
LAPACK=%{_libdir} \
%if %{compiler_family} == intel
LDSHARED="icc -shared" \
python setup.py config --compiler=intelm --fcompiler=intelem build_clib --compiler=intelem --fcompiler=intelem build_ext --compiler=intelem --fcompiler=intelem build
%else
python setup.py config_fc --fcompiler=gnu95 --noarch build
%endif

%install
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

python setup.py install --prefix=%{_prefix} --root=%{buildroot}
find %{buildroot}%{python_sitearch}/scipy -type d -name tests | xargs rm -rf # Don't ship tests
# Don't ship weave examples, they're marked as documentation:
find %{buildroot}%{python_sitearch}/scipy/weave -type d -name examples | xargs rm -rf
%if 0%{?sles_version} || 0%{?suse_version}
%fdupes %{buildroot}%{python_sitearch}
%endif
# fix executability issue
chmod +x %{buildroot}%{python_sitearch}/%{pname}/io/arff/arffread.py
chmod +x %{buildroot}%{python_sitearch}/%{pname}/special/spfun_stats.py

%files
%defattr(-,root,root,-)
%doc LICENSE.txt
%{python_sitearch}/%{pname}/
%{python_sitearch}/*egg-info
%exclude %{python_sitearch}/%{pname}/weave

%files weave
%defattr(-,root,root,-)
%doc scipy/weave/examples
%{python_sitearch}/%{pname}/weave

%changelog
