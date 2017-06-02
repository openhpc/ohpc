#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

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

# scipy build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

%{!?mpi_family: %global mpi_family openmpi}

%if "%{compiler_family}" != "intel"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires: openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi%{PROJ_DELIM}
Requires:      intel-mpi%{PROJ_DELIM}
%endif
%if %{mpi_family} == mpich
BuildRequires: mpich-%{compiler_family}%{PROJ_DELIM}
Requires:      mpich-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif


# Base package name
%define pname scipy
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           python-%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        0.19.0
Release:        1%{?dist}
Summary:        Scientific Tools for Python
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Url:            http://www.scipy.org
Source0:        https://github.com/scipy/scipy/archive/v%{version}.tar.gz#$/%{pname}-%{version}.tar.gz
BuildRequires:  blas-devel
Source1:        OHPC_macros
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
%endif
BuildRequires:  fftw-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires:  lapack-devel
BuildRequires:  python-devel
BuildRequires:  python-setuptools
BuildRequires:  python-Cython
BuildRequires:  python-numpy-%{compiler_family}%{PROJ_DELIM}
BuildRequires:  swig
Requires:       python-numpy-%{compiler_family}%{PROJ_DELIM}

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

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

%prep
%setup -q -n %{pname}-%{version}
find . -type f -name "*.py" -exec sed -i "s|#!/usr/bin/env python||" {} \;

%if %{compiler_family} == intel
cat > site.cfg << EOF
[mkl]
library_dirs = $MKLROOT/lib/intel64
include_dirs = $MKLROOT/include
mkl_libs = mkl_rt
lapack_libs =
EOF
%else
cat > site.cfg << EOF
[openblas]
libraries = openblas
library_dirs = $OPENBLAS_LIB
include_dirs = $OPENBLAS_INC
EOF
%endif

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" != "intel"
module load openblas
%endif

module load numpy

CFLAGS="%{optflags} -fno-strict-aliasing" \
ATLAS=%{_libdir}/atlas \
FFTW=%{_libdir}
BLAS=%{_libdir} \
LAPACK=%{_libdir} \
%if "%{compiler_family}" == "intel"
LDSHARED="icc -shared" \
python setup.py config --compiler=intelm --fcompiler=intelem build_clib --compiler=intelem --fcompiler=intelem build_ext --compiler=intelem --fcompiler=intelem build
%else
%if "%{compiler_family}" == "llvm"
LDSHARED=clang \
LDFLAGS="-shared -rtlib=compiler-rt -lm" \
python setup.py config_fc --fcompiler=flang --noarch build
%else
python setup.py config_fc --fcompiler=gnu95 --noarch build
%endif
%endif

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" != "intel"
module load openblas
%endif

module load numpy
python setup.py install --prefix=%{install_path} --root=%{buildroot}
find %{buildroot}%{install_path}/lib64/python2.7/site-packages/scipy -type d -name tests | xargs rm -rf # Don't ship tests
# Don't ship weave examples, they're marked as documentation:
find %{buildroot}%{install_path}/lib64/python2.7/site-packages/scipy/weave -type d -name examples | xargs rm -rf
%if 0%{?sles_version} || 0%{?suse_version}
%fdupes %{buildroot}%{install_path}/lib64/python2.7/site-packages
%endif

# fix executability issue
chmod +x %{buildroot}%{install_path}/lib64/python2.7/site-packages/%{pname}/io/arff/arffread.py
chmod +x %{buildroot}%{install_path}/lib64/python2.7/site-packages/%{pname}/special/spfun_stats.py

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI library."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family}"
module-whatis "Version: %{version}"
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PYTHONPATH          %{install_path}/lib64/python2.7/site-packages

setenv          %{PNAME}_DIR        %{install_path}

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded fftw]  } {
        module load fftw
    }
    if {  ![is-loaded numpy]  } {
        module load numpy
    }
    if {  ![is-loaded openblas]  } {
        module load openblas
    }
}

if [ module-info mode remove ] {
    module unload numpy
}

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc THANKS.txt
%doc LICENSE.txt

%changelog
* Tue May 23 2017 Adrian Reber <areber@redhat.com> - 0.19.0-2
- Remove separate mpi setup; it is part of the %%ohpc_compiler macro

* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 0.19.0-1
- switch to ohpc_compiler_dependent flag

* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 0.19.0-1
- Switching to %%ohpc_compiler macro
