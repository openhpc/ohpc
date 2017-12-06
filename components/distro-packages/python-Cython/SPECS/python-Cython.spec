#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

%{?!python_module:%define python_module() python-%{**} python3-%{**}}
%define pname Cython
Name:           python-%{pname}%{PROJ_DELIM}
Version:        0.26.1
Release:        76.1%{?dist}
Url:            http://www.cython.org
Summary:        The Cython compiler for writing C extensions for the Python language
License:        Apache-2.0
Group:          Development/Languages/Python
Source:         https://files.pythonhosted.org/packages/source/C/Cython/Cython-%{version}.tar.gz
Source1:        python-Cython-rpmlintrc
Source2:        OHPC_macros
Patch1:         python-Cython-c++11.patch
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
BuildRequires:  %{python_module xml}
Requires:       python-xml
Requires(post): update-alternatives
Requires(postun): update-alternatives
%else
BuildRequires:  libxml2-python
Requires:       libxml2-python
%endif
BuildRequires:  gcc-c++
BuildRequires:  %{python_module devel}
Requires:       python-devel
BuildRequires:  python-rpm-macros
Provides:       python-cython = %{version}
Obsoletes:      python-cython < %{version}
%if 0%{?suse_version} && 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print get_python_lib(1)")}
%endif

%python_subpackages

%description
The Cython language makes writing C extensions for the Python language as
easy as Python itself.  Cython is a source code translator based on the
well-known Pyrex, but supports more cutting edge functionality and
optimizations.

The Cython language is very close to the Python language (and most Python
code is also valid Cython code), but Cython additionally supports calling C
functions and declaring C types on variables and class attributes. This
allows the compiler to generate very efficient C code from Cython code.

This makes Cython the ideal language for writing glue code for external C
libraries, and for fast C modules that speed up the execution of Python
code.

%prep
%setup -q -n %{pname}-%{version}
%patch1
# Fix non-executable scripts
sed -i "s|^#!.*||" Cython/Debugger/{libpython,Cygdb}.py cython.py

%build
CFLAGS="%{optflags}" python setup.py build
%python_build

%install
%{python_expand %$python_install
# Prepare for update-alternatives usage
mkdir -p %{buildroot}%{_sysconfdir}/alternatives
for p in cython cythonize cygdb ; do
    mv %{buildroot}%{_bindir}/$p %{buildroot}%{_bindir}/$p-%{$python_bin_suffix}
done
}
for p in cython cythonize cygdb ; do
    %prepare_alternative $p
done

%{python_expand chmod a+x %{buildroot}%{$python_sitearch}/Cython/Build/Cythonize.py
sed -i "s|^#!/usr/bin/env python$|#!%{__$python}|" %{buildroot}%{$python_sitearch}/Cython/Build/Cythonize.py
$python -m compileall -d %{$python_sitearch} %{buildroot}%{$python_sitearch}/Cython/Build/
$python -O -m compileall -d %{$python_sitearch} %{buildroot}%{$python_sitearch}/Cython/Build/
%fdupes %{buildroot}%{$python_sitearch}
}

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s %{buildroot}%{python_sitearch} %{buildroot}%{_docdir}
%endif
rm -rf %{buildroot}%{python_sitearch}/__pycache__/

%post
%python_install_alternative cython cythonize cygdb

%postun
%python_uninstall_alternative cython

%files %{python_files}
%defattr(-,root,root,-)
%if 0%{?leap_version} >= 420200 || 0%{?suse_version} > 1320
%license LICENSE.txt
%else
%doc LICENSE.txt
%endif
%doc COPYING.txt README.txt ToDo.txt USAGE.txt
%python_alternative %{_bindir}/cygdb
%python_alternative %{_bindir}/cython
%python_alternative %{_bindir}/cythonize
%{python_sitearch}/Cython/
%{python_sitearch}/Cython-%{version}-py*.egg-info
%{python_sitearch}/cython.py*
%pycache_only %{python_sitearch}/__pycache__/cython*.py*
%{python_sitearch}/pyximport/

%changelog
* Tue Oct 10 2017 Adrian Reber <areber@redhat.com> - 0.23.4-76.1
- Added OHPC_macros for correct PROJ_DELIM
- Removed reference to Suse and http://bugs.opensuse.org/ as it is
  an OpenHPC spec file now
- Removed old changelog entries
