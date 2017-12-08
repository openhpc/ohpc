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
Name:           python-Cython-ohpc
Version:        0.26.1
Release:        0
Url:            http://www.cython.org
Summary:        The Cython compiler for writing C extensions for the Python language
License:        Apache-2.0
Group:          Development/Languages/Python
Source:         https://files.pythonhosted.org/packages/source/C/Cython/Cython-%{version}.tar.gz
Source1:        python-Cython-rpmlintrc
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  %{python_module xml}
BuildRequires:  fdupes
Requires(post): update-alternatives
Requires(postun): update-alternatives
%else
BuildRequires:  %{python_module devel}
BuildRequires:  libxml2-python
Requires:       libxml2-python
#BuildRequires:  python-devel
#BuildRequires:  python34-devel
Requires(post): chkconfig
Requires(postun): chkconfig
%endif
BuildRequires:  gcc-c++
BuildRequires:  python-rpm-macros%{PROJ_DELIM}
Requires:       python-devel
Requires:       python-xml

%python_subpackages

%description
The Cython language allows for writing C extensions for the Python
language. Cython is a source code translator based on Pyrex, but
supports more cutting edge functionality and optimizations.

The Cython language is very close to the Python language (and most Python
code is also valid Cython code), but Cython additionally supports calling C
functions and declaring C types on variables and class attributes. This
allows the compiler to generate very efficient C code from Cython code.

%prep
%setup -q -n Cython-%{version}
# Fix non-executable scripts
sed -i "s|^#!.*||" Cython/Debugger/{libpython,Cygdb}.py cython.py

%build
export CFLAGS="%{optflags}"
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
%if 0%{?sles_version} || 0%{?suse_version}
%fdupes %{buildroot}%{$python_sitearch}
%endif
}

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

