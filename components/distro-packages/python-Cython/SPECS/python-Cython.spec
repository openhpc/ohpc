#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%define ohpc_python_dependent 1

%include %{_sourcedir}/OHPC_macros

Name:           %{python_prefix}-Cython%{PROJ_DELIM}
Version:        0.29.37
Release:        1%{?dist}
Url:            http://www.cython.org
Summary:        The Cython compiler for writing C extensions for the Python language
License:        Apache-2.0
Group:          %{PROJ_NAME}/distro-packages
Source0:        https://files.pythonhosted.org/packages/source/C/Cython/Cython-%{version}.tar.gz
Source1:        python-Cython-rpmlintrc
%if 0%{?rhel} || 0%{?openEuler}
Requires(post): chkconfig
Requires(postun): chkconfig
%else
BuildRequires: fdupes
Requires(post): update-alternatives
Requires(postun): update-alternatives
%endif
BuildRequires: gcc-c++
Requires: %{python_prefix}-devel

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
CFLAGS="%{optflags}" %{__python} setup.py build

%install
%{__python} setup.py install --prefix=%{_prefix} --root=%{buildroot}

# Prepare for update-alternatives usage
mkdir -p %{buildroot}%{_sysconfdir}/alternatives
for p in cython cythonize cygdb ; do
    mv %{buildroot}%{_bindir}/$p %{buildroot}%{_bindir}/$p-%{python3_version}
    ln -s -f %{_sysconfdir}/alternatives/$p %{buildroot}%{_bindir}/$p
    # create a dummy target for /etc/alternatives/$p
    touch %{buildroot}%{_sysconfdir}/alternatives/$p
done

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes %{buildroot}%{python3_sitearch} %{buildroot}%{_docdir}
%endif

%post
"%_sbindir/update-alternatives" \
   --install %{_bindir}/cython cython %{_bindir}/cython-%{python3_version} 30 \
   --slave %{_bindir}/cythonize cythonize %{_bindir}/cythonize-%{python3_version} \
   --slave %{_bindir}/cygdb cygdb %{_bindir}/cygdb-%{python3_version} \
   >/dev/null 2>&1 || :

%postun
if [ $1 -ge 1 ]; then
    if [ "$(readlink /etc/alternatives/cython)" == "%{_bindir}/cython-%{python3_version}" ]; then
        /usr/sbin/update-alternatives --set cython %{_bindir}/cython-%{python3_version} >/dev/null 2>&1 || :
    fi
fi

%files
%license LICENSE.txt
%doc COPYING.txt README.rst ToDo.txt USAGE.txt
%{_bindir}/cygdb
%{_bindir}/cython
%{_bindir}/cythonize
%{_bindir}/cygdb-%{python3_version}
%{_bindir}/cython-%{python3_version}
%{_bindir}/cythonize-%{python3_version}
%ghost %{_sysconfdir}/alternatives/cygdb
%ghost %{_sysconfdir}/alternatives/cython
%ghost %{_sysconfdir}/alternatives/cythonize
%{python3_sitearch}/Cython/
%{python3_sitearch}/Cython-%{version}-py*.egg-info
%{python3_sitearch}/cython.py*
%{python3_sitearch}/__pycache__/*
%{python3_sitearch}/pyximport/
