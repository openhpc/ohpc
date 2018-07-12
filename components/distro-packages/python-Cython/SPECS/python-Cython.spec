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

%if 0%{?sles_version} || 0%{?suse_version}
%define python3_prefix python3
%else
%define python3_prefix python34
%endif

Name:           %{python3_prefix}-Cython%{PROJ_DELIM}
Version:        0.26.1
Release:        0
Url:            http://www.cython.org
Summary:        The Cython compiler for writing C extensions for the Python language
License:        Apache-2.0
Group:          Development/Languages/Python
Source:         https://files.pythonhosted.org/packages/source/C/Cython/Cython-%{version}.tar.gz
Source1:        python-Cython-rpmlintrc
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  %{python3_prefix}-xml
BuildRequires:  fdupes
Requires(post): update-alternatives
Requires(postun): update-alternatives
%else
BuildRequires:  libxml2-python
Requires:       libxml2-python
Requires(post): chkconfig
Requires(postun): chkconfig
%endif
BuildRequires:  gcc-c++
BuildRequires:  %{python3_prefix}-devel
Requires:       %{python3_prefix}-devel

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
CFLAGS="%{optflags}" python3 setup.py build

%install
python3 setup.py install --prefix=%{_prefix} --root=%{buildroot}

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
   --slave %{_bindir}/cygdb cygdb %{_bindir}/cygdb-%{python3_version}

%postun
if [ $1 -eq 0 ] ; then
    "%_sbindir/update-alternatives" --remove cython %{_bindir}/cython-%{python3_version}
fi

%files
%if 0%{?leap_version} >= 420200 || 0%{?suse_version} > 1320
%license LICENSE.txt
%else
%doc LICENSE.txt
%endif
%doc COPYING.txt README.txt ToDo.txt USAGE.txt
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
