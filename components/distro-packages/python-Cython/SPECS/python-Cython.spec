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

%define pname Cython
Name:           python-%{pname}%{PROJ_DELIM}
Version:        0.23.4
Release:        76.1%{?dist}
Url:            http://www.cython.org
Summary:        The Cython compiler for writing C extensions for the Python language
License:        Apache-2.0
Group:          Development/Languages/Python
Source:         https://pypi.python.org/packages/source/C/Cython/Cython-%{version}.tar.gz
Source1:        python-Cython-rpmlintrc
Source2:        OHPC_macros
Patch1:         python-Cython-c++11.patch
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
BuildRequires:  python-xml
Requires:       python-xml
Requires(post): update-alternatives
Requires(postun): update-alternatives
%else
BuildRequires:  libxml2-python
Requires:       libxml2-python
%endif
BuildRequires:  gcc-c++
BuildRequires:  python-devel
Requires:       python-devel
Provides:       python-cython = %{version}
Obsoletes:      python-cython < %{version}
%if 0%{?suse_version} && 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print get_python_lib(1)")}
%endif

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
# Fix EOL encoding
sed -i "s|\r||" Demos/callback/{README.txt,cheesefinder.h} Demos/embed/Makefile.{unix,msc.static} Doc/primes.c

%build
CFLAGS="%{optflags}" python setup.py build

%install
python setup.py install --prefix=%{_prefix} --root=%{buildroot}

# Prepare for update-alternatives usage
mkdir -p %{buildroot}%{_sysconfdir}/alternatives
for p in cython cythonize cygdb ; do
    mv %{buildroot}%{_bindir}/$p %{buildroot}%{_bindir}/$p-%{py_ver}
    ln -s -f %{_sysconfdir}/alternatives/$p %{buildroot}%{_bindir}/$p
    # create a dummy target for /etc/alternatives/$p
    touch %{buildroot}%{_sysconfdir}/alternatives/$p
done

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s %{buildroot}%{python_sitearch} %{buildroot}%{_docdir}
%endif
rm -rf %{buildroot}%{python_sitearch}/__pycache__/

%post
%_sbindir/update-alternatives \
   --install %{_bindir}/cython cython %{_bindir}/cython-%{py_ver} 30 \
   --slave %{_bindir}/cythonize cythonize %{_bindir}/cythonize-%{py_ver} \
   --slave %{_bindir}/cygdb cygdb %{_bindir}/cygdb-%{py_ver}

%postun
if [ $1 -eq 0 ] ; then
    %_sbindir/update-alternatives --remove cython %{_bindir}/cython-%{py_ver}
fi

%check
%if 0%{?suse_version} && 0%{?suse_version} <= 1140
sed -i.SLES11.SP4.bak -e 's/const char/char/' ./tests/run/cpdef_extern_func.pyx
#mv ./tests/run/cpdef_extern_func.pxd ./tests/run/cpdef_extern_func.pxd.TNT.txt
#mv ./tests/run/cpdef_extern_func.pyx ./tests/run/cpdef_extern_func.pyx.TNT.txt
#sleep 60
%endif
python runtests.py -vv

%files
%defattr(-,root,root,-)
%doc COPYING.txt LICENSE.txt README.txt ToDo.txt USAGE.txt Doc Demos
%{_bindir}/cygdb
%{_bindir}/cython
%{_bindir}/cythonize
%{_bindir}/cygdb-%{py_ver}
%{_bindir}/cython-%{py_ver}
%{_bindir}/cythonize-%{py_ver}
%ghost %{_sysconfdir}/alternatives/cygdb
%ghost %{_sysconfdir}/alternatives/cython
%ghost %{_sysconfdir}/alternatives/cythonize
%{python_sitearch}/Cython/
%{python_sitearch}/Cython-%{version}-py*.egg-info
%{python_sitearch}/cython.py*
%{python_sitearch}/pyximport/

%changelog
* Tue Oct 10 2017 Adrian Reber <areber@redhat.com> - 0.23.4-76.1
- Added OHPC_macros for correct PROJ_DELIM
- Removed reference to Suse and http://bugs.opensuse.org/ as it is
  an OpenHPC spec file now
- Removed old changelog entries
