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
# spec file for package python-setuptools
#
# Copyright (c) 2015 SUSE LINUX GmbH, Nuernberg, Germany.
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


Name:           python-setuptools
Version:        18.8
Release:        62.1
Url:            http://pypi.python.org/pypi/setuptools
Summary:        Easily download, build, install, upgrade, and uninstall Python packages
License:        Python Software Foundation License
Group:          Development/Languages/Python
Source:         https://pypi.python.org/packages/source/s/setuptools/setuptools-%{version}.tar.gz
Source1:        psfl.txt
Source2:        zpl.txt
Patch1:         setuptools-5.4.1-create-sitedir.patch
# NOTE(toabctl): Fix for SLE11SP3 test failures
Patch3:         fix-sle11-test-failure.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRequires:  python-devel
BuildRequires:  python-xml
# for tests
#BuildRequires:  python-mock
#BuildRequires:  python-pytest
#BuildRequires:  python-pytest-runner
# needed for SLE
Requires:       python
Requires:       python-xml
Requires(post): update-alternatives
Requires(postun): update-alternatives
# NOTE(saschpe): Distribute was merged into 0.7.x, so even though distribute
# obsoletes setuptools < 0.6.45, current setuptools obsoletes distribute again
Provides:       python-distribute = %{version}
Obsoletes:      python-distribute < %{version}
%if 0%{?suse_version} && 0%{?suse_version} <= 1110
%{!?python_sitelib: %global python_sitelib %(python -c "from distutils.sysconfig import get_python_lib; print get_python_lib()")}
%else
BuildArch:      noarch
%endif

%description
setuptools is a collection of enhancements to the Python distutils that
allow you to more easily build and distribute Python packages,
especially ones that have dependencies on other packages.

%prep
%setup -q -n setuptools-%{version}
%patch1 -p1
%if 0%{?suse_version} && 0%{?suse_version} <= 1220
%patch3 -p1
%endif
find . -type f -name "*.orig" -delete

%build
chmod -x *txt
python setup.py build

%install
python setup.py install --prefix=%{_prefix} --root=%{buildroot}
rm %{buildroot}%{_bindir}/easy_install
mkdir -p %{buildroot}%{_sysconfdir}/alternatives
touch %{buildroot}%{_sysconfdir}/alternatives/easy_install
ln -sf %{_sysconfdir}/alternatives/easy_install %{buildroot}/%{_bindir}/easy_install

%check
# Can not run testsuite as this introduces build cycle
#export LANG="en_US.UTF-8"
#python setup.py ptr --addopts='-rxs'

%post
update-alternatives \
    --install %{_bindir}/easy_install easy_install %{_bindir}/easy_install-%{py_ver} 20

%postun
if [ $1 -eq 0 ] ; then
    update-alternatives --remove easy_install %{_bindir}/easy_install-%{py_ver}
fi

%files
%defattr(-,root,root,-)
%doc CHANGES.txt README.txt
%{_bindir}/easy_install
%{_bindir}/easy_install-%{py_ver}
%ghost %{_sysconfdir}/alternatives/easy_install
%{python_sitelib}/_markerlib
%{python_sitelib}/setuptools
%{python_sitelib}/setuptools-%{version}-py%{py_ver}.egg-info
%{python_sitelib}/easy_install.py*
%{python_sitelib}/pkg_resources
