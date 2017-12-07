#
# spec file for package python-rpm-macros
#
# Copyright (c) 2017 SUSE LINUX GmbH, Nuernberg, Germany.
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

%include %{_sourcedir}/OHPC_macros

Name:           python-rpm-macros%{PROJ_DELIM}
Version:        2017.12.07.9d4e9eb
Release:        0
Summary:        RPM macros for building of Python modules
License:        WTFPL
Group:          %{PROJ_NAME}/distro-packages
Url:            https://github.com/opensuse/multipython-macros
Source:         python-rpm-macros-%{version}.tar.bz2
Source100:      README.packaging
Source101:      update.sh
# Fedora compatibility
Provides:       python2-rpm-macros
Provides:       python3-rpm-macros
#!BuildIgnore:  python-rpm-macros
BuildArch:      noarch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
This package contains SUSE RPM macros for Python build automation.
You should BuildRequire this package unless you are sure that you
are only building for distros newer than Leap 42.2

%prep
%setup -q -n python-rpm-macros-2017.12.07.9d4e9eb
%if 0%{?suse_version} < 1330
mv macros-default-pythons macros/035-default-pythons
%endif

%build
./compile-macros.sh

%install
mkdir -p %{buildroot}%{_sysconfdir}/rpm
install -m 644 macros.python_all %{buildroot}%{_sysconfdir}/rpm

%files
%defattr(-,root,root)
%{_sysconfdir}/rpm/macros.python_all

%changelog
