
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
Patch1:         specfile_search.patch
# Fedora compatibility
Provides:       python2-rpm-macros
Provides:       python3-rpm-macros
#!BuildIgnore:  python-rpm-macros
BuildArch:      noarch

%description
This package contains SUSE RPM macros for Python build automation.
You should BuildRequire this package unless you are sure that you
are only building for distros newer than Leap 42.2

%prep
%setup -q -n python-rpm-macros-2017.12.07.9d4e9eb
%if 0%{?suse_version} < 1330
mv macros-default-pythons macros/035-default-pythons
%endif
%patch1 -p1

%if 0%{?suse_version}
%global install_path %{_sysconfdir}/rpm
%else
%global install_path /usr/lib/rpm/macros.d
%endif

%build
./compile-macros.sh

%install
mkdir -p %{buildroot}%{install_path}
install -m 644 macros.python_all %{buildroot}%{install_path}

%files
%{install_path}/macros.python_all

