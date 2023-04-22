#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%global ohpc_python_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%global pname dasbus

Name:          %{python_prefix}-%{pname}%{PROJ_DELIM}
Summary:       DBus library in Python 3
Version:       1.6
Release:       1
Group:         %{PROJ_NAME}/distro-packages
License:       License:        LGPL-2.1-or-later
URL:           https://pypi.python.org/pypi/dasbus
Source0:       https://github.com/rhinstaller/dasbus/releases/download/v%{version}/dasbus-%{version}.tar.gz

Provides: python3-dasbus = %{version}
%if 0%{?suse_version} || 0%{?sle_version}
BuildRequires:  fdupes
Requires:       python3-gobject
%else
Requires:       python3-gobject-base
%endif

%description
Dasbus is a DBus library written in Python 3, based on
GLib and inspired by pydbus. It is designed to be easy
to use and extend.


%prep
%setup -q -n %{pname}-%{version}


%build
%{python_prefix} setup.py build


%install
# setup.py install is deprecated, but simple for this stopgap fix
%{python_prefix} setup.py install --root=%{buildroot}


%files
%doc README.md
%license LICENSE
%{python3_sitelib}/%{pname}*
