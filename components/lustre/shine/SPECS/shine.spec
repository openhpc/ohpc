# Base package name
%define pname shine

Name:      %{pname}%{?PROJ_DELIM}
Summary:   Lustre administration utility
Version:   1.4
Release:   1%{?dist}
Source0:   http://downloads.sourceforge.net/lustre-shine/%{pname}/%{version}/%{pname}-%{version}.tar.gz
License:   GPLv2
Group:     Applications/System
Vendor:    CEA
Url:       http://lustre-shine.sourceforge.net/
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root-%(%{__id_u} -n)
BuildArch: noarch
Requires:  clustershell >= 1.5.1
Provides:  %{pname} = %{version}

%description
Lustre administration utility.

%prep
%setup -q -n %{pname}-%{version}

%build
export SHINEVERSION=%{version}
python setup.py build

%install
export SHINEVERSION=%{version}
python setup.py install --root=%{buildroot} --record=INSTALLED_FILES
mkdir -p %{buildroot}/%{_sysconfdir}/shine/models
cp conf/*.conf* %{buildroot}/%{_sysconfdir}/shine
cp conf/models/* %{buildroot}/%{_sysconfdir}/shine/models
# man pages
mkdir -p %{buildroot}/%{_mandir}/{man1,man5}
gzip -c doc/shine.1 >%{buildroot}/%{_mandir}/man1/shine.1.gz
gzip -c doc/shine.conf.5 >%{buildroot}/%{_mandir}/man5/shine.conf.5.gz

%clean
rm -rf %{buildroot}

%files -f INSTALLED_FILES
%defattr(-,root,root)
%config(noreplace) %{_sysconfdir}/shine/*.conf
%config %{_sysconfdir}/shine/*.conf.example
%config %{_sysconfdir}/shine/models/*.lmf
%doc LICENSE README ChangeLog
%doc %{_mandir}/man1/shine.1.gz
%doc %{_mandir}/man5/shine.conf.5.gz

%changelog
* Wed Feb 24 2016 <aurelien.degremont@cea.fr> - 1.4-1
- Initial packaging for OpenHPC
