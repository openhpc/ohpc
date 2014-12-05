%if 0%{?suse_version} <= 1220
%define luaver 5.1
%else
%define luaver 5.2
%endif
%define luapkgdir %{_datadir}/lua/%{luaver}

Name:           lunit
Version:        0.5
Release:        1%{?dist}
Summary:        Unit testing framework for Lua

Group:          Development/Libraries
License:        MIT
Url:            http://nessie.de/mroth/lunit
Source0:        %{name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root


# for running tests
BuildRequires:  lua >= %{luaver}
Requires:       lua >= %{luaver}

BuildArch:      noarch

%description
Lunit is a unit testing framework for lua, written in lua.


%prep
%setup -q 


%build


%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_bindir}
cp -p lunit $RPM_BUILD_ROOT%{_bindir}

mkdir -p $RPM_BUILD_ROOT%{luapkgdir}
cp -pr lunit{,-console}.lua $RPM_BUILD_ROOT%{luapkgdir}


%check
./lunit lunit-tests.lua | tee testlog.txt
grep -q "0 failed, 0 errors" testlog.txt


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc LICENSE ANNOUNCE CHANGES DOCUMENTATION README* example.lua
%{_bindir}/lunit
%dir %{_datadir}/lua
%dir %{luapkgdir}
%{luapkgdir}/*
