%include %{_sourcedir}/FSP_macros

Name:           docs-fsp
Version:        15.16
Release:        1
Summary:        Forest Peak documentation
License:        BSD-3-Clause
Group:          fsp/admin
Source0:        docs-fsp-%{version}.tar.gz
BuildRequires:  texlive-latex
BuildRequires:  texlive-caption
BuildRequires:  texlive-colortbl
BuildRequires:  texlive-fancyhdr
BuildRequires:  texlive-mdwtools
BuildRequires:  texlive-multirow
BuildRequires:  texlive-draftwatermark
BuildRequires:  latexmk
Requires:       make

%define debug_package %{nil}

BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
This guide presents a simple cluster installation procedure using components from the Forest Peak (FSP) software stack.

%prep
%setup 

%build
%if 0%{?suse_version}
%define source_path sles12/vanilla
%else
%if 0%{?rhel_version} || 0%{?centos_version}
%define source_path centos7/vanilla
%endif
%endif
cd %{source_path}
make

%install

%{__mkdir} -p %{buildroot}%{FSP_PUB}/docs
install -m 0644 -p ChangeLog %{buildroot}/%{FSP_PUB}/docs/ChangeLog
install -m 0644 -p Release_Notes.txt %{buildroot}/%{FSP_PUB}/docs/Release_Notes.txt
install -m 0644 -p %{source_path}/steps.pdf %{buildroot}/%{FSP_PUB}/docs/Install_guide.pdf 


%files
%defattr(-,root,root)
%dir %{FSP_HOME}
%{FSP_PUB}/docs/*

%changelog
