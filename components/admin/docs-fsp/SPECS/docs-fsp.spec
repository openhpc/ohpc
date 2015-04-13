%include %{_sourcedir}/FSP_macros

Name:           docs-fsp
Version:        0.1
Release:        1.2
Summary:        Forest Peak documentation
License:        BSD-3-Clause
Group:          fsp/admin
Source0:        recipes.tar.gz
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
%setup -qn recipes

%build
%if 0%{?suse_version}
%define source_path install/sles12/vanilla
%else
%if 0%{?rhel_version} || 0%{?centos_version}
%define source_path install/centos7/vanilla
%endif
%endif
cd %{source_path}
make

%install

mv %{source_path}/steps.pdf %{source_path}/Install_guide.pdf
%{__mkdir} -p %{buildroot}%{FSP_PUB}/docs
install -m 0644 -p %{source_path}/Install_guide.pdf %{buildroot}/%{FSP_PUB}/docs/install_guide.pdf 



%files
%defattr(-,root,root)
%{FSP_PUB}

%changelog
