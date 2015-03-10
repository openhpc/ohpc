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
BuildRequires:  texlive-ncctools
BuildRequires:  latexmk
Requires:       make


BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
This guide presents a simple cluster installation procedure using components from the Forest Peak (FSP) software stack.

%prep
%setup -qn recipes

%build
%if %{suse_version}
%define source_path install/sles12/vanilla
%else
%if %{rhel_version} || %{centos_version}
%define source_path install/centos7/vanilla
%endif
%endif
cd %{source_path}
make

%install

%{__mkdir} -p %{buildroot}%{FSP_HOME}/docs
install -m 0644 -p %{source_path}/steps.pdf %{buildroot}/%{FSP_HOME}/docs/Install_guide.pdf



%files
%defattr(-,root,root)
%{FSP_HOME}

%changelog
