#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/FSP_macros

Name:           docs-fsp
Version:        15.31
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

# Include convenience recipe script(s)

../../parse_doc.pl --ci_run steps.tex > fsp_vanilla_recipe.sh


%install

%{__mkdir} -p %{buildroot}%{FSP_PUB}/docs
install -m 0644 -p ChangeLog %{buildroot}/%{FSP_PUB}/docs/ChangeLog
install -m 0644 -p Release_Notes.txt %{buildroot}/%{FSP_PUB}/docs/Release_Notes.txt
install -m 0644 -p %{source_path}/steps.pdf %{buildroot}/%{FSP_PUB}/docs/Install_guide.pdf 
install -m 0644 -p %{source_path}/fsp_vanilla_recipe.sh %{buildroot}/%{FSP_PUB}/docs/fsp_vanilla_recipe.sh

%files
%defattr(-,root,root)
%dir %{FSP_HOME}
%{FSP_PUB}

%changelog
