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
Source0:        docs-fsp.tar
BuildRequires:  texlive-latex
BuildRequires:  texlive-caption
BuildRequires:  texlive-colortbl
BuildRequires:  texlive-fancyhdr
BuildRequires:  texlive-mdwtools
BuildRequires:  texlive-multirow
BuildRequires:  texlive-draftwatermark
BuildRequires:  latexmk
BuildRequires:  git
Requires:       make

%define debug_package %{nil}

BuildRoot:     %{_tmppath}/%{name}-%{version}-build
DocDir:        %{FSP_PUB}/doc/contrib

%description
This guide presents a simple cluster installation procedure using components from the Forest Peak (FSP) software stack.

%prep
%setup -n docs-fsp

%build
%if 0%{?suse_version}
%define source_path docs/recipes/install/sles12/vanilla
%else
%if 0%{?rhel_version} || 0%{?centos_version}
%define source_path docs/recipes/install/centos7.1/vanilla
%endif
%endif

cd %{source_path}
make

# Include convenience recipe script(s)

../../parse_doc.pl steps.tex > fsp_vanilla_recipe.sh


%install

%{__mkdir_p} %{buildroot}%{FSP_PUB}/doc
%{__mkdir_p} %{buildroot}%{FSP_PUB}/doc/recipes/vanilla
install -m 0644 -p docs/recipes/install/ChangeLog %{buildroot}/%{FSP_PUB}/doc/ChangeLog
install -m 0644 -p docs/recipes/install/Release_Notes.txt %{buildroot}/%{FSP_PUB}/doc/Release_Notes.txt
install -m 0644 -p %{source_path}/steps.pdf %{buildroot}/%{FSP_PUB}/doc/Install_guide.pdf 
install -m 0755 -p %{source_path}/fsp_vanilla_recipe.sh %{buildroot}/%{FSP_PUB}/doc/recipes/vanilla/recipe.sh
install -m 0644 -p docs/recipes/install/input.local.template %{buildroot}/%{FSP_PUB}/doc/recipes/vanilla/input.local

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%defattr(-,root,root)
%dir %{FSP_HOME}
%{FSP_PUB}
%doc LICENSE

%changelog
