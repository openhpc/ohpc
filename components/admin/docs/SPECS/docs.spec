#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

%define recipe_base docs/recipes/install
%define recipe_dest %{buildroot}/%{OHPC_PUB}/doc/recipes

Name:           docs%{PROJ_DELIM}
Version:        4.0.0
Release:        1
Summary:        OpenHPC documentation
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/admin
URL:            https://github.com/openhpc/ohpc
Source0:        docs-ohpc.tar

BuildRequires:  git
BuildRequires:  make
BuildRequires:  texlive-latex
BuildRequires:  texlive-caption
BuildRequires:  texlive-colortbl
BuildRequires:  texlive-fancyhdr
BuildRequires:  texlive-mdwtools
BuildRequires:  texlive-multirow
#BuildRequires:  texlive-draftwatermark
BuildRequires:  texlive-tcolorbox
BuildRequires:  texlive-environ
BuildRequires:  texlive-trimspaces
BuildRequires:  texlive-amsmath


BuildRequires:  texlive-metafont
BuildRequires:  texlive-cm
BuildRequires:  texlive-helvetic
BuildRequires:  texlive-ec
BuildRequires:  texlive-cm-super
BuildRequires:  texlive-dvips
BuildRequires:  texlive-mfware
BuildRequires:  latexmk

%if 0%{?rhel}
BuildRequires:  texlive-latexconfig
BuildRequires:  texlive-tikzfill
BuildRequires:  texlive-pdftex-def
BuildRequires:  texlive-epstopdf-pkg
BuildRequires:  tex
%endif

%if 0%{?openEuler}
BuildRequires:  texlive-texlive-scripts-extra
BuildRequires:  texlive-pdftex
BuildRequires:  texlive-epstopdf
BuildRequires:  texlive-collection-basic
%endif

%description

This guide presents a simple cluster installation procedure using components
from the OpenHPC software stack.

%prep
%setup -q -n docs-ohpc

%build

%define parser ../../../../parse_doc.pl

for recipe_path in \
	"almalinux10/x86_64/confluent/slurm" \
; do
	pushd "%{recipe_base}/${recipe_path}"
	make ; %{parser} steps.tex > recipe.sh ; popd
done

%install

%{__mkdir_p} %{buildroot}%{OHPC_PUB}/doc

install -m 0644 -p docs/ChangeLog %{buildroot}/%{OHPC_PUB}/doc/ChangeLog
install -m 0644 -p docs/Release_Notes.txt %{buildroot}/%{OHPC_PUB}/doc/Release_Notes.txt

for recipe_path in \
	"almalinux10/x86_64/confluent/slurm" \
; do
	install -m 0644 -p -D "%{recipe_base}/${recipe_path}/steps.pdf" "%{recipe_dest}/${recipe_path}/Install_guide.pdf"
	install -m 0755 -p -D "%{recipe_base}/${recipe_path}/recipe.sh" "%{recipe_dest}/${recipe_path}/recipe.sh"
done

# input file templates
for distro in "almalinux10"; do
	install -m 0644 -p "%{recipe_base}/${distro}/input.local.template" "%{recipe_dest}/${distro}/input.local"
done

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%dir %{OHPC_HOME}
%{OHPC_PUB}
