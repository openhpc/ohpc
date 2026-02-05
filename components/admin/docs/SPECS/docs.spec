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

%define recipe_base docs/install
%define recipe_dest %{buildroot}/%{OHPC_PUB}/doc/recipes

Name:           docs%{PROJ_DELIM}
Version:        4.0.0
Release:        1
Summary:        OpenHPC documentation
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/admin
URL:            https://github.com/openhpc/ohpc
Source0:        docs-ohpc.tar
BuildArch:      noarch

BuildRequires:  git
BuildRequires:  make
BuildRequires:  python3
BuildRequires:  python3-jinja2
BuildRequires:  python3-pyyaml
BuildRequires:  yq

# PDF generation (pandoc + xelatex)
BuildRequires:  pandoc
BuildRequires:  texlive-xetex
BuildRequires:  texlive-adjustbox
BuildRequires:  texlive-collectbox
BuildRequires:  texlive-unicode-math
BuildRequires:  texlive-lm-math
BuildRequires:  texlive-latex
BuildRequires:  texlive-fancyhdr
BuildRequires:  texlive-amsmath
BuildRequires:  texlive-mdwtools

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

pushd %{recipe_base}
make PYTHON=python3                # .md + .sh
make PYTHON=python3 pdf            # .pdf
make PYTHON=python3 html           # .html
popd

%install

%{__mkdir_p} %{buildroot}%{OHPC_PUB}/doc

install -m 0644 -p docs/ChangeLog %{buildroot}/%{OHPC_PUB}/doc/ChangeLog
install -m 0644 -p docs/Release_Notes.txt %{buildroot}/%{OHPC_PUB}/doc/Release_Notes.txt

# Install recipes
for recipe_name in \
	"rocky10-x86_64-warewulf-slurm" \
	"rocky10-aarch64-warewulf-slurm" \
	"almalinux10-x86_64-warewulf-slurm" \
	"almalinux10-aarch64-warewulf-slurm" \
	"rocky10-x86_64-confluent-slurm" \
	"rocky10-aarch64-confluent-slurm" \
	"almalinux10-x86_64-confluent-slurm" \
	"almalinux10-aarch64-confluent-slurm" \
	"rocky10-x86_64-openchami-slurm" \
	"rocky10-aarch64-openchami-slurm" \
	"almalinux10-x86_64-openchami-slurm" \
	"almalinux10-aarch64-openchami-slurm" \
	"openeuler24.03-x86_64-warewulf-slurm" \
	"openeuler24.03-aarch64-warewulf-slurm" \
; do
	# Parse: distro-arch-provisioner-scheduler -> distro/arch/provisioner/scheduler
	distro=$(echo "${recipe_name}" | cut -d- -f1)
	arch=$(echo "${recipe_name}" | cut -d- -f2)
	provisioner=$(echo "${recipe_name}" | cut -d- -f3)
	scheduler=$(echo "${recipe_name}" | cut -d- -f4)
	dest="${distro}/${arch}/${provisioner}/${scheduler}"

	install -m 0644 -p -D "%{recipe_base}/build/${recipe_name}.pdf"  "%{recipe_dest}/${dest}/Install_guide.pdf"
	install -m 0644 -p -D "%{recipe_base}/build/${recipe_name}.md"   "%{recipe_dest}/${dest}/Install_guide.md"
	install -m 0644 -p -D "%{recipe_base}/build/${recipe_name}.html" "%{recipe_dest}/${dest}/Install_guide.html"
	install -m 0755 -p -D "%{recipe_base}/build/${recipe_name}.sh"   "%{recipe_dest}/${dest}/recipe.sh"
done

# Install input.local templates
for distro in "almalinux10" "rocky10" "openeuler24.03"; do
	install -m 0644 -p -D "%{recipe_base}/input.local.template" "%{recipe_dest}/${distro}/input.local"
done

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%dir %{OHPC_HOME}
%{OHPC_PUB}
