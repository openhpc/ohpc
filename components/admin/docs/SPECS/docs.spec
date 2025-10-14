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
BuildRequires:  python3
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

%define parser_perl ../../../../parse_doc.pl
%define parser_python ../../../../parse_doc.py

for recipe_path in \
	"almalinux10/x86_64/confluent/slurm" \
	"almalinux10/x86_64/openchami/slurm" \
	"almalinux10/x86_64/warewulf4/slurm" \
	"almalinux10/aarch64/warewulf4/slurm" \
	"openeuler24.03/x86_64/warewulf4/slurm" \
	"openeuler24.03/aarch64/warewulf4/slurm" \
	"rocky10/x86_64/warewulf4/slurm" \
	"rocky10/aarch64/warewulf4/slurm" \
	"rocky10/x86_64/openchami/slurm" \
; do
	pushd "%{recipe_base}/${recipe_path}"
	make

	# Generate output with both parsers
	echo "Generating recipe.sh for ${recipe_path}..."
	%{parser_perl} steps.tex > recipe_perl.sh
	%{parser_python} steps.tex > recipe_python.sh

	# Compare outputs and fail if they differ (ignoring comment-only differences)
	echo "Comparing Perl and Python parser outputs for ${recipe_path}..."
	if ! diff -u recipe_perl.sh recipe_python.sh > parser_diff.log 2>&1; then
		# Filter out differences that are only comment lines (both added and removed)
		# Keep only non-comment differences for evaluation (excluding diff headers)
		if grep -E "^[+-]" parser_diff.log | grep -v -E "^[+-][[:space:]]*#" | grep -v -E "^[+-]{3}" | grep -q "^[+-]"; then
			echo "ERROR: Parser outputs have significant differences for ${recipe_path}!"
			echo "Differences found (ignoring comment-only lines):"
			cat parser_diff.log
			echo "Perl output (first 50 lines):"
			head -50 recipe_perl.sh
			echo "Python output (first 50 lines):"
			head -50 recipe_python.sh
			exit 1
		else
			echo "Note: Only comment line differences detected, parsers functionally equivalent"
		fi
	else
		echo "Parser outputs are identical"
	fi

	# Use Python parser output as the final result
	cp recipe_python.sh recipe.sh

	# Clean up temporary files
	rm -f recipe_perl.sh recipe_python.sh parser_diff.log

	popd
done

%install

%{__mkdir_p} %{buildroot}%{OHPC_PUB}/doc

install -m 0644 -p docs/ChangeLog %{buildroot}/%{OHPC_PUB}/doc/ChangeLog
install -m 0644 -p docs/Release_Notes.txt %{buildroot}/%{OHPC_PUB}/doc/Release_Notes.txt

for recipe_path in \
	"almalinux10/x86_64/confluent/slurm" \
	"almalinux10/x86_64/openchami/slurm" \
	"almalinux10/x86_64/warewulf4/slurm" \
	"almalinux10/aarch64/warewulf4/slurm" \
	"openeuler24.03/x86_64/warewulf4/slurm" \
	"openeuler24.03/aarch64/warewulf4/slurm" \
	"rocky10/x86_64/warewulf4/slurm" \
	"rocky10/aarch64/warewulf4/slurm" \
	"rocky10/x86_64/openchami/slurm" \
; do
	install -m 0644 -p -D "%{recipe_base}/${recipe_path}/steps.pdf" "%{recipe_dest}/${recipe_path}/Install_guide.pdf"
	install -m 0755 -p -D "%{recipe_base}/${recipe_path}/recipe.sh" "%{recipe_dest}/${recipe_path}/recipe.sh"
done

# input file templates
for distro in "almalinux10" "rocky10" "openeuler24.03"; do
	install -m 0644 -p "%{recipe_base}/${distro}/input.local.template" "%{recipe_dest}/${distro}/input.local"
done

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%dir %{OHPC_HOME}
%{OHPC_PUB}
