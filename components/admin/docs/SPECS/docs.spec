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

Name:           docs%{PROJ_DELIM}
Version:        2.0.0
Release:        1
Summary:        OpenHPC documentation
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/admin
URL:            https://github.com/openhpc/ohpc
Source0:        docs-ohpc.tar

BuildRequires:  git
BuildRequires:  texlive-latex
BuildRequires:  texlive-caption
BuildRequires:  texlive-colortbl
BuildRequires:  texlive-fancyhdr
BuildRequires:  texlive-mdwtools
BuildRequires:  texlive-multirow
BuildRequires:  texlive-draftwatermark
BuildRequires:  texlive-tcolorbox
BuildRequires:  texlive-environ
BuildRequires:  texlive-trimspaces
BuildRequires:  texlive-amsmath

%if 0%{?suse_version}
BuildRequires:  libstdc++6
BuildRequires:  texlive-latexmk
%endif

%if 0%{?rhel}
BuildRequires:  texlive-texconfig
BuildRequires:  texlive-metafont
BuildRequires:  texlive-cm
BuildRequires:  texlive-pdftex-def
BuildRequires:  texlive-helvetic
BuildRequires:  texlive-ec
BuildRequires:  texlive-cm-super
BuildRequires:  texlive-dvips
BuildRequires:  latexmk
%endif


%description 

This guide presents a simple cluster installation procedure using components
from the OpenHPC software stack.

%prep
%setup -n docs-ohpc

%build
%if 0%{?suse_version}
%define source_path docs/recipes/install/leap15
%else
%if 0%{?rhel}
%define source_path docs/recipes/install/centos8
%endif
%endif

%define parser ../../../../parse_doc.pl

#----------------------
# x86_64-based recipes
#----------------------

pushd docs/recipes/install/centos8/x86_64/warewulf/slurm
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/centos8/x86_64/warewulf/pbspro
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/centos8/x86_64/xcat/slurm
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/centos8/x86_64/xcat_stateful/slurm
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/leap15/x86_64/warewulf/slurm
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/leap15/x86_64/warewulf/pbspro
make ; %{parser} steps.tex > recipe.sh ; popd

#----------------------
# aarch64-based recipes
#----------------------

pushd docs/recipes/install/centos8/aarch64/warewulf/slurm
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/centos8/aarch64/warewulf/pbspro
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/leap15/aarch64/warewulf/slurm
make ; %{parser} steps.tex > recipe.sh ; popd

pushd docs/recipes/install/leap15/aarch64/warewulf/pbspro
make ; %{parser} steps.tex > recipe.sh ; popd

%install

%{__mkdir_p} %{buildroot}%{OHPC_PUB}/doc

install -m 0644 -p docs/ChangeLog %{buildroot}/%{OHPC_PUB}/doc/ChangeLog
install -m 0644 -p docs/Release_Notes.txt %{buildroot}/%{OHPC_PUB}/doc/Release_Notes.txt

# x86_64 guides

%define lpath centos8/x86_64/warewulf/slurm
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath centos8/x86_64/warewulf/pbspro
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath centos8/x86_64/xcat/slurm
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath centos8/x86_64/xcat_stateful/slurm
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath leap15/x86_64/warewulf/slurm
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath leap15/x86_64/warewulf/pbspro
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

# aarch64 guides

%define lpath centos8/aarch64/warewulf/slurm
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath centos8/aarch64/warewulf/pbspro
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath leap15/aarch64/warewulf/slurm
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

%define lpath leap15/aarch64/warewulf/pbspro
install -m 0644 -p -D docs/recipes/install/%{lpath}/steps.pdf %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/Install_guide.pdf
install -m 0755 -p -D docs/recipes/install/%{lpath}/recipe.sh %{buildroot}/%{OHPC_PUB}/doc/recipes/%{lpath}/recipe.sh

# input file templates
install -m 0644 -p docs/recipes/install/centos8/input.local.template %{buildroot}/%{OHPC_PUB}/doc/recipes/centos8/input.local
install -m 0644 -p docs/recipes/install/leap15/input.local.template %{buildroot}/%{OHPC_PUB}/doc/recipes/leap15/input.local

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%dir %{OHPC_HOME}
%{OHPC_PUB}
