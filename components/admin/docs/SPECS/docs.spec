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

%define recipe_source docs/install
%define recipe_dest   %{buildroot}/%{OHPC_PUB}/doc/recipes

Name:           docs%{PROJ_DELIM}
Version:        3.5.0
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

%description

This guide presents a simple cluster installation procedure using components
from the OpenHPC software stack.

%prep
%setup -q -n docs-ohpc

%build

cd %{recipe_source}
make PYTHON=python3

%install

%{__mkdir_p} %{buildroot}%{OHPC_PUB}/doc

install -m 0644 -p docs/ChangeLog %{buildroot}/%{OHPC_PUB}/doc/ChangeLog
install -m 0644 -p docs/Release_Notes.txt %{buildroot}/%{OHPC_PUB}/doc/Release_Notes.txt

for recipe in \
	"rocky9/x86_64/warewulf/slurm" \
	"rocky9/x86_64/warewulf3/slurm" \
	"rocky9/x86_64/warewulf3/openpbs" \
	"rocky9/x86_64/confluent/slurm" \
	"rocky9/x86_64/openchami/slurm" \
	"almalinux9/x86_64/warewulf/slurm" \
	"almalinux9/x86_64/warewulf3/slurm" \
	"almalinux9/x86_64/warewulf3/openpbs" \
	"almalinux9/x86_64/confluent/slurm" \
	"almalinux9/x86_64/openchami/slurm" \
	"openeuler22.03/x86_64/warewulf3/slurm" \
	"openeuler22.03/x86_64/warewulf3/openpbs" \
	"leap15/x86_64/warewulf3/slurm" \
	"leap15/x86_64/warewulf3/openpbs" \
	"rocky9/aarch64/warewulf/slurm" \
	"rocky9/aarch64/warewulf3/slurm" \
	"rocky9/aarch64/warewulf3/openpbs" \
	"rocky9/aarch64/confluent/slurm" \
	"rocky9/aarch64/openchami/slurm" \
	"almalinux9/aarch64/warewulf/slurm" \
	"almalinux9/aarch64/warewulf3/slurm" \
	"almalinux9/aarch64/warewulf3/openpbs" \
	"almalinux9/aarch64/confluent/slurm" \
	"almalinux9/aarch64/openchami/slurm" \
	"openeuler22.03/aarch64/warewulf3/slurm" \
	"openeuler22.03/aarch64/warewulf3/openpbs" \
	"leap15/aarch64/warewulf3/slurm" \
	"leap15/aarch64/warewulf3/openpbs" \
; do
	name=$(echo "$recipe" | tr '/' '-')
	install -m 0644 -p -D "%{recipe_source}/build/${name}.md" \
		"%{recipe_dest}/${recipe}/Install_guide.md"
	install -m 0755 -p -D "%{recipe_source}/build/${name}.sh" \
		"%{recipe_dest}/${recipe}/recipe.sh"
done

# input.local template (one per distro, shared across arch/provisioner/scheduler)
for distro in "rocky9" "almalinux9" "leap15" "openeuler22.03"; do
	install -m 0644 -p "%{recipe_source}/input.local.template" \
		"%{recipe_dest}/${distro}/input.local"
done

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%dir %{OHPC_HOME}
%{OHPC_PUB}
