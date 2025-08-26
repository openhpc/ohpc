#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# https://github.com/mikefarah/yq
Name:           yq-ohpc
Version:        4.47.1
Release:        1%{?dist}
Group:          %{PROJ_NAME}/admin
Summary:        Yq is a portable command-line YAML, JSON, XML, CSV, TOML  and properties processor

License:        Apache-2.0 AND BSD-2-Clause AND BSD-3-Clause AND MIT
URL:            github.com/mikefarah/yq/v4
Source:         https://github.com/mikefarah/yq/archive/v%{version}.tar.gz#/yq-%{version}.tar.gz

BuildRequires:  golang >= 1.24

%description
Yq is a portable command-line YAML, JSON, XML, CSV, TOML  and properties
processor.

%global debug_package %{nil}

%prep
%autosetup -n yq-%{version}
go mod vendor

%build
go build -mod=vendor -v -o yq

%check
go test -mod=vendor -v ./...

%install
install -m 0755 -vd                     %{buildroot}%{_bindir}
install -m 0755 -vp yq %{buildroot}%{_bindir}/

%files
%license LICENSE
%doc examples CODE_OF_CONDUCT.md how-it-works.md project-words.txt
%doc release_instructions.txt CONTRIBUTING.md README.md release_notes.txt
%{_bindir}/yq
