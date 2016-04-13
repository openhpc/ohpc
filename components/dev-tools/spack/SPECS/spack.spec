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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define pname spack

Name:		  %{pname}%{PROJ_DELIM}
Version:	0.8.17
Release:	1%{?dist}
Summary:	HPC software package management

Group:		System/Configuration
License:	LGPL
URL:		  https://github.com/LLNL/spack
Source0:	https://github.com/LLNL/%{pname}/archive/v%{version}.tar.gz

BuildArch: noarch
BuildRequires:	rsync
BuildRequires:	python
Requires:	python >= 2.6
Requires: bash
Requires: coreutils
Requires: subversion
Requires: hg
Requires: patch
DocDir:    %{OHPC_PUB}/doc/contrib

%define install_path %{OHPC_HOME}/admin/%{pname}

%description
Spack is a package management tool designed to support multiple versions and configurations of software on a wide variety of platforms and environments. It was designed for large supercomputing centers, where many users and application teams share common installations of software on clusters with exotic architectures, using libraries that do not have a standard ABI. Spack is non-destructive: installing a new version does not break existing installations, so many configurations can coexist on the same system.

Most importantly, Spack is simple. It offers a simple spec syntax so that users can specify versions and configuration options concisely. Spack is also simple for package authors: package files are written in pure Python, and specs allow package authors to write a single build script for many different builds of the same package.


%prep
%setup -q -n %{pname}-%{version}

%install
mkdir -p %{buildroot}%{install_path}
rsync -av --exclude=.gitignore {bin,lib,var} %{buildroot}%{install_path}


%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%{OHPC_PUB}
%doc LICENSE README.md

%changelog
