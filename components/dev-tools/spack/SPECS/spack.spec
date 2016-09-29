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
Patch0:   spack.patch

BuildArch: noarch
BuildRequires:	rsync
Requires:	python >= 2.6
Requires: bash
Requires: coreutils

# Default library install path
# relocation still needs some work
# %define install_path %{OHPC_HOME}/admin/%{pname}
%define install_path /usr

%description
Spack is a package management tool designed to support multiple versions and configurations of software on a wide variety of platforms and environments. It was designed for large supercomputing centers, where many users and application teams share common installations of software on clusters with exotic architectures, using libraries that do not have a standard ABI. Spack is non-destructive: installing a new version does not break existing installations, so many configurations can coexist on the same system.

Most importantly, Spack is simple. It offers a simple spec syntax so that users can specify versions and configuration options concisely. Spack is also simple for package authors: package files are written in pure Python, and specs allow package authors to write a single build script for many different builds of the same package.


%prep
%setup -q -n %{pname}-%{version}
%patch0 -p0

%install
mkdir -p %{buildroot}%{install_path}
rsync -av --exclude=.gitignore {bin,lib,var} %{buildroot}%{install_path}
#./bin/spack bootstrap %{buildroot}%{install_path}
mkdir -vp %{buildroot}/etc/profile.d
mv share/spack/setup-env.sh  share/spack/spack-env.sh
mv share/spack/setup-env.csh share/spack/spack-env.csh
cp -prv share/spack/* %{buildroot}/etc/profile.d

%files
%doc LICENSE README.md
%{install_path}/bin/*
%dir %{install_path}/lib/spack
%dir %{install_path}/lib/spack/docs
%{install_path}/lib/spack/docs/*
%{install_path}/lib/spack/env
%{install_path}/lib/spack/external
%{install_path}/lib/spack/llnl
%dir %{install_path}/lib/spack/spack
%{install_path}/lib/spack/spack/*
%dir /etc/profile.d
/etc/profile.d/*
%dir %{install_path}/var
%dir %{install_path}/var/spack
%dir %{install_path}/var/spack/mock_configs
%{install_path}/var/spack/mock_configs/*
%dir %{install_path}/var/spack/mock_packages
%{install_path}/var/spack/mock_packages/*
%dir %{install_path}/var/spack/packages
%{install_path}/var/spack/packages/*
