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

Name:		%{pname}%{PROJ_DELIM}
Version:	0.8.17
Release:	1%{?dist}
Summary:	HPC software package management

Group:		System/Configuration
License:	LGPL
URL:		https://github.com/LLNL/spack
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

%define install_path %{OHPC_ADMIN}/%{pname}/%version

%description
Spack is a package management tool designed to support multiple versions and configurations of software on a wide variety of platforms and environments. It was designed for large supercomputing centers, where many users and application teams share common installations of software on clusters with exotic architectures, using libraries that do not have a standard ABI. Spack is non-destructive: installing a new version does not break existing installations, so many configurations can coexist on the same system.

Most importantly, Spack is simple. It offers a simple spec syntax so that users can specify versions and configuration options concisely. Spack is also simple for package authors: package files are written in pure Python, and specs allow package authors to write a single build script for many different builds of the same package.


%prep
%setup -q -n %{pname}-%{version}

%install
mkdir -p %{buildroot}%{install_path}
rsync -av --exclude=.gitignore {bin,lib,var,share} %{buildroot}%{install_path}

# OpenHPC module file
%{__mkdir} -p %{buildroot}/%{OHPC_ADMIN}/modulefiles/spack
%{__cat} << EOF > %{buildroot}/%{OHPC_ADMIN}/modulefiles/spack/%{version}
#%Module1.0#####################################################################

module-whatis "Name: Spack"
module-whatis "Version: %{version}"
module-whatis "Category: System/Configuration"
module-whatis "Description: Spack package management"
module-whatis "URL: https://github.com/LLNL/spack/"

set     version             %{version}
set     SPACK_ROOT          %{install_path}

prepend-path   PATH         %{install_path}/bin
prepend-path   MODULEPATH   %{install_path}/modules

EOF

%{__mkdir} -p %{RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT
%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_ADMIN}
%doc LICENSE README.md

%changelog
