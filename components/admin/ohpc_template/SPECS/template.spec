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

%define pname losf-config-templates

Summary:   A Linux operating system framework for managing HPC clusters
Name:      %{pname}%{PROJ_DELIM}
Version:   1.1
Release:   1
License:   BSD-3
Group:     ohpc/admin
BuildArch: noarch
URL:       https://github.com/openhpc/ohpc
Source1:   OHPC_macros
Source2:   config.machines
Source3:   config.default
Source4:   packages-os.config
Source5:   packages-custom.config
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%define installPath %{OHPC_PUB}/examples/losf-template

Requires: losf%{PROJ_DELIM}

%description

Provides configuration templates for use with configuration management schemes
and OpenHPC.

%prep

%build

%install
rm -rf $RPM_BUILD_ROOT
%{__mkdir_p} %{buildroot}/%{installPath}
install -D -p -m 0640 %{SOURCE2} %{buildroot}/%{installPath}/config.machines
install -D -p -m 0640 %{SOURCE3} %{buildroot}/%{installPath}/config.default
install -D -p -m 0640 %{SOURCE4} %{buildroot}/%{installPath}/os-packages/default/packages.config
install -D -p -m 0640 %{SOURCE5} %{buildroot}/%{installPath}/custom-packages/default/packages.config

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{installPath}
%dir %{OHPC_PUB}/examples


