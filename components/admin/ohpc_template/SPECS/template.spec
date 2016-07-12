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
License:   Apache-2.0
Group:     ohpc/admin
BuildArch: noarch
URL:       https://github.com/openhpc/ohpc
Source1:   OHPC_macros
Source2:   config.machines
Source3:   config.default
Source4:   notify_header
Source5:   packages-os.config
Source6:   packages-custom.config
Source7:   provision.conf.master
Source8:   tftp.master
Source9:   warewulf-httpd.conf.master
Source10:  limits.conf.master
Source11:  limits.conf.compute
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
%{__mkdir_p} %{buildroot}/%{installPath}/const_files/default
install -D -p -m 0640 %{SOURCE2} %{buildroot}/%{installPath}/config.machines
install -D -p -m 0640 %{SOURCE3} %{buildroot}/%{installPath}/config.default
install -D -p -m 0640 %{SOURCE4} %{buildroot}/%{installPath}/notify_header
install -D -p -m 0640 %{SOURCE5} %{buildroot}/%{installPath}/os-packages/default/packages.config
install -D -p -m 0640 %{SOURCE6} %{buildroot}/%{installPath}/custom-packages/default/packages.config

# config file templates
install -D -p -m 0600 %{SOURCE7}  %{buildroot}/%{installPath}/const_files/default/master/provision.conf
install -D -p -m 0600 %{SOURCE8}  %{buildroot}/%{installPath}/const_files/default/master/tftp
install -D -p -m 0600 %{SOURCE9}  %{buildroot}/%{installPath}/const_files/default/master/warewulf-httpd.conf
install -D -p -m 0600 %{SOURCE10} %{buildroot}/%{installPath}/const_files/default/master/limits.conf
install -D -p -m 0600 %{SOURCE11} %{buildroot}/%{installPath}/const_files/default/compute/limits.conf


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{installPath}
%dir %{OHPC_PUB}/examples


