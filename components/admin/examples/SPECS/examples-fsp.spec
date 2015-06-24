#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/FSP_macros
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary: Example source code and templates for use within FSP environment.
Name:    examples%{PROJ_DELIM}
Version: 1.1
Release: 1
License: BSD
Group:   fsp/admin
Source0: FSP_macros
Source1: hello.c
Source2: ifcfg-ib0
Source3: ifcfg-ib0.sles.ww
Source4: ifcfg-ib0.centos.ww

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description

Collection of simple example programs and file templates for use wihin
FSP development environment.

%prep

%build

df -h

%install

install -D -m 0644 %SOURCE1 %{buildroot}%{FSP_HOME}/pub/examples/mpi/hello.c
install -D -m 0644 %SOURCE2 %{buildroot}%{FSP_HOME}/pub/examples/network/sles/ifcfg-ib0
install -D -m 0644 %SOURCE2 %{buildroot}%{FSP_HOME}/pub/examples/network/centos/ifcfg-ib0

install -D -m 0644 %SOURCE3 %{buildroot}%{FSP_HOME}/pub/examples/network/sles/ifcfg-ib0.ww
install -D -m 0644 %SOURCE4 %{buildroot}%{FSP_HOME}/pub/examples/network/centos/ifcfg-ib0.ww


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%dir %{FSP_HOME}
%dir %{FSP_HOME}/pub
%{FSP_HOME}/pub/examples

%changelog

