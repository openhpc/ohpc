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
Version: 1.2
Release: 1
License: BSD-3
Group:   fsp/admin
Source0: FSP_macros
Source1: hello.c
Source2: ifcfg-ib0
Source3: ifcfg-ib0.sles.ww
Source4: ifcfg-ib0.centos.ww
Source5: job.mpi
Source6: 60-ipath.rules
Source7: LICENSE

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
DocDir:    %{FSP_PUB}/doc/contrib

%description

Collection of simple example programs and file templates for use within
FSP development environment.

%prep

%{__cp} %SOURCE7 .

%build

df -h

%install

install -D -m 0644 %SOURCE1 %{buildroot}%{FSP_HOME}/pub/examples/mpi/hello.c
install -D -m 0644 %SOURCE2 %{buildroot}%{FSP_HOME}/pub/examples/network/sles/ifcfg-ib0
install -D -m 0644 %SOURCE2 %{buildroot}%{FSP_HOME}/pub/examples/network/centos/ifcfg-ib0

install -D -m 0644 %SOURCE3 %{buildroot}%{FSP_HOME}/pub/examples/network/sles/ifcfg-ib0.ww
install -D -m 0644 %SOURCE4 %{buildroot}%{FSP_HOME}/pub/examples/network/centos/ifcfg-ib0.ww
install -D -m 0644 %SOURCE5 %{buildroot}%{FSP_HOME}/pub/examples/slurm/job.mpi
install -D -m 0644 %SOURCE6 %{buildroot}%{FSP_HOME}/pub/examples/udev/60-ipath.rules

#install -D -m 0644 %SOURCE7 %{buildroot}%{FSP_HOME}/pub/examples/LICENSE

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%dir %{FSP_HOME}
%dir %{FSP_HOME}/pub
# {FSP_HOME}/pub/examples
%{FSP_PUB}
%doc LICENSE

%changelog

