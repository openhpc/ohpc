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

Summary: Example source code and templates for use within OpenHPC environment.
Name:    examples%{PROJ_DELIM}
Version: 1.3
Release: 1
License: BSD-3
Group:   %{PROJ_NAME}/admin
URL:     https://github.com/openhpc/ohpc
Source0: OHPC_macros
Source1: hello.c
Source2: ifcfg-ib0
Source3: ifcfg-ib0.sles.ww
Source4: ifcfg-ib0.centos.ww
Source5: job.mpi
Source6: 60-ipath.rules
Source7: gmond.conf
Source8: LICENSE


BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%description

Collection of simple example programs and file templates for use within
OpenHPC development environment.

%prep

%{__cp} %SOURCE8 .

%build

%install

install -D -m 0644 %SOURCE1 %{buildroot}%{OHPC_HOME}/pub/examples/mpi/hello.c
install -D -m 0644 %SOURCE2 %{buildroot}%{OHPC_HOME}/pub/examples/network/sles/ifcfg-ib0
install -D -m 0644 %SOURCE2 %{buildroot}%{OHPC_HOME}/pub/examples/network/centos/ifcfg-ib0

install -D -m 0644 %SOURCE3 %{buildroot}%{OHPC_HOME}/pub/examples/network/sles/ifcfg-ib0.ww
install -D -m 0644 %SOURCE4 %{buildroot}%{OHPC_HOME}/pub/examples/network/centos/ifcfg-ib0.ww
install -D -m 0644 %SOURCE5 %{buildroot}%{OHPC_HOME}/pub/examples/slurm/job.mpi
install -D -m 0644 %SOURCE6 %{buildroot}%{OHPC_HOME}/pub/examples/udev/60-ipath.rules
install -D -m 0644 %SOURCE7 %{buildroot}%{OHPC_HOME}/pub/examples/ganglia/gmond.conf

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%dir %{OHPC_HOME}
%doc LICENSE
%{OHPC_PUB}

%changelog

