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

%define pname examples

Summary: Example source code and templates for use within OpenHPC environment.
Name:    %{pname}%{PROJ_DELIM}
Version: 1.5
Release: 1%{?dist}
License: Apache-2.0
Group:   %{PROJ_NAME}/admin
URL:     https://github.com/openhpc/ohpc
Source0: LICENSE
Source1:  hello.c
Source2:  ifcfg-ib0
Source3:  ifcfg-ib0.sles.ww
Source4:  ifcfg-ib0.centos.ww
Source5:  job.mpi
Source6:  60-ipath.rules
Source7:  gmond.conf
Source8:  job.pbs.mpi
Source10: example.modulefile
Source11: example-mpi-dependent.modulefile



%description

Collection of simple example programs and file templates for use within
OpenHPC development environment.

%prep

%{__cp} %SOURCE0 .

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
install -D -m 0644 %SOURCE8 %{buildroot}%{OHPC_HOME}/pub/examples/pbspro/job.mpi
install -D -m 0644 %SOURCE10 %{buildroot}%{OHPC_HOME}/pub/examples/example.modulefile
install -D -m 0644 %SOURCE11 %{buildroot}%{OHPC_HOME}/pub/examples/example-mpi-dependent.modulefile

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%dir %{OHPC_HOME}
%doc LICENSE
%{OHPC_PUB}
