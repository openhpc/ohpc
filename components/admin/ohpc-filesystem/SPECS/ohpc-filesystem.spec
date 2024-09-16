#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%if 0%{?rhel} >= 9 || 0%{?openEuler} || 0%{?sle_version} >= 150400
%define version 3.2
%else
%define version 2.7
%endif

Name: ohpc-filesystem
Version: %{version}
Release: %{?dist}.1
Summary: Common top-level OpenHPC directories

Group:   ohpc/admin
License: ASL 2.0
URL:     https://github.com/openhpc/ohpc
Source0: OHPC_setup_compiler
Source1: OHPC_setup_mpi
Source2: ohpc-find-requires
Source3: ohpc-find-provides

BuildArch: noarch

%description
This administrative package is used to define top level OpenHPC installation
directories. It is utilized by most packages that do not install into system
default paths.

%package -n ohpc-buildroot
Summary: Common build scripts used in OpenHPC packaging
Group: ohpc/admin
Requires: lmod-ohpc
Requires: ohpc-filesystem

%description -n ohpc-buildroot

This administrative package is used to provide RPM dependency analysis tools
and common compiler and MPI family convenience scripts used during OpenHPC
builds.

%install
# The ohpc-filesystems owns all the common directories
mkdir -p $RPM_BUILD_ROOT/opt/ohpc/pub/{apps,doc,compiler,libs,moduledeps,modulefiles,mpi}
mkdir -p $RPM_BUILD_ROOT/opt/ohpc/admin/ohpc
mkdir -p $RPM_BUILD_ROOT/usr/lib/rpm/fileattrs

install -p -m 644 %{SOURCE0} $RPM_BUILD_ROOT/opt/ohpc/admin/ohpc
install -p -m 644 %{SOURCE1} $RPM_BUILD_ROOT/opt/ohpc/admin/ohpc

# rpm dependency plugins
install -p -m 755 %{SOURCE2} $RPM_BUILD_ROOT/usr/lib/rpm
install -p -m 755 %{SOURCE3} $RPM_BUILD_ROOT/usr/lib/rpm

%{__mkdir_p} %{buildroot}/usr/lib/rpm/fileattrs/
%{__cat} <<EOF > %{buildroot}//usr/lib/rpm/fileattrs/ohpc.attr
%%__ohpc_provides        /usr/lib/rpm/ohpc-find-provides
%%__ohpc_requires        /usr/lib/rpm/ohpc-find-requires %%{buildroot} %{OHPC_HOME}

%%__ohpc_path            ^(%{OHPC_HOME})|(/usr/sbin/slurm.*)|(/usr/lib64/slurm/mpi_pmix.*so)|(/opt/pbs/sbin/pbs.*)$
%%__elf_exclude_path     ^(%{OHPC_HOME})|(/usr/sbin/slurm.*)|(/usr/lib64/slurm/mpi_pmix.*so)|(/opt/pbs/sbin/pbs.*)$

%%__ohpc_magic           ^ELF (32|64)-bit.*$
%%__ohpc_flags           magic_and_path
EOF

%if 0%{?sles_version} || 0%{?suse_version}
%{__cat} <<EOF >> %{buildroot}//usr/lib/rpm/fileattrs/ohpc.attr
%%__elflib_exclude_path  ^%{OHPC_HOME}
EOF
%endif


%files
%dir /opt/ohpc/
%dir /opt/ohpc/admin/
%dir /opt/ohpc/pub/
%dir /opt/ohpc/pub/apps/
%dir /opt/ohpc/pub/doc/
%dir /opt/ohpc/pub/compiler/
%dir /opt/ohpc/pub/libs/
%dir /opt/ohpc/pub/moduledeps/
%dir /opt/ohpc/pub/modulefiles/
%dir /opt/ohpc/pub/mpi/

%files -n ohpc-buildroot
%dir /opt/ohpc/admin/ohpc/
%dir /usr/lib/rpm/
%dir /usr/lib/rpm/fileattrs/
/opt/ohpc/admin/ohpc/OHPC_setup_compiler
/opt/ohpc/admin/ohpc/OHPC_setup_mpi
/usr/lib/rpm/ohpc-find-provides
/usr/lib/rpm/ohpc-find-requires
/usr/lib/rpm/fileattrs/ohpc.attr
