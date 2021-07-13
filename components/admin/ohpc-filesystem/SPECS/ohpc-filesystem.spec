#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%define ohpc_bootstrap 1
%include %{_sourcedir}/OHPC_macros

%define pname filesystem

Name:    %{PROJ_NAME}-%{pname}
Version: 2.0.1
Release: 1%{?dist}
Summary: Common top-level OpenHPC directories

Group:   %{PROJ_NAME}/admin
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


%prep
%build
%install
# The ohpc-filesystems owns all the common directories
mkdir -p $RPM_BUILD_ROOT/%{OHPC_ADMIN}/ohpc
mkdir -p $RPM_BUILD_ROOT/{%{OHPC_APPS},%{OHPC_COMPILERS},%{OHPC_LIBS},%{OHPC_MODULES},%{OHPC_MODULEDEPS},%{OHPC_MPI_STACKS},%{OHPC_UTILS}}
mkdir -p $RPM_BUILD_ROOT/%{_docdir}

# Install compiler and MPI setup scripts
install -p -m 644 %{SOURCE0} $RPM_BUILD_ROOT/%{OHPC_ADMIN}/ohpc
install -p -m 644 %{SOURCE1} $RPM_BUILD_ROOT/%{OHPC_ADMIN}/ohpc

# rpm dependency plugins
mkdir -p $RPM_BUILD_ROOT/usr/lib/rpm/fileattrs
install -p -m 755 %{SOURCE2} $RPM_BUILD_ROOT/usr/lib/rpm
install -p -m 755 %{SOURCE3} $RPM_BUILD_ROOT/usr/lib/rpm

cat <<EOF > $RPM_BUILD_ROOT/usr/lib/rpm/fileattrs/ohpc.attr
%%__ohpc_provides        /usr/lib/rpm/ohpc-find-provides
%%__ohpc_requires        /usr/lib/rpm/ohpc-find-requires %%{buildroot} %{OHPC_HOME}

%%__ohpc_path            ^%{OHPC_HOME}
%%__elf_exclude_path     ^%{OHPC_HOME}

%%__ohpc_magic           ^ELF (32|64)-bit.*$
%%__ohpc_flags           magic_and_path
%if 0%{?sle_version} || 0%{?suse_version}
%%__elflib_exclude_path  ^%{OHPC_HOME}
%endif
EOF


%files
%dir %{OHPC_HOME}
%dir %{OHPC_ADMIN}
%{OHPC_PUB}


#######################################

%package -n %{PROJ_NAME}-buildroot
Summary: Common build scripts used in OpenHPC packaging
Group: %{PROJ_NAME}/admin
Requires: lmod%{PROJ_DELIM}
Requires: %{PROJ_NAME}-filesystem

%description -n %{PROJ_NAME}-buildroot

This administrative package is used to provide RPM dependency analysis tools
and common compiler and MPI family convenience scripts used during OpenHPC
builds.


%files -n %{PROJ_NAME}-buildroot
%{OHPC_ADMIN}/ohpc
/usr/lib/rpm/ohpc-find-provides
/usr/lib/rpm/ohpc-find-requires
/usr/lib/rpm/fileattrs/ohpc.attr
