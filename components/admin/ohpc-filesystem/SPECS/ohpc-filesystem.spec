Name: ohpc-filesystem
Version: 1
Release: 2.ohpc
Summary: Common openHPC directories

Group: ohpc/admin
License: ASL 2.0
Source0: OHPC_setup_compiler
Source1: OHPC_setup_mpi

BuildArch: noarch

%description
This package own the top level common directories and should be required
by every openHPC package.

%package -n ohpc-buildroot
Summary: Common scripts to build openHPC packages
Group: ohpc/admin
Requires: lmod-ohpc
Requires: ohpc-filesystem

%description -n ohpc-buildroot
Common scripts to build openHPC packages.

%install
# The ohpc-filesystems owns all the common directories
mkdir -p $RPM_BUILD_ROOT/opt/ohpc/pub/{apps,doc,compiler,libs,moduledeps,modulefiles,mpi}
mkdir -p $RPM_BUILD_ROOT/opt/ohpc/admin/ohpc
install -p -m 644 %{SOURCE0} $RPM_BUILD_ROOT/opt/ohpc/admin/ohpc
install -p -m 644 %{SOURCE1} $RPM_BUILD_ROOT/opt/ohpc/admin/ohpc

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
/opt/ohpc/admin/ohpc/OHPC_setup_compiler
/opt/ohpc/admin/ohpc/OHPC_setup_mpi

%changelog
* Wed Feb 08 2017 Adrian Reber <areber@redhat.com> - 1.2
- Initial release
