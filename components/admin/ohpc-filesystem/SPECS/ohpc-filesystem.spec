Name: ohpc-filesystem
Version: 1.3
Release: 1.ohpc
Summary: Common top-level OpenHPC directories

Group: ohpc/admin
License: ASL 2.0
Source0: OHPC_setup_compiler
Source1: OHPC_setup_mpi

BuildArch: noarch

%description
This administrative package is used to define top level OpenHPC installation
directories and is utilized by most packages that do not install into system
default paths.

%package -n ohpc-buildroot
Summary: Common build scripts used in OpenHPC packaging
Group: ohpc/admin
Requires: lmod-ohpc
Requires: ohpc-filesystem

%description -n ohpc-buildroot
Common compiler and MPI family convenience scripts used during OpenHPC builds.

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
* Mon May  8 2017 Karl W Schulz <karl.w.schulz@intel.com> - 1.3
- minor description updates

* Wed Feb 08 2017 Adrian Reber <areber@redhat.com> - 1.2
- Initial release
