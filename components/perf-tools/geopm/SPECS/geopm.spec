#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is is dependent on compiler toolchain and MPI
%global ohpc_compiler_dependent 1
%global ohpc_mpi_dependent 1
%global ohpc_python_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%global pname geopm
%define svcpkg %{pname}-service%{PROJ_DELIM}

Name:          %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Summary:       Global Extensible Open Power Manager
Version:       2.0.1
Release:       1
License:       BSD-3-Clause
Group:         %{PROJ_NAME}/perf-tools
URL:           https://geopm.github.io
Source0:       https://github.com/geopm/geopm/releases/download/v%{version}/geopm-%{version}.tar.gz
Source1:       https://github.com/geopm/geopm/releases/download/v%{version}/geopm-service-%{version}.tar.gz

# Install paths
%global install_path  %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version
%global module_path  %{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}

Requires:      %{svcpkg} = %{version}

BuildRequires: autoconf
BuildRequires: automake
BuildRequires: make
BuildRequires: gcc-c++
BuildRequires: systemd-devel >= 239-29
BuildRequires: libtool
BuildRequires: libtool-ltdl-devel
BuildRequires: unzip
BuildRequires: sqlite-devel

%if 0%{?suse_version} || 0%{?sle_version}
Buildrequires: libelf-devel
BuildRequires: fdupes
BuildRequires: systemd-rpm-macros
%if 0%{?suse_version} >= 1320 || 0%{?sle_version} >= 132000
BuildRequires: openssh
%endif
%else
BuildRequires: elfutils-libelf-devel
%endif

%description
The Global Extensible Open Power Manager (GEOPM) is a framework for
exploring power and energy optimizations on heterogeneous platforms.


%prep
%setup -q -n %{pname}-%{version} -a 1
ln -s geopm-service-%{version} service


%build
# Build the GEOPM Service
pushd service
./autogen.sh

CFLAGS= CXXFLAGS= CC=gcc CXX=g++ \
./configure --prefix="/usr" \
            --libdir=%{_libdir} \
            --libexecdir=%{_libexecdir} \
            --includedir=%{_includedir} \
            --sbindir=%{_sbindir} \
            --mandir=%{_mandir} \
            --with-python=%{__python3} \
            --bindir=%{_bindir} \
            || ( cat config.log && false )

CFLAGS= CXXFLAGS= CC=gcc CXX=g++ \
make %{?_smp_mflags}
popd

# Build the GEOPM Framework
%ohpc_setup_compiler

CFLAGS="$CFLAGS -Wno-error=stringop-truncation" \
./autogen.sh

test -f configure || ./autogen.sh
./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --with-python=%{__python3} \
            || ( cat config.log && false )

make %{?_smp_mflags}


%install
# Install the GEOPM Service
pushd service
make DESTDIR=%{buildroot} install
install -Dp -m644 geopm.service %{buildroot}%{_unitdir}/geopm.service
install -Dp -m644 io.github.geopm.conf %{buildroot}%{_datadir}/dbus-1/system.d/io.github.geopm.conf
install -Dp -m644 io.github.geopm.xml %{buildroot}%{_datadir}/dbus-1/interfaces/io.github.geopm.xml
mkdir -p %{buildroot}%{_sysconfdir}/geopm-service
mkdir -p %{buildroot}%{_sbindir}
rm -f %{buildroot}%{_datadir}/doc/geopm-service/*
ln -s /usr/sbin/service %{buildroot}%{_sbindir}/rcgeopm
popd

# Install the GEOPM Framework
%ohpc_setup_compiler

make DESTDIR=%{buildroot} install

%if 0%{?suse_version} || 0%{?sle_version}
%python_expand %fdupes %{buildroot}%{python3_sitelib}
%endif

find %{buildroot}/ \( -name '*.a' -o -name '*.la' \
     -o -name 'geopm_launcher.1*' \) -print -exec rm -f {} \;
rm -f %{buildroot}/%{_mandir}/man1/geopmbench.1
rm -f %{buildroot}/%{_mandir}/man1/geopmctl.1
rm -f %{buildroot}/%{_mandir}/man1/geopmlaunch.1
rm -f %{buildroot}/%{_mandir}/man3/geopm_fortran.3
rm -f %{buildroot}/%{_mandir}/man3/geopm_ctl.3
rm -f %{buildroot}/%{_includedir}/geopm_ctl.h
rm -f %{buildroot}/%{_bindir}/geopmlaunch

# OpenHPC module file
mkdir -p %{buildroot}/%{module_path}
cat << EOF > %{buildroot}/%{module_path}/%{version}.lua
local version="%{version}"
help([[
This module loads the %{pname} library built with the %{compiler_family}
compiler toolchain and the %{mpi_family} MPI stack.
Version ]] .. version .. [[
]])

whatis("Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI")
whatis("Version: %{version}")
whatis("Category: runtime")
whatis("Description: %{summary}")
whatis("URL: %{url}")

prepend_path("PATH",            "%{install_path}/bin")
prepend_path("PYTHONPATH",      "%{install_path}/lib/python%{python3_version}/site-packages")
prepend_path("INCLUDE",         "%{install_path}/include")
prepend_path("LD_LIBRARY_PATH", "%{install_path}/lib")
prepend_path("MANPATH",         "%{install_path}/share/man")

setenv("%{PNAME}_DIR", "%{install_path}")
setenv("%{PNAME}_BIN", "%{install_path}/bin")
setenv("%{PNAME}_LIB", "%{install_path}/lib")
setenv("%{PNAME}_INC", "%{install_path}/include")

EOF

ln -s %{version} %{buildroot}/%{module_path}/default.lua


%files
%{install_path}
%{module_path}
%doc README VERSION
%license COPYING COPYING-TPP


##########################################################################
%package -n %{svcpkg}
Summary:       Global Extensible Open Power Manager Service

Requires:      systemd
Requires:      python3
Requires:      python3-psutil
Requires:      python3-cffi
Requires:      python3-jsonschema
Requires:      python3-dasbus >= 1.5

%Description -n %{svcpkg}
Linux systemd service with DBus interface for user-level access to hardware
features on heterogeneous systems.


%if 0%{?suse_version} || 0%{?sle_version}
%pre -n %{svcpkg}
%service_add_pre geopm.service
%endif


%post -n %{svcpkg}
/usr/bin/getent group geopm >/dev/null 2>&1 || groupadd -r geopm
%if 0%{?rhel}
%systemd_post geopm.service
%else
%service_add_post geopm.service
%endif


%preun -n %{svcpkg}
%if 0%{?rhel}
%systemd_preun geopm.service
%else
%service_del_preun geopm.service
%endif


%postun -n %{svcpkg}
/sbin/ldconfig
%if 0%{?rhel}
%systemd_postun_with_restart geopm.service
%else
%service_del_postun geopm.service
%endif


%files -n %{svcpkg}
%defattr(-,root,root,-)
%{_sbindir}/rcgeopm
%{_datadir}/dbus-1/system.d/io.github.geopm.conf
%{_datadir}/dbus-1/interfaces/io.github.geopm.xml
%{_unitdir}/geopm.service
%{_bindir}/geopmread
%{_bindir}/geopmwrite
%{_bindir}/geopmd
%{_bindir}/geopmaccess
%{_bindir}/geopmsession
%{_includedir}/geopm*
%{_libdir}/libgeopmd.so
%{python3_sitelib}/geopmdpy*
%doc %{_mandir}/man3/geopm*
%doc %{_mandir}/man1/geopm*
%doc %{_mandir}/man7/geopm*
%dir %{_libdir}/geopm
%{_libdir}/libgeopmd.so.1.0.0
%{_libdir}/libgeopmd.so.1
%doc service/README.rst
%doc service/VERSION
%doc service/AUTHORS
%license service/COPYING
%license service/COPYING-TPP
