#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#-fsp-header-comp-begin----------------------------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
%{!?PROJ_DELIM:      %define PROJ_DELIM   %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM}
%if %{compiler_family} == gnu
BuildRequires: gcc-c++
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

# MPI dependencies
#%if %{mpi_family} == impi
#BuildRequires: intel-mpi-devel%{PROJ_DELIM}
#Requires:      intel-mpi-devel%{PROJ_DELIM}
#%endif
#%if %{mpi_family} == mvapich2
#BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
#Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
#%endif
#%if %{mpi_family} == openmpi
#BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
#Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
#%endif

#-fsp-header-comp-end------------------------------------------------


# don't stop with an error if we don't pack all files at once
#%define _unpackaged_files_terminate_build  0

#
# System versions
#
%if 0%{?suse_version} > 1220
%define uses_systemd 1

%else
%if 0%{?el7}
%define uses_systemd 1

%else
%define uses_systemd 0

%endif
%endif


#
# global Open RCM stuff
#
Prefix: /opt/openrcm
%define orcm_name openrcm
%define orcm_version 0.13.0
%{!?orcm_package_version:%define orcm_package_version default}
%define orcm_extra_version %{nil}
%define orcm_release 1
%define orcm_prefix  /opt/%{orcm_name}
%define orcm_build_root %{_tmppath}/%{orcm_name}-%{orcm_version}-%{orcm_release}-root
%define orcm_source %{orcm_name}-%{orcm_version}-d659794.tar.gz
%define orcm_url https://www.open-mpi.org/projects/orcm/
%define orcm_specfile %{_topdir}/SOURCES/%{orcm_name}.spec
%{!?configure_options: %define configure_options  %{nil}}
%if 0%{?suse_version} > 1220
%define configure_options --with-platform=contrib/platform/intel/hillsboro/orcm-linux --with-postgres=/usr/include/pgsql
%else
%define configure_options --with-platform=contrib/platform/intel/hillsboro/orcm-linux --with-postgres=/usr/pgsql-9.3
%endif
%define orcm_configure_params  %{nil}
%define orcm_compile_root %{orcm_name}-%{orcm_version}


#
# fix configure 
#
%define _prefix %{orcm_prefix}
%define _sysconfdir %{_prefix}/etc
%define _libdir %{_prefix}/lib64
%define _includedir %{_prefix}/include
%define _mandir %{_prefix}/share/man
%define _pkgdatadir %{_prefix}/share/openmpi


#
# compiler settings
#
%define orcm_compiler default
%define orcm_cc  " "
%define orcm_cxx " "


######################################################################
#
# Build section
#
######################################################################
Summary: Configure and build the Open RCM tree
Name: %{orcm_name}%{PROJ_DELIM}
Version: %{orcm_version}
Release: %{orcm_release}
License: BSD
Group: fsp/admin
URL: %{orcm_url}
Source0: %{orcm_source}
BuildRoot: %{orcm_build_root}
%if %{uses_systemd}
BuildRequires:  pkgconfig(systemd)
%{?systemd_requires}
%endif
BuildRequires: ipmiutil-devel%{PROJ_DELIM} >= 2.9.5
BuildRequires: libtool%{PROJ_DELIM}
%if 0%{?el6}
BuildRequires: libtool-ltdl
%endif 
BuildRequires: sigar%{PROJ_DELIM} 
#>= 1.6.4
BuildRequires: sigar-devel%{PROJ_DELIM} 
#>= 1.6.4
## john.a.westlund@intel.com addition
BuildRequires: flex
%if 0%{?sles_version}
BuildRequires: libopenssl-devel
BuildRequires: postgresql93
BuildRequires: postgresql93-devel
%else
BuildRequires: openssl-devel
BuildRequires: postgresql
BuildRequires: postgresql-devel
%endif
Requires:      openssl
##

Requires: ipmiutil-devel%{PROJ_DELIM} >= 2.9.5
Requires: sigar%{PROJ_DELIM} >= 1.6.4

%description
This part build and install the Open RCM source tree.

%prep
%setup -q -n %{orcm_name}-%{orcm_version}

%build
./autogen.pl
ORCM_CONFIGURE_FLAGS="%{orcm_configure_params}"
ORCM_CONFIGURE_OPTIONS="%{configure_options}"
if [ "%{orcm_compiler}" != "default" ]; then
ORCM_CONFIGURE_FLAGS="$ORCM_CONFIGURE_FLAGS CC=%{orcm_cc} CXX=%{orcm_cxx}"
fi

%configure $ORCM_CONFIGURE_FLAGS $ORCM_CONFIGURE_OPTIONS
make -j4

%install
make DESTDIR=$RPM_BUILD_ROOT install

%if %{uses_systemd}
install -D -m 0644 contrib/dist/linux/orcmd.service %{buildroot}%{_unitdir}/orcmd.service
install -D -m 0644 contrib/dist/linux/orcmsched.service %{buildroot}%{_unitdir}/orcmsched.service
%else
install -D -m 0755 contrib/dist/linux/orcmd.init %{buildroot}%{_initddir}/orcmd
install -D -m 0755 contrib/dist/linux/orcmsched.init %{buildroot}%{_initddir}/orcmsched
%endif
install -D -m 0644 contrib/dist/linux/orcmd.sysconfig %{buildroot}/etc/sysconfig/orcmd
install -D -m 0644 contrib/dist/linux/orcmsched.sysconfig %{buildroot}/etc/sysconfig/orcmsched
install -D -m 0644 contrib/database/orcmdb_psql.ini %{buildroot}%{_sysconfdir}/orcmdb_psql.ini
install -D -m 0644 contrib/database/orcmdb_psql.sql %{buildroot}%{_sysconfdir}/orcmdb_psql.sql
install -D -m 0644 contrib/database/psql_odbc_driver.ini %{buildroot}%{_sysconfdir}/psql_odbc_driver.ini

#
# create a module file on request
#
if [ 1 == 0 ] ; then 
%{__mkdir_p} $RPM_BUILD_ROOT/nirwana/%{orcm_name}/
cat <<EOF >$RPM_BUILD_ROOT/nirwana/%{orcm_name}/%{orcm_version}
#%Module

# NOTE: This is an automatically-generated file!  (generated by the
# Open RCM RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

proc ModulesHelp { } {
   puts stderr "This module adds Open RCM to various paths"
}

module-whatis   "Sets up Open RCM in your enviornment"

append-path PATH "%{_prefix}/bin/"
append-path LD_LIBRARY_PATH %{_libdir}
append-path MANPATH %{_mandir}
EOF
fi


#
# profile.d files
#
if [ 1 == 0 ] ; then
%{__mkdir_p} $RPM_BUILD_ROOT/etc/profile.d/
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{orcm_name}-%{orcm_version}.sh
# NOTE: This is an automatically-generated file!  (generated by the
# Open RCM RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

CHANGED=0
if test -z "`echo $PATH | grep %{_prefix}/bin`"; then
    PATH=\${PATH}:%{_prefix}/bin/
    CHANGED=1
fi
if test -z "`echo $LD_LIBRARY_PATH | grep %{_libdir}`"; then
    LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:%{_libdir}
    CHANGED=1
fi
if test -z "`echo $MANPATH | grep %{_mandir}`"; then
    MANPATH=\${MANPATH}:%{_mandir}
    CHANGED=1
fi
if test "$CHANGED" = "1"; then
    export PATH LD_LIBRARY_PATH MANPATH
fi
EOF

cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{orcm_name}-%{orcm_version}s.csh
# NOTE: This is an automatically-generated file!  (generated by the
# Open RCM RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

if ("`echo $PATH | grep %{_prefix}/bin`") then
    setenv PATH \${PATH}:%{_prefix}/bin/
endif
if ("$?LD_LIBRARY_PATH") then
    if ("`echo $LD_LIBRARY_PATH | grep %{_libdir}`") then
        setenv LD_LIBRARY_PATH \${LD_LIBRARY_PATH}:%{_libdir}
    endif
endif
if ("$?MANPATH") then
    if ("`echo $MANPATH | grep %{_mandir}`") then
        setenv MANPATH \${MANPATH}:%{_mandir}
    endif
endif
EOF
fi


%clean

%files 

%defattr(-,root,root,-)
%config %{_sysconfdir}/*
%config /etc/sysconfig/*
%{_prefix}/bin/*
%{_libdir}/*
%{_includedir}/*
%doc %{_pkgdatadir}/*
%doc %{_mandir}/man1/*
%doc %{_mandir}/man7/*
%if %{uses_systemd}
%{_unitdir}/*
%else
%{_initddir}/*
%endif

#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Wed Dec 10 2014 Ben McClelland <benjamin.m.mcclelland@intel.com>
- convert ompi spec to orcm

* Tue Jun 27 2006 Sven Stork <stork@hlrs.de>
- switch to specfile generator

* Wed Apr 26 2006 Jeff Squyres <jsquyres@cisco.com>
- Revamp files listings to ensure that rpm -e will remove directories
  if rpm -i created them.
- Simplify options for making modulefiles and profile.d scripts.
- Add oscar define.
- Ensure to remove the previous installation root during prep.
- Cleanup the modulefile specification and installation; also ensure
  that the profile.d scripts get installed if selected.
- Ensure to list sysconfdir in the files list if it's outside of the
  prefix.

* Thu Mar 30 2006 Jeff Squyres <jsquyres@cisco.com>
- Lots of bit rot updates
- Reorganize and rename the subpackages
- Add / formalize a variety of rpmbuild --define options
- Comment out the docs subpackage for the moment (until we have some
  documentation -- coming in v1.1!)

* Tue May 03 2005 Jeff Squyres <jsquyres@open-mpi.org>
- Added some defines for LANL defaults
- Added more defines for granulatirty of installation location for
  modulefile
- Differentiate between installing in /opt and whether we want to
  install environment script files
- Filled in files for man and mca-general subpackages

* Thu Apr 07 2005 Greg Kurtzer <GMKurtzer@lbl.gov>
- Added opt building
- Added profile.d/modulefile logic and creation
- Minor cleanups

* Fri Apr 01 2005 Greg Kurtzer <GMKurtzer@lbl.gov>
- Added comments
- Split package into subpackages
- Cleaned things up a bit
- Sold the code to Microsoft, and now I am retiring. Thanks guys!

* Wed Mar 23 2005 Mezzanine <mezzanine@kainx.org>
- Specfile auto-generated by Mezzanine



