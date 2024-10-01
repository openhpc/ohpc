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

%define pname genders

Name:    %{pname}%{PROJ_DELIM}
Version: 1.29
Release: 1%{?dist}
Summary: Static cluster configuration database
License: GPL
Source0:  https://github.com/chaos/genders/archive/genders-1-29-1.tar.gz
Group:   %{PROJ_NAME}/admin
URL:     https://github.com/chaos/genders

Requires: perl
BuildRequires: gcc-c++ make
BuildRequires: bison flex
BuildRequires: perl(ExtUtils::MakeMaker)
BuildRequires: python3
BuildRequires: python3-devel

%if 0%{?rhel_version} || 0%{?openEuler}
BuildRequires: byacc
%endif

%if 0%{?openEuler}
BuildRequires: perl-devel
%endif

Provides: %{pname} = %{version}

%description
Genders is a static cluster configuration database used for cluster
configuration management.  It is used by a variety of tools and
scripts for management of large clusters.  The genders database is
typically replicated on every node of the cluster. It describes the
layout and configuration of the cluster so that tools and scripts can
sense the variations of cluster nodes. By abstracting this information
into a plain text file, it becomes possible to change the
configuration of a cluster by modifying only one file.

%package -n %{pname}-compat%{PROJ_DELIM}
Summary: Compatibility Library
Group: System Environment/Base
%description -n %{pname}-compat%{PROJ_DELIM}
genders API that is compatible with earlier releases of genders

%{!?_with_perl_extensions: %{!?_without_perl_extensions: %global _with_perl_extensions --with-perl-extensions}}
%{!?_with_python_extensions: %{!?_without_python_extensions: %global _with_python_extensions --with-python-extensions}}
%{!?_with_cplusplus_extensions: %{!?_without_cplusplus_extensions: %global _with_cplusplus_extensions --with-cplusplus-extensions}}

# choose vendor arch by default
%{!?_with_perl_site_arch: %{!?_with_perl_vendor_arch: %global _with_perl_vendor_arch --with-perl-vendor-arch}}

%prep
%setup  -q -n %{pname}-%{pname}-1-29-1

%build

%configure PYTHON=/usr/bin/python3 --program-prefix=%{?_program_prefix:%{_program_prefix}} \
    --with-extension-destdir="$RPM_BUILD_ROOT" \
    %{?_with_perl_extensions} \
    %{?_without_perl_extensions} \
    %{?_with_perl_site_arch} \
    %{?_without_perl_site_arch} \
    %{?_with_perl_vendor_arch} \
    %{?_without_perl_vendor_arch} \
    %{?_with_python_extensions} \
    %{?_without_python_extensions} \
    %{?_with_cplusplus_extensions} \
    %{?_without_cplusplus_extensions} \
    --without-java-extensions
make

%install
DESTDIR="$RPM_BUILD_ROOT" make install

find "$RPM_BUILD_ROOT" -name .packlist -exec sed -i "s#$RPM_BUILD_ROOT##" {} \;
find "$RPM_BUILD_ROOT" -name .packlist -exec sed -i '/BUILDROOT/d'        {} \;

%if %{?_with_perl_extensions:1}%{!?_with_perl_extensions:0}
%define _perldir %(perl -e 'use Config; $T=$Config{installvendorarch}; $P=$Config{vendorprefix}; $T=~/$P\\/(.*)/; print "%{_prefix}/$1\\n"')
chmod 755 ${RPM_BUILD_ROOT}/%{_perldir}/auto/Libgenders/Libgenders.so
%endif

# turn off rpath check... causes failure on libgenders library
export NO_BRP_CHECK_RPATH=true

%files
%doc README NEWS ChangeLog DISCLAIMER DISCLAIMER.UC COPYING TUTORIAL genders.sample
# It doesn't matter if the user chooses a 32bit or 64bit target.  The
# packaging must work off whatever Perl is installed.
%if %{?_with_perl_site_arch:1}%{!?_with_perl_site_arch:0}
%define _perldir %(perl -e 'use Config; $T=$Config{installsitearch}; $P=$Config{siteprefix}; $T=~/$P\\/(.*)/; print "%{_prefix}/$1\\n"')
%endif
%if %{?_with_perl_vendor_arch:1}%{!?_with_perl_vendor_arch:0}
%define _perldir %(perl -e 'use Config; $T=$Config{installvendorarch}; $P=$Config{vendorprefix}; $T=~/$P\\/(.*)/; print "%{_prefix}/$1\\n"')
%endif
%{_mandir}/man1/*
%{_mandir}/man3/genders*
%{_mandir}/man3/libgenders*
%{_includedir}/*
%{_bindir}/*
%{_libdir}/libgenders.*
%if %{?_with_perl_extensions:1}%{!?_with_perl_extensions:0}
%{_mandir}/man3/Libgenders*
%{_mandir}/man3/Genders*
%{_perldir}/*
%endif
%if %{?_with_python_extensions:1}%{!?_with_python_extensions:0}
%{_libdir}/python*
%endif
%if %{?_with_cplusplus_extensions:1}%{!?_with_cplusplus_extensions:0}
%{_libdir}/libgendersplusplus.*
%endif

%files -n %{pname}-compat%{PROJ_DELIM}
%{_mandir}/man3/gendlib*
%dir %{_prefix}/lib/genders
%{_prefix}/lib/genders/*
