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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

Name:    %{pname}%{PROJ_DELIM}
Version: 1.22
Release: 1%{?dist}
Summary: Static cluster configuration database
Group: System Environment/Base
License: GPL
Source: https://github.com/chaos/genders/releases/download/genders-1-22-1/%{pname}-%{version}.tar.gz
Requires: perl
DocDir:    %{OHPC_PUB}/doc/contrib
Group:     %{PROJ_NAME}/admin
URL: https://github.com/chaos/genders
BuildRequires: gcc-c++
BuildRequires: bison flex
BuildRequires: perl(ExtUtils::MakeMaker)
BuildRequires: python
BuildRequires: python-devel
BuildRoot: %{_tmppath}/%{pname}-%{version}
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

%{!?_with_perl_extensions: %{!?_without_perl_extensions: %define _with_perl_extensions --with-perl-extensions}}
%{!?_with_python_extensions: %{!?_without_python_extensions: %define _with_python_extensions --with-python-extensions}}
%{!?_with_cplusplus_extensions: %{!?_without_cplusplus_extensions: %define _with_cplusplus_extensions --with-cplusplus-extensions}}

# choose vendor arch by default
%{!?_with_perl_site_arch: %{!?_with_perl_vendor_arch: %define _with_perl_vendor_arch --with-perl-vendor-arch}}

%prep
%setup  -q -n %{pname}-%{version}

%build

%configure --program-prefix=%{?_program_prefix:%{_program_prefix}} \
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
rm -rf $RPM_BUILD_ROOT
DESTDIR="$RPM_BUILD_ROOT" make install 

find "$RPM_BUILD_ROOT" -name .packlist -exec sed -i "s#$RPM_BUILD_ROOT##" {} \;
find "$RPM_BUILD_ROOT" -name .packlist -exec sed -i '/BUILDROOT/d'        {} \;

# turn off rpath check... causes failure on libgenders library
export NO_BRP_CHECK_RPATH=true

%post
if [ -x /sbin/ldconfig ]; then /sbin/ldconfig %{_libdir}; fi

%postun
if [ -x /sbin/ldconfig ]; then /sbin/ldconfig %{_libdir}; fi

%files
%defattr(-,root,root)
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
%defattr(-,root,root)
%{_mandir}/man3/gendlib*
%dir %{_prefix}/lib/genders
%{_prefix}/lib/genders/*
