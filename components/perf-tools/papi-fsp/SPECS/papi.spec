%include %{_sourcedir}/FSP_macros
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Base package name
%define pname papi
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Summary: Performance Application Programming Interface
Name: %{pname}%{PROJ_DELIM}
Version: 5.4.0
Release: 1%{?dist}
License: BSD
Group: Development/System
URL: http://icl.cs.utk.edu/papi/
Source0: %{pname}-%{version}.tar.gz
Patch1: papi.ldconfig.patch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: ncurses-devel
%if 0%{?suse_version}
BuildRequires: gcc-fortran
%else
BuildRequires: gcc-gfortran
BuildRequires: chrpath
%endif
BuildRequires: kernel-headers >= 2.6.32
#Right now libpfm does not know anything about s390 and will fail
ExcludeArch: s390 s390x

%description
PAPI provides a programmer interface to monitor the performance of
running programs.

%package devel
Summary: Header files for the compiling programs with PAPI
Group: Development/System
Requires: papi = %{version}-%{release}
%description devel
PAPI-devel includes the C header files that specify the PAPI userspace
libraries and interfaces. This is required for rebuilding any program
that uses PAPI.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build
cd src
%configure --with-static-lib=no --with-shared-lib=yes --with-shlib
#DBG workaround to make sure libpfm just uses the normal CFLAGS
DBG="" make

#%check
#cd src
#make fulltest

%install
rm -rf $RPM_BUILD_ROOT
cd src
sudo make DESTDIR=$RPM_BUILD_ROOT install
sudo chown -R abuild $RPM_BUILD_ROOT

%if !0%{?suse_version}
chrpath --delete $RPM_BUILD_ROOT%{_libdir}/*.so*
%endif

# Remove the static libraries. Static libraries are undesirable:
# https://fedoraproject.org/wiki/Packaging/Guidelines#Packaging_Static_Libraries
rm -rf $RPM_BUILD_ROOT%{_libdir}/*.a

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig
%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_bindir}/*
%{_libdir}/*.so.*
/usr/share/papi
%doc INSTALL.txt README LICENSE.txt RELEASENOTES.txt

%files devel
%defattr(-,root,root,-)
%{_includedir}/*.h
%{_includedir}/perfmon
%{_libdir}/*.so
%doc %{_mandir}/man3/*
%doc %{_mandir}/man1/*

%changelog
