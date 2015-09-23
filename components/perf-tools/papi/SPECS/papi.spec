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

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family variable via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%if 0%{?OHPC_build}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end------------------------------------------------

# Base package name
%define pname papi
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Summary: Performance Application Programming Interface
Name: %{pname}%{PROJ_DELIM}
Version: 5.4.1
Release: 1%{?dist}
License: BSD
Group: ohpc/perf-tools
URL: http://icl.cs.utk.edu/papi/
Source0: %{pname}-%{version}.tar.gz
Patch1: papi.ldconfig.patch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
DocDir: %{OHPC_PUB}/doc/contrib
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
%global debug_package %{nil} 

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
PAPI provides a programmer interface to monitor the performance of
running programs.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build

# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

cd src
./configure --with-static-lib=no --with-shared-lib=yes --with-shlib --prefix=%{install_path}
#DBG workaround to make sure libpfm just uses the normal CFLAGS
DBG="" make

#%check
#cd src
#make fulltest

%install

# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

rm -rf $RPM_BUILD_ROOT
cd src
#sudo make DESTDIR=$RPM_BUILD_ROOT install
#sudo chown -R abuild $RPM_BUILD_ROOT

make DESTDIR=$RPM_BUILD_ROOT install

#chown -R abuild $RPM_BUILD_ROOT

# if !0%{?suse_version}
# chrpath --delete $RPM_BUILD_ROOT%{_libdir}/*.so*
# endif

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

    puts stderr " "
    puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
    puts stderr "toolchain."
    puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

# Remove the static libraries. Static libraries are undesirable:
# https://fedoraproject.org/wiki/Packaging/Guidelines#Packaging_Static_Libraries
rm -rf $RPM_BUILD_ROOT%{_libdir}/*.a

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig
%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc ChangeLog*.txt INSTALL.txt LICENSE.txt README RELEASENOTES.txt

%changelog
