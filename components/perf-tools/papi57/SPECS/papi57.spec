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

# Base package name
%define pname papi

Summary:   Performance Application Programming Interface
Name:      %{pname}57%{PROJ_DELIM}
Version:   5.7.0
Release:   1%{?dist}
License:   BSD
Group:     %{PROJ_NAME}/perf-tools
URL:       http://icl.cs.utk.edu/papi/
Source0:   http://icl.cs.utk.edu/projects/papi/downloads/papi-%{version}.tar.gz
Patch1:    papi.ldconfig.patch

BuildRequires: ncurses-devel make
%if 0%{?sle_version}
BuildRequires: gcc-fortran
%else
BuildRequires: gcc-gfortran
BuildRequires: chrpath
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
PAPI provides a programmer interface to monitor the performance of
running programs.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build

cd src
CFLAGS="-fPIC -DPIC" CXXFLAGS="-fPIC -DPIC" FCFLAGS="-fPIC" ./configure --with-static-lib=yes --with-shared-lib=yes --with-shlib --prefix=%{install_path}
#DBG workaround to make sure libpfm just uses the normal CFLAGS
DBG="" CFLAGS="-fPIC -DPIC" CXXFLAGS="-fPIC -DPIC" FCFLAGS="-fPIC" make

%install
cd src

make DESTDIR=$RPM_BUILD_ROOT install

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

    puts stderr " "
    puts stderr "This module loads the %{pname} library toolchain."
    puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
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

%ifarch x86_64
rm -rf $RPM_BUILD_ROOT/%{install_path}/lib/*.a
%endif
rm -rf $RPM_BUILD_ROOT%{_libdir}/*.la

%{__mkdir_p} $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc ChangeLog*.txt INSTALL.txt LICENSE.txt README.md RELEASENOTES.txt
