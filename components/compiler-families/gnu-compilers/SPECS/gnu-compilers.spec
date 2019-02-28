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

%global gnu_version 8.3.0
%global gnu_major_ver 8
%global gnu_release 3
%global pname gnu8-compilers

# Define subcomponent versions required for build

%global gmp_version 6.1.2
%global mpc_version 1.0.3
%global mpfr_version 3.1.6

Summary:   The GNU C Compiler and Support Files
Name:      %{pname}%{PROJ_DELIM}
Version:   %{gnu_version}
Release:   %{gnu_release}%{?dist}
License:   GNU GPL
Group:     %{PROJ_NAME}/compiler-families
URL:       http://gcc.gnu.org/
Source0:   https://ftp.gnu.org/gnu/gcc/gcc-%{gnu_version}/gcc-%{gnu_version}.tar.xz
Source1:   https://ftp.gnu.org/gnu/gmp/gmp-%{gmp_version}.tar.bz2
Source2:   https://ftp.gnu.org/gnu/mpc/mpc-%{mpc_version}.tar.gz
Source3:   https://ftp.gnu.org/gnu/mpfr/mpfr-%{mpfr_version}.tar.gz

BuildRequires:  bison
BuildRequires:  flex
BuildRequires:  gettext-devel
BuildRequires:  perl
BuildRequires:  gcc-c++
%if 0%{?suse_version} > 1220
BuildRequires:  makeinfo
%else
BuildRequires:  texinfo
%endif
BuildRequires:  zlib-devel
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
%endif
Requires: glibc-devel


%define install_path %{OHPC_COMPILERS}/gcc/%{version}

%description

Core package for the GNU Compiler Collection, including the C language
frontend.

%prep
%setup -q -n gcc-%{version} -a1 -a2 -a3

ln -s gmp-%{gmp_version} gmp
ln -s mpc-%{mpc_version} mpc
ln -s mpfr-%{mpfr_version} mpfr

%build

%{__mkdir} obj
cd obj
../configure --disable-multilib --enable-languages="c,c++,fortran"  --prefix=%{install_path} --disable-static --enable-shared
make %{?_smp_mflags}
%install
cd obj
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/include
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/lib
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/install-tools
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/share
%endif


# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/gnu%{gnu_major_ver}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/gnu%{gnu_major_ver}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the GNU compiler collection"
puts stderr " "
puts stderr "See the man pages for gcc, g++, and gfortran for detailed information"
puts stderr "on available compiler options and command-line syntax."
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: GNU Compiler Collection"
module-whatis "Version: %{version}"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: GNU Compiler Family (C/C++/Fortran for x86_64)"
module-whatis "URL: http://gcc.gnu.org/"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib64
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/%{compiler_family}

family "compiler"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/gnu%{gnu_major_ver}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_MODULES}/gnu%{gnu_major_ver}/
%dir %{OHPC_COMPILERS}/gcc
%{install_path}
%doc COPYING
%doc COPYING3
%doc COPYING3.LIB
%doc README
%doc ChangeLog.tree-ssa
%doc ChangeLog
%doc COPYING.LIB
%doc COPYING.RUNTIME
%if "%{compiler_family}" != "gnu7"
%doc NEWS
%endif
