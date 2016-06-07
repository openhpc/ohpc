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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define pname gnu-compilers

# Define subcomponent versions required for build

%define gmp_version 6.1.0
%define mpc_version 1.0.3
%define mpfr_version 3.1.3

Summary:   The GNU C Compiler and Support Files
Name:      %{pname}%{PROJ_DELIM}
Version:   5.3.0
Release:   1
License:   GNU GPL
Group:     %{PROJ_NAME}/compiler-families
URL:       http://gcc.gnu.org/
DocDir:    %{OHPC_PUB}/doc/contrib
Source0:   https://ftp.gnu.org/gnu/gcc/gcc-%{version}/gcc-%{version}.tar.bz2
Source1:   https://ftp.gnu.org/gnu/gmp/gmp-%{gmp_version}.tar.bz2
Source2:   ftp://ftp.gnu.org/gnu/mpc/mpc-%{mpc_version}.tar.gz
#Source3:   http://www.mpfr.org/mpfr-current/mpfr-%{mpfr_version}.tar.gz
Source3:   http://ftp.gnu.org/gnu/mpfr/mpfr-%{mpfr_version}.tar.gz
Source4:   OHPC_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

%define debug_package %{nil}

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


%define install_path %{OHPC_COMPILERS}/gcc/%{version}

%description

Core package for the GNU Compiler Collection, including the C language
frontend.

%prep
%setup -n gcc-%{version}
%setup -n gcc-%{version} -T -D -a 1
%setup -n gcc-%{version} -T -D -a 2
%setup -n gcc-%{version} -T -D -a 3

ln -s gmp-%{gmp_version} gmp
ln -s mpc-%{mpc_version} mpc
ln -s mpfr-%{mpfr_version} mpfr

%build

%{__mkdir} obj
cd obj
../configure --disable-multilib --enable-languages="c,c++,fortran"  --prefix=%{install_path}

%install
cd obj
make %{?_smp_mflags} 
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/include
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/lib
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/install-tools
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/share
%endif

# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/gnu
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/gnu/%{version}
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
prepend-path    MODULEPATH          %{OHPC_MODULEDEPS}/gnu

family "compiler"
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/gnu/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%post
/sbin/ldconfig

%postun
/sbin/ldconfig


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc COPYING
%doc COPYING3
%doc NEWS
%doc COPYING3.LIB
%doc README
%doc ChangeLog.tree-ssa
%doc ChangeLog
%doc COPYING.LIB
%doc COPYING.RUNTIME


%changelog
* Tue Aug  5 2014  <karl.w.schulz@intel.com> - 
- Initial build.

