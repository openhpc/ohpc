Summary: The GNU C Compiler and Support Files
Name:    FSP-gnu-compilers
Version: 4.9.2
Release: 1
License: GPL-3.0+
Group:   Development/Languages/C and C++
URL:     http://gcc.gnu.org/
Source0: gcc-%{version}.tar.bz2
Source1: gmp-6.0.0a.tar.bz2
Source2: mpc-1.0.2.tar.gz
Source3: mpfr-3.1.2.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

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

%define install_path %{FSP_COMPILERS}/gcc/%{version}

%description

Core package for the GNU Compiler Collection, including the C language
frontend.

%prep
%setup -n gcc-%{version}
%setup -n gcc-%{version} -T -D -a 1
%setup -n gcc-%{version} -T -D -a 2
%setup -n gcc-%{version} -T -D -a 3

ln -s gmp-6.0.0 gmp
ln -s mpc-1.0.2 mpc
ln -s mpfr-3.1.2 mpfr

%build

mkdir obj
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

# FSP module file
mkdir -p %{buildroot}/%{FSP_MODULES}/gnu
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/gnu/%{version}
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
prepend-path    MODULEPATH          %{FSP_MODULEDEPS}/gnu

family "compiler"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/gnu/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{name}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%post
/sbin/ldconfig

%postun
/sbin/ldconfig


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
* Tue Aug  5 2014  <karl.w.schulz@intel.com> - 
- Initial build.

