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

%global gnu_version 12.2.0
%global gnu_major_ver gnu12
%global pname %{gnu_major_ver}-compilers

# Define subcomponent versions required for build
%global gmp_version 6.2.1
%global mpc_version 1.2.1
%global mpfr_version 4.1.0

Summary:   The GNU C Compiler and Support Files
Name:      %{pname}%{PROJ_DELIM}
Version:   %{gnu_version}
Release:   1%{?dist}
License:   GPLv3 and GPLv3+ with exceptions and LGPLv3 and GPLv2 and LGPLv2+
Group:     %{PROJ_NAME}/compiler-families
URL:       http://gcc.gnu.org/
Source0:   https://ftp.gnu.org/gnu/gcc/gcc-%{gnu_version}/gcc-%{gnu_version}.tar.xz
Source1:   https://ftp.gnu.org/gnu/gmp/gmp-%{gmp_version}.tar.bz2
Source2:   https://ftp.gnu.org/gnu/mpc/mpc-%{mpc_version}.tar.gz
Source3:   https://ftp.gnu.org/gnu/mpfr/mpfr-%{mpfr_version}.tar.gz

# Requirements from https://gcc.gnu.org/install/prerequisites.htmlzypper
BuildRequires:  gcc-c++
BuildRequires:  binutils >= 2.30
BuildRequires:  make >= 3.80
BuildRequires:  gettext-devel >= 0.14.5
BuildRequires:  flex >= 2.5.4
BuildRequires:  texinfo >= 4.7
BuildRequires:  m4 >= 1.4.6
%if 0%{?sle_version} || 0%{?suse_version}
BuildRequires:  fdupes
%endif

Requires: glibc >= 2.10.90

%global install_path %{OHPC_COMPILERS}/gcc/%{version}
%global module_path %{OHPC_MODULES}/%{gnu_major_ver}

%description
Core package for the GNU Compiler Collection, including the C language
frontend.


%prep
%setup -q -n gcc-%{version} -a1 -a2 -a3

ln -s gmp-%{gmp_version} gmp
ln -s mpc-%{mpc_version} mpc
ln -s mpfr-%{mpfr_version} mpfr


%build
mkdir obj
cd obj
../configure --disable-multilib \
             --enable-languages="c,c++,fortran" \
             --prefix=%{install_path} \
             --disable-static \
             --enable-shared
make %{?_smp_mflags}


%install
cd obj
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

%if 0%{?sle_version} || 0%{?suse_version}
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/include
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/lib
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/install-tools
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/share
%endif

mkdir -p ${RPM_BUILD_ROOT}/%{_docdir}

# Based on https://git.centos.org/rpms/gcc/blob/c8/f/SPECS/gcc.spec
ln -sf gcc %{buildroot}%{install_path}/bin/cc
ln -sf gfortran %{buildroot}%{install_path}/bin/f95

cat > %{buildroot}%{install_path}/bin/c89 <<"EOF"
#!/bin/sh
fl="-std=c89"
for opt; do
  case "$opt" in
    -ansi|-std=c89|-std=iso9899:1990) fl="";;
    -std=*) echo "`basename $0` called with non ANSI/ISO C option $opt" >&2
    exit 1;;
  esac
done
exec gcc $fl ${1+"$@"}
EOF
cat > %{buildroot}%{install_path}/bin/c99 <<"EOF"
#!/bin/sh
fl="-std=c99"
for opt; do
  case "$opt" in
    -std=c99|-std=iso9899:1999) fl="";;
    -std=*) echo "`basename $0` called with non ISO C99 option $opt" >&2
    exit 1;;
  esac
done
exec gcc $fl ${1+"$@"}
EOF
chmod 755 %{buildroot}%{install_path}/bin/c?9

# OpenHPC module files
mkdir -p %{buildroot}%{module_path}

cat << EOF > %{buildroot}%{module_path}/%{version}.lua
help([[
This module loads the GNU compiler collection"

See the man pages for gcc, g++, and gfortran for detailed information
on available compiler options and command-line syntax.

Version %{version}
]])

whatis("Name: GNU Compiler Collection")
whatis("Version: %{version}")
whatis("Category: compiler, runtime support")
whatis("Description: GNU Compiler Family (C/C++/Fortran for x86_64)")
whatis("URL: http://gcc.gnu.org/")

local version = "%{version}"

prepend_path("PATH",            "%{install_path}/bin")
prepend_path("MANPATH",         "%{install_path}/share/man")
prepend_path("INCLUDE",         "%{install_path}/include")
prepend_path("LD_LIBRARY_PATH", "%{install_path}/lib64")
prepend_path("MODULEPATH",      "%{OHPC_MODULEDEPS}/%{gnu_major_ver}")

family("compiler")
EOF

ln -s %{version}.lua %{buildroot}%{module_path}/default


%files
%{module_path}
%dir %{OHPC_COMPILERS}/gcc
%{install_path}
%license COPYING
%license COPYING.LIB
%license COPYING.RUNTIME
%license COPYING3
%license COPYING3.LIB
%doc README
%doc ChangeLog.tree-ssa
%doc ChangeLog
