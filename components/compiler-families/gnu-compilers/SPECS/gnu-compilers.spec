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

%global gnu12_version 12.2.0
%global gnu12_gmp_version 6.2.1
%global gnu12_mpc_version 1.2.1
%global gnu12_mpfr_version 4.1.0

%global gnu13_version 13.2.0
%global gnu13_gmp_version 6.3.0
%global gnu13_mpc_version 1.3.1
%global gnu13_mpfr_version 4.2.1

%global gnu14_version 14.2.0
%global gnu14_gmp_version 6.3.0
%global gnu14_mpc_version 1.3.1
%global gnu14_mpfr_version 4.2.1

%global gnu15_version 15.2.0
%global gnu15_gmp_version 6.3.0
%global gnu15_mpc_version 1.4.1
%global gnu15_mpfr_version 4.2.2

%global gnu16_version 16.2.0
%global gnu16_gmp_version 6.3.0
%global gnu16_mpc_version 1.4.1
%global gnu16_mpfr_version 4.2.2

%global mirror_url https://mirror1.hs-esslingen.de/Mirrors/ftp.gnu.org/

%if "%{compiler_family}" == "gnu12"
%global gnu_major_ver gnu12
%global gnu_version %{gnu12_version}
%endif
%if "%{compiler_family}" == "gnu13"
%global gnu_major_ver gnu13
%global gnu_version %{gnu13_version}
%endif
%if "%{compiler_family}" == "gnu14"
%global gnu_major_ver gnu14
%global gnu_version %{gnu14_version}
%endif
%if "%{compiler_family}" == "gnu15"
%global gnu_major_ver gnu15
%global gnu_version %{gnu15_version}
%endif
%if "%{compiler_family}" == "gnu16"
%global gnu_major_ver gnu16
%global gnu_version %{gnu16_version}
%endif

%if 0%{?openEuler}
# The brp-strip script in rpm 4.17 and 4.18 runs several strip processes in
# parallel. GCC installs g++ and c++ as hard links of one file, and two strip
# processes can rewrite that file at the same time. strip then fails with
# "file format not recognized". rpm 4.19 strips hard-linked files serially
# (upstream commit ae2e75a129), but openEuler 22.03 and 24.03 ship older rpm.
%global __brp_strip RPM_BUILD_NCPUS=1 %{_rpmconfigdir}/brp-strip %{__strip}
%endif

Source0:   %{mirror_url}gcc/gcc-%{gnu12_version}/gcc-%{gnu12_version}.tar.xz
Source1:   %{mirror_url}gmp/gmp-%{gnu12_gmp_version}.tar.bz2
Source2:   %{mirror_url}mpc/mpc-%{gnu12_mpc_version}.tar.gz
Source3:   %{mirror_url}mpfr/mpfr-%{gnu12_mpfr_version}.tar.gz

Source4:   %{mirror_url}gcc/gcc-%{gnu13_version}/gcc-%{gnu13_version}.tar.xz
Source5:   %{mirror_url}gmp/gmp-%{gnu13_gmp_version}.tar.bz2
Source6:   %{mirror_url}mpc/mpc-%{gnu13_mpc_version}.tar.gz
Source7:   %{mirror_url}mpfr/mpfr-%{gnu13_mpfr_version}.tar.gz

Source8:   %{mirror_url}gcc/gcc-%{gnu14_version}/gcc-%{gnu14_version}.tar.xz
Source9:   %{mirror_url}gmp/gmp-%{gnu14_gmp_version}.tar.bz2
Source10:   %{mirror_url}mpc/mpc-%{gnu14_mpc_version}.tar.gz
Source11:   %{mirror_url}mpfr/mpfr-%{gnu14_mpfr_version}.tar.gz

Source12:   %{mirror_url}gcc/gcc-%{gnu15_version}/gcc-%{gnu15_version}.tar.xz
Source13:   %{mirror_url}gmp/gmp-%{gnu15_gmp_version}.tar.bz2
Source14:   %{mirror_url}mpc/mpc-%{gnu15_mpc_version}.tar.xz
Source15:   %{mirror_url}mpfr/mpfr-%{gnu15_mpfr_version}.tar.gz

Source17:   %{mirror_url}gcc/gcc-%{gnu16_version}/gcc-%{gnu16_version}.tar.xz
Source18:   %{mirror_url}gmp/gmp-%{gnu16_gmp_version}.tar.bz2
Source19:   %{mirror_url}mpc/mpc-%{gnu16_mpc_version}.tar.xz
Source20:   %{mirror_url}mpfr/mpfr-%{gnu16_mpfr_version}.tar.gz

%global pname %{gnu_major_ver}-compilers

Summary:   The GNU C Compiler and Support Files
Name:      %{pname}%{PROJ_DELIM}
Version:   %{gnu_version}
Release:   1%{?dist}
License:   GPLv3 and GPLv3+ with exceptions and LGPLv3 and GPLv2 and LGPLv2+
Group:     %{PROJ_NAME}/compiler-families
URL:       http://gcc.gnu.org/

# Requirements from https://gcc.gnu.org/install/prerequisites.htmlzypper
%if 0%{?openEuler}
BuildRequires:  gcc-toolset-12-gcc-c++
BuildRequires:  gcc-toolset-12-libstdc++-devel
%else
BuildRequires:  gcc-c++
%endif
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
Requires: glibc-devel
Requires: binutils

%global install_path %{OHPC_COMPILERS}/gcc/%{version}
%global module_path %{OHPC_MODULES}/%{gnu_major_ver}

%description
Core package for the GNU Compiler Collection, including the C language
frontend.


%prep
%if "%{compiler_family}" == "gnu12"
%setup -T -q -n gcc-%{version} -b0 -a1 -a2 -a3

ln -s gmp-%{gnu12_gmp_version} gmp
ln -s mpc-%{gnu12_mpc_version} mpc
ln -s mpfr-%{gnu12_mpfr_version} mpfr
%endif

%if "%{compiler_family}" == "gnu13"
%setup -T -q -n gcc-%{version} -b4 -a5 -a6 -a7

ln -s gmp-%{gnu13_gmp_version} gmp
ln -s mpc-%{gnu13_mpc_version} mpc
ln -s mpfr-%{gnu13_mpfr_version} mpfr
%endif

%if "%{compiler_family}" == "gnu14"
%setup -T -q -n gcc-%{version} -b8 -a9 -a10 -a11

ln -s gmp-%{gnu14_gmp_version} gmp
ln -s mpc-%{gnu14_mpc_version} mpc
ln -s mpfr-%{gnu14_mpfr_version} mpfr
%endif

%if "%{compiler_family}" == "gnu15"
%setup -T -q -n gcc-%{version} -b12 -a13 -a14 -a15

ln -s gmp-%{gnu15_gmp_version} gmp
ln -s mpc-%{gnu15_mpc_version} mpc
ln -s mpfr-%{gnu15_mpfr_version} mpfr
%endif

%if "%{compiler_family}" == "gnu16"
%setup -T -q -n gcc-%{version} -b17 -a18 -a19 -a20

ln -s gmp-%{gnu16_gmp_version} gmp
ln -s mpc-%{gnu16_mpc_version} mpc
ln -s mpfr-%{gnu16_mpfr_version} mpfr
%endif

%build

%if 0%{?openEuler}
export PATH=/opt/openEuler/gcc-toolset-12/root/usr/bin:$PATH
export LD_LIBRARY_PATH=/opt/openEuler/gcc-toolset-12/root/usr/lib64:$LD_LIBRARY_PATH
%endif

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

%if 0%{?sle_version}
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
whatis("Description: GNU Compiler Family (C/C++/Fortran)")
whatis("URL: http://gcc.gnu.org/")

local version = "%{version}"

prepend_path("PATH",            "%{install_path}/bin")
prepend_path("MANPATH",         "%{install_path}/share/man")
prepend_path("INCLUDE",         "%{install_path}/include")
prepend_path("LD_LIBRARY_PATH", "%{install_path}/lib64")
prepend_path("MODULEPATH",      "%{OHPC_MODULEDEPS}/%{gnu_major_ver}")

family("compiler")
EOF

%if "%{compiler_family}" == "gnu12"
ln -s %{version}.lua %{buildroot}%{module_path}/default
%endif

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
