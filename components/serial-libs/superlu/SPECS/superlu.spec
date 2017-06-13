#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package superlu
#
# Copyright (c) 2013 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

# SuperLU library build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname superlu
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary:        A general purpose library for the direct solution of linear equations
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/serial-libs
Version:        5.2.1
Release:        0%{?dist}
Source:         http://crd-legacy.lbl.gov/%7Exiaoye/SuperLU/%{pname}_%{version}.tar.gz
Source1:        OHPC_macros
# PATCH-FEATURE-OPENSUSE superlu-5.1-make.patch : add compiler and build flags in make.inc
Patch:          superlu-5.2-make.patch
# PATCH-FIX-UPSTREAM superlu-4.3-include.patch : avoid implicit declaration warnings
Patch1:         superlu-4.3-include.patch
# PATCH-FIX-UPSTREAM superlu-4.3-dont-opt-away.diff
Patch2:         superlu-4.3-dont-opt-away.diff
# PATCH-FIX-OPENSUSE superlu-5.1-remove-hsl.patch [bnc#796236]
# The Harwell Subroutine Library (HSL) routine m64ad.c have been removed
# from the original sources for legal reasons. This patch disables the inclusion of
# this routine in the library which, however, remains fully functionnal
Patch3:         superlu-5.1-disable-hsl.patch
Url:            http://crd.lbl.gov/~xiaoye/SuperLU/
BuildRequires:  tcsh

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
SuperLU is an algorithm that uses group theory to optimize LU
decomposition of sparse matrices. It's the fastest direct solver for
linear systems that the author is aware of.

Docu can be found on http://www.netlib.org.

%prep
%setup -q -n SuperLU_%{version}
%patch -p1
%patch1 -p1
%patch2 -p1
%patch3 -p1

%build
%ohpc_setup_compiler

make lib

mkdir tmp
(cd tmp; ar -x ../SRC/libsuperlu.a)
$FC -shared -Wl,-soname,libsuperlu.so.4 -o lib/libsuperlu.so tmp/*.o

%install
mkdir -p %{buildroot}%{install_path}/lib
mkdir -p %{buildroot}%{install_path}/include
install -m644 SRC/*.h %{buildroot}%{install_path}/include
install -m755 lib/libsuperlu.so %{buildroot}%{install_path}/lib/libsuperlu.so.%{version}
pushd %{buildroot}%{install_path}/lib
ln -s libsuperlu.so.%{version} libsuperlu.so.4
ln -s libsuperlu.so.4 libsuperlu.so
popd

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the SuperLU library built with the %{compiler_family} compiler"
puts stderr "toolchain."
puts stderr " "
puts stderr "Note that this build of SuperLU leverages the OpenBLAS linear algebra libraries."
puts stderr "Consequently, openblas is loaded automatically with this module."

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version                     %{version}

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if { [is-loaded gnu] } {
        if { ![is-loaded openblas]  } {
          module load openblas
        }
    }
}


prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_LIB        %{install_path}/lib

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc README

%changelog
* Fri May 12 2017 Karl W Schulz <karl.w.schulz@intel.com> - 5.2.1-0
- switch to ohpc_compiler_dependent flag

* Mon Feb 20 2017 Adrian Reber <areber@redhat.com> - 5.2.1-0
- Switching to %%ohpc_compiler macro
