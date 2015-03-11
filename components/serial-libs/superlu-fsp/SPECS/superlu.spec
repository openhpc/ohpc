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

# Serial superlu build dependent on compiler toolchain

%include %{_sourcedir}/FSP_macros

#-fsp-header-comp-begin-----------------------------

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM   %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end-------------------------------

Name:           superlu
Summary:        A general purpose library for the direct solution of linear equations
License:        BSD-3-Clause
Group:          Development/Libraries/C and C++
Version:        4.3
Release:        6.2.2
#Source:         http://crd-legacy.lbl.gov/~xiaoye/SuperLU/superlu_4.3.tar.gz
Source:         superlu_%{version}-bsd.tar.bz2
Source1:        superlu_ug.pdf
# PATCH-FEATURE-OPENSUSE superlu-4.3.diff : add compiler and build flags in make.inc
Patch:          superlu-4.3.diff
# PATCH-FIX-UPSTREAM superlu-4.3-include.patch : avoid implicit declaration warnings
Patch1:         superlu-4.3-include.patch
# PATCH-FIX-UPSTREAM superlu-4.3-dont-opt-away.diff
Patch2:         superlu-4.3-dont-opt-away.diff
# PATCH-FIX-OPENSUSE superlu-4.3-remove-hsl.patch [bnc#796236] 
# The Harwell Subroutine Library (HSL) routine m64ad.c have been removed
# from the original sources for legal reasons. This patch disables the inclusion of
# this routine in the library which, however, remains fully functionnal
Patch3:         superlu-4.3-disable-hsl.patch
Url:            http://crd.lbl.gov/~xiaoye/SuperLU/
BuildRequires:  blas
%if 0%{?suse_version}
BuildRequires:  fdupes
%endif
BuildRequires:  tcsh
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{pname}/%version

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
# superlu_ug.pdf in %%doc 
cp %SOURCE1 ./

%build
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

make lib

mkdir tmp 
(cd tmp; ar -x ../lib/libsuperlu_%{version}.a)
gfortran -shared -Wl,-soname,libsuperlu.so.4 -o lib/libsuperlu.so tmp/*.o

%install
mkdir -p %{buildroot}%{install_path}/lib
mkdir -p %{buildroot}%{install_path}/include
install -m644 SRC/*.h %{buildroot}%{install_path}/include
install -m755 lib/libsuperlu.so %{buildroot}%{install_path}/lib/libsuperlu.so.%{version}
ln -s %{_libdir}/libsuperlu.so.%{version} %{buildroot}%{install_path}/lib/libsuperlu.so.4
ln -s %{_libdir}/libsuperlu.so.4 %{buildroot}%{install_path}/lib/libsuperlu.so

#fix permissions
chmod 644 MATLAB/*

# remove all build examples
cd EXAMPLE
make clean
rm -rf *itersol*
cd ..
mv EXAMPLE examples
cp FORTRAN/README README.fortran
%if 0%{?suse_version}
%fdupes -s examples
%endif

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version             %{version}

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "%{pname}"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%post 
/sbin/ldconfig

%postun 
/sbin/ldconfig

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
* Tue Jan 15 2013 scorot@free.fr
- repackage original source tarball in order to remove the
  HSL mc64ad routine that caonnt be redistributed bnc#796236
- add README.SUSE file in the %%%%doci in order to explain
  that change
* Tue Jan  8 2013 scorot@free.fr
- add patch superlu-4.3-disable-hsl.patch in order to disable HSL
  code from the library
- update patch superlu-4.3.diff so that test routines are run
  against the  shared library
- build tests routines in %%%%check section
* Tue Dec 18 2012 scorot@free.fr
- Update to SuperLU 4.3:
  * Remove recursive DFS for postordering elimination tree in
    sp_coletree.c (The nonrecursive routine nr_etdfs() was
    contributed by Cedric Doucet, CEDRAT Group, Meylan, France.)
  * Make supermatrix.h the same for all three libraries
  * Include an on-line HTML documentation for the source code
  * Corrected backward error BERR formula when a component of the
    RHS and the residual are exactly zero
  * Change parameter "delta" to genmmd() from 1 to 0 in get_perm_c
    remove "static" declaration in EXAMPLE/xlinsolx*.c
  * Include threshold-based incomplete factorization (ILU)
  * Removed the static global variables so that it is thread-safe.
  * Make superlu_options_t{} structure and enum constants the same
    for both superlu & superlu_dist.
  * Replace qsort by "quick select" (qselect) in ILU's secondary
    dropping.
  * Replace mc64ad.f by mc64ad.c using f2c.
  * Bug fixes in ilu_sdrop_row.c.
  * Bug fixes in xgsisx.c, so that when mc64 permutation is used
    in ILU, the right-hand side is permuted properly.
  * Add parameter #7 in sp_ienv(), setting as the maxsuper for ILU
    code (smaller than parameter #3).
  * Update Users Guide.
  * Update doxygen code documentation.
  * Fix a bug in ILU driver routine dgsisx.c, so that on return,
    the initial row permutation is combined with perm_r[] from
    partial pivoting.
  * Modify 2 ILU examples: EXAMPLE/ditersol.c, EXAMPLE/ditersol1.c
  * Update superlu_timer.c
- Remove unneeded patches
  + superlu-overflow.patch : Applied upstream
  + superlu-initialize.diff : Applied upstream
  + superlu-undef-code.diff : Applied upstream
- Updated patches
  + superlu-4.3.diff
  + superlu-4.3-include.patch
  + superlu-4.3-dont-opt-away.diff
- Build shared libraries
- Put shared libs and devel files in separate packages
- Use rpm macros instead of plain directory names
- Add %%%%ckeck
- Update documentation file %%%%source1 and put html and examples
  files in %%%%doc
- Spec file reformating
* Sun Jan 29 2012 jengelh@medozas.de
- Remove redundant tags/sections per specfile guideline suggestions
- Parallel building using %%_smp_mflags
* Thu Jun 22 2006 ro@suse.de
- remove selfprovides
* Mon Mar 27 2006 garloff@suse.de
- superlu-undef-code.diff: Fix undefined code (#160443).
* Wed Jan 25 2006 mls@suse.de
- converted neededforbuild to BuildRequires
* Thu Dec 22 2005 garloff@suse.de
- Install header files into /usr/include/superlu/
- Update User Guide.
* Wed Dec 21 2005 garloff@suse.de
- Include EXAMPLE dir as documentation.
- Build as non-root.
- Nuke unused local vars referencing uninitialized vars.
* Wed Dec 21 2005 garloff@suse.de
- Update to SuperLU-3.0: [#133821]
  * Added "options" input argument and "stat" output argument
    for the driver routines
  * More Examples
  * Added a "symmetric mode" for (nearly) symmetric matrices
  * Better Fortran interface.
  * fixed a bug in the complex drivers sgssvx/zgssvx when the
    input matrix is stored in compressed row format.
  * prefixed the header files by "slu_".
  * Fixed a memory leak in get_perm_c() when bnz=0.
  * Changed "abs" to "fabs" in izmax1.
  * Upgraded COLAMD to the latest version (2.3).15
- Add various declarations and #include statements
- Trick compiler into not optimizing away a loop in our timing
  measurement.
* Fri Nov 25 2005 yxu@suse.de
- add missing function prototypes
* Tue Nov 15 2005 dmueller@suse.de
- avoid an endless loop during building (#133820)
* Mon May  9 2005 meissner@suse.de
- fixed 1 byte buffer overflows.
* Mon Dec 20 2004 garloff@suse.de
- Pass $(RPM_OPT_FLAGS) -Os -fPIC for files that don't need to be
  optimized.
* Sat Sep  6 2003 garloff@suse.de
- Add -fPIC to allow inclusion in dynamic lib.
* Sun Aug 10 2003 garloff@suse.de
- -O3 leads to an endless loop in dlamanch test on x86. Probably
  some compiler of FPU weirdness. Remove -O3 on x86 again.
* Thu Aug  7 2003 garloff@suse.de
- Adapt chown syntax to owner:group.
- Reenable -O3 (compiler bug fixed?), but limit automatical
  inlining.
* Wed Jun 18 2003 ro@suse.de
- added directory to filelist.
* Wed Aug 28 2002 ro@suse.de
- remove -O3 from cflags (results in endless loop).
* Wed May  8 2002 garloff@suse.de
- lib64 fix.
* Wed May  8 2002 garloff@suse.de
- Fix test scripts.
- Use BuildRoot.
* Wed May  8 2002 garloff@suse.de
- Remove BLAS, Lapack.
- Rename package into SuperLU.
* Sat Apr 20 2002 garloff@suse.de
- Disable -fast-math
- In SuperLU, use CBLAS that ships with it
* Fri Apr 19 2002 garloff@suse.de
- Update to
  * CLAPACK-3.0
  * SuperLU-2.0
- Added SuperLU user guide
* Fri Apr 19 2002 garloff@suse.de
- Fixed compilation with gcc-3.1:
  link with -lfrtbegin (which contains main formerly in g2c)
* Tue Apr  9 2002 ro@suse.de
- fix owner for README.SuperLU
* Mon Aug  7 2000 garloff@suse.de
- Use %%{_docdir} to put docu in the right place.
* Mon Aug  7 2000 garloff@suse.de
- Add -mcpu=pentiumpro for i386
* Mon Sep 13 1999 bs@suse.de
- ran old prepare_spec on spec file to switch to new prepare_spec.
* Tue Aug 31 1999 garloff@suse.de
- First creation of package.
