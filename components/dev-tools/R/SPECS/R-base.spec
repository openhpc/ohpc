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
# spec file for package R-base
#-------------------------------------------------------------------------------
# Copyright (c) 2015 SUSE LINUX GmbH, Nuernberg, Germany.
# Copyright (c) 2015, Intel Corporation
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.
#
#
#-------------------------------------------------------------------------------


# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family variable via rpmbuild or other
# mechanisms.
%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%{!?compiler_family: %define compiler_family gnu}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency.
%if !0%{?opensuse_bs}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
BuildRequires: openblas-gnu%{PROJ_DELIM}
Requires:      openblas-gnu%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%if 0%{?OHPC_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-ohpc-header-comp-end------------------------------------------------


%define 	pname R_base
%define 	PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name:		%{pname}%{PROJ_DELIM}
Release:	1%{?dist}
Version:        3.2.2
Source:         https://cran.r-project.org/src/base/R-3/R-%{version}.tar.gz
Patch:          tre.patch
Url:            http://www.r-project.org/
DocDir:         %{OHPC_PUB}/doc/contrib
Summary:        R is a language and environment for statistical computing and graphics (S-Plus like).
License:        GPL-2.0 or GPL-3.0
Group:          ohpc/dev-tools
BuildRoot:	%{_tmppath}/%{pname}-%{version}-%{release}-root

# Default library install path
%define         install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version
%define         debug_package %{nil}

BuildRequires:  cairo-devel
BuildRequires:  gcc-fortran
BuildRequires:  libjpeg-devel
BuildRequires:  libpng-devel
BuildRequires:  libtiff-devel
BuildRequires:  perl
BuildRequires:  readline-devel
%if 0%{?suse_version} > 1020
%if 0%{?suse_version} < 1230
%if 0%{?suse_version} > 1120 
%endif
%else
BuildRequires:  xdg-utils
%endif
%endif
BuildRequires:  pango-devel
BuildRequires:  tcl-devel
### Moved to CENTOS only until SLES has a newer texinfo version
###BuildRequires:  texinfo >= 5.1 
BuildRequires:  tk-devel
# BuildRequires:  xorg-x11-devel
# CentOS needs X11 headers/libs like Intrisic.h which suse provides
%if 0%{?suse_version}  
#BuildRequires:  texlive-fonts-extra
%else
BuildRequires:  libXt-devel
BuildRequires:  texinfo >= 5.1 
#BuildRequires:  bzip2
#BuildRequires:  bzip2-devel
#BuildRequires:  bzip2-libs
%endif
Requires:       cairo >= 1.2
Requires:       fontconfig
Requires:       freetype2
Requires:       make
Requires:       readline
Requires:       xdg-utils
%if 0%{?suse_version}  
BuildRequires:  libicu52_1
Requires:	libicu52_1
%else
BuildRequires:  libicu
Requires:	libicu
%endif

Provides:       R = %{version}
Provides:       R-KernSmooth = 2.23.14
Provides:       R-MASS = 7.3.39
Provides:       R-Matrix = 1.1.5
Obsoletes:      R-Matrix < 1.1.5
Provides:       R-boot = 1.3.15
Provides:       R-class = 7.3.12
Provides:       R-cluster = 2.0.1
Provides:       R-codetools = 0.2.10
Provides:       R-compiler = %{version}
Provides:       R-datasets = %{version}
Provides:       R-foreign = 0.8.63
Provides:       R-grDevices = %{version}
Provides:       R-graphics = %{version}
Provides:       R-grid = %{version}
Provides:       R-lattice = 0.20.30
Provides:       R-methods = %{version}
Provides:       R-mgcv = 1.8.4
Provides:       R-nlme = 3.1.120
Provides:       R-nnet = 7.3.9
Provides:       R-parallel = %{version}
Provides:       R-rpart = 4.1.9
Provides:       R-spatial = 7.3.9
Provides:       R-splines = %{version}
Provides:       R-stats = %{version}
Provides:       R-stats4 = %{version}
Provides:       R-survival = 2.38.1
Provides:       R-tcltk = %{version}
Provides:       R-tools = %{version}
Provides:       R-utils = %{version}

#!BuildIgnore: post-build-checks rpmlint-Factory

%description
R is a language and environment for statistical computing and graphics. 
It is a GNU project which is similar to the S language and environment 
which was developed at Bell Laboratories (formerly AT&T, now Lucent Technologies) 
by John Chambers and colleagues. 

R can be considered as a different implementation of S.  There are some important 
differences, but much code written for S runs unaltered under R.

R provides a wide variety of statistical (linear and nonlinear modelling, 
classical statistical tests, time-series analysis, classification, clustering, …) 
and graphical techniques, and is highly extensible.

%prep 
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

%setup -n R-%{version}
%patch -p1

%build 
# OpenHPC compiler designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

export R_BROWSER="xdg-open"
export R_PDFVIEWER="xdg-open"

%if %{compiler_family} == gnu
module load openblas
BLAS="-L${OPENBLAS_LIB} -lopenblas"
%else
MKL_LIB_PATH=$MKLROOT/lib/intel64
BLAS="-L${MKL_LIB_PATH} -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -fopenmp -ldl -lpthread -lm"
echo "MKL options flag .... $MKL "
%endif

./configure  \
            --with-lapack="$BLAS" \
            --with-blas="$BLAS" \
            --enable-R-shlib  \
            --prefix=%{install_path} \
            --without-system-zlib \
            --without-system-bzlib \
            --without-x \
              LIBnn=lib64 


make %{?_smp_mflags}


%install 
# OpenHPC compiler designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

make DESTDIR=%{buildroot} install

# there is a backup file in survival for 3.1.3
%{__rm} -f %{buildroot}%{_libdir}/R/library/survival/NEWS.Rd.orig

# Install ld.so.conf.d file to ensure other applications access the shared lib
%{__mkdir_p} %{buildroot}/etc/ld.so.conf.d
cat << EOF >%{buildroot}/etc/ld.so.conf.d/R.conf
%{_libdir}/R/lib
EOF

# OpenHPC module file

%{__mkdir_p}  %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}

#%Module1.0#####################################################################

proc ModulesHelp { } {

    puts stderr " "
    puts stderr "This module loads the %{pname} package for statistical computing."
    puts stderr "\nVersion %{version}\n"
    puts stderr " "

}

module-whatis "Name: R project for statistical computing built with the %{compiler_family} compiler toolchain."
module-whatis "Version: %{version}"
module-whatis "Category: utility, developer support, user tool"
module-whatis "Keywords: Statistics"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}


prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib64

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib64
setenv          %{PNAME}_SHARE      %{install_path}/share

if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if { [is-loaded gnu] } {
        if { ![is-loaded openblas]  } {
          module load openblas
        }
    }
}

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{RPM_BUILD_ROOT}/%{_docdir}

export NO_BRP_CHECK_RPATH true

%post
/sbin/ldconfig

%postun
/sbin/ldconfig

%files
%defattr(-,root,root)
%{OHPC_HOME}
%doc ChangeLog
%doc COPYING
%doc README

# ld.so.conf
%config /etc/ld.so.conf.d/R.conf



%changelog
