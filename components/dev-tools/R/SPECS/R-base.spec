#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
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
# Please submit bugfixes or comments to bugs.FSP
#
#-------------------------------------------------------------------------------


# FSP convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family variable via rpmbuild or other
# mechanisms.
%include %{_sourcedir}/FSP_macros

%{!?compiler_family:	%define compiler_family gnu}
%{!?PROJ_DELIM:		%define PROJ_DELIM   %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM}
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
# hack to install MKL for the moment
BuildRequires: intel-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end------------------------------------------------


%define 	pname R_base
%define 	PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name:		%{pname}%{PROJ_DELIM}
Release:	1%{?dist}
Version:        3.2.0
Source:         R-%{version}.tar.gz
Patch:          tre.patch
Url:            http://www.r-project.org/
Summary:        R is a language and environment for statistical computing and graphics (S-Plus like).
License:        GPL-2.0 or GPL-3.0
Group:          Productivity/Scientific/Math
###BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRoot:	%{_tmppath}/%{pname}-%{version}-%{release}-root

# Default library install path
%define		install_path %{FSP_PUB}/%{pname}/%version
%define         debug_package %{nil}

BuildRequires:  cairo-devel
BuildRequires:  gcc-fortran
BuildRequires:  libjpeg-devel
BuildRequires:  libpng-devel
BuildRequires:  libtiff-devel
BuildRequires:  perl
BuildRequires:  readline-devel
%if 0%{?suse_version} > 1020
BuildRequires:  fdupes
%if 0%{?suse_version} < 1230
%if 0%{?suse_version} > 1120 
%endif
%else
BuildRequires:  xdg-utils
%if 0%{?suse_version} != 1315
###BuildRequires:  tex(inconsolata.sty)
%endif
%endif
%endif
BuildRequires:  pango-devel
BuildRequires:  tcl-devel
### Moved to CENTOS only until SLES has a newer texinfo version
###BuildRequires:  texinfo >= 5.1 
BuildRequires:  tk-devel
BuildRequires:  xorg-x11-devel
BuildRequires:  intel-compilers%{PROJ_DELIM}
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
###Requires:       R-base-devel = %{version}
Requires:       cairo >= 1.2
Requires:       fontconfig
Requires:       freetype2
####Requires:       glibc-locale
Requires:       make
Requires:       readline
Requires:       xdg-utils
####Requires:       xorg-x11-fonts-100dpi
####Requires:       xorg-x11-fonts-75dpi
###Requires:       texlive-latex
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
#Provides:       R-base = %%{version} # implicitly provided
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

###%package -n R-base-devel
###Summary:        Libraries and includefiles for developing with R-base
###Group:          Development/Libraries/Other
###Provides:       R-Matrix-devel = 1.1.5
###Provides:       R-devel = %{version}
###Requires:       R-base
###Obsoletes:      R-Matrix-devel < 1.1.5

###%description -n R-base-devel
###This package provides the necessary development headers and
###libraries to allow you to devel with R-base.

%prep 
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

%if %{compiler_family} == gnu
module load mkl
%endif

%setup -n R-%{version}
%patch -p1

%build 
# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

%if %{compiler_family} == gnu
module load mkl
%endif

export R_BROWSER="xdg-open"
export R_PDFVIEWER="xdg-open"


MKL_LIB_PATH=$MKLROOT/lib/intel64
MKL="-L${MKL_LIB_PATH} -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -fopenmp -ldl -lpthread -lm"
echo "MKL options flag .... $MKL "


./configure --with-blas="$MKL" \
            --with-lapack \
            --enable-R-shlib  \
            --enable-BLAS-shlib \
            --prefix=%{install_path} \
            --without-system-zlib \
            --without-system-bzlib \
            --without-x \
              LIBnn=lib64 

make %{?_smp_mflags}
###make pdf
%if 0%{?suse_version}
### don't make info
### need texinfo > 5.1 but SLE12 only provides ver 4.xx; update the distro or add newer texinfo to FSP?
%else
###make info
# Convert to UTF-8
###for i in doc/manual/R-intro.info doc/manual/R-FAQ.info doc/FAQ doc/manual/R-admin.info doc/manual/R-exts.info-1; do
###  iconv -f iso-8859-1 -t utf-8 -o $i{.utf8,}
###  mv $i{.utf8,}
###done
%endif

%install 
# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

%if %{compiler_family} == gnu
module load mkl
%endif

make DESTDIR=%{buildroot} install
###make DESTDIR=%{buildroot} install-pdf

echo "*********11111***********"
echo %{buildroot}
echo %{__install}
echo %{_infodir}

# Installation of Info-files
####%{__install} -m 755 -d %{_infodir}
###make DESTDIR=%{buildroot} INFODIR=%{buildroot}%{_infodir} install-info
###
### 
%if 0%{?suse_version}
### don't make info
### need texinfo > 5.1 but SLE12 only provides ver 4.xx; update the distro or add newer texinfo to FSP?
%else
####make DESTDIR=%{buildroot} install-info
####%{__rm} -f %{buildroot}%{_infodir}/dir
####%{__rm} -f %{buildroot}%{_infodir}/dir.old
%endif

###chmod +x %{buildroot}%{_libdir}/R/share/sh/echo.sh

###chmod -x %{buildroot}%{_libdir}/R/library/mgcv/CITATION

# there is a backup file in survival for 3.1.3
%{__rm} -f %{buildroot}%{_libdir}/R/library/survival/NEWS.Rd.orig

%if 0%{?suse_version} > 1020    
%fdupes -s $RPM_BUILD_ROOT  
%endif

# Install ld.so.conf.d file to ensure other applications access the shared lib
mkdir -p %{buildroot}/etc/ld.so.conf.d
cat << EOF >%{buildroot}/etc/ld.so.conf.d/R.conf
%{_libdir}/R/lib
EOF

echo "*******222222*********"
# FSP module file

%{__mkdir} -p %{buildroot}/%{FSP_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/%{version}

#%Module1.0#####################################################################

proc ModulesHelp { } {

    puts stderr " "
    puts stderr "This module loads the %{pname} package for statistical computing."
    puts stderr "\nVersion %{version}\n"
    puts stderr " "

}

module-whatis "Name: %{pname} built with %{compiler_family} compiler and Intel MKL support"
module-whatis "Version: %{version}"
module-whatis "Category: utility, developer support, user tool"
module-whatis "Keywords: Statistics"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

## module load mkl ...
if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  ![is-loaded intel]  } {
        module load mkl
    }
}


### TRon (4/27/15) Add path for shared libraries: libgfortran.so.3 when using Intel
if [ expr [ module-info mode load ] || [module-info mode display ] ] {
    if {  [is-loaded intel]  } {
        prepend-path    LD_LIBRARY_PATH     %{FSP_COMPILERS}/gcc/4.9.2/lib64
        #/opt/fsp/pub/compiler/gcc/4.9.2/lib64
    }
}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib64

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib64
setenv          %{PNAME}_SHARE      %{install_path}/share

EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

export NO_BRP_CHECK_RPATH true

%post
/sbin/ldconfig
%install_info --info-dir=%{_infodir} %{_infodir}/R-exts.info-1.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-FAQ.info.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-lang.info.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-admin.info.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-exts.info-2.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-intro.info.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-data.info.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-exts.info.gz
%install_info --info-dir=%{_infodir} %{_infodir}/R-ints.info.gz

%postun
/sbin/ldconfig
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-exts.info-1.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-FAQ.info.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-lang.info.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-admin.info.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-exts.info-2.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-intro.info.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-data.info.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-exts.info.gz
%install_info_delete --info-dir=%{_infodir} %{_infodir}/R-ints.info.gz

#*#%files -n R-base%files -n %{name}
%files
%defattr(-,root,root)
%{FSP_HOME}


# ld.so.conf
%config /etc/ld.so.conf.d/R.conf


%changelog
