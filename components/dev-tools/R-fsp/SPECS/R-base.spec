#
# spec file for package R-base
#
# Copyright (c) 2015 SUSE LINUX GmbH, Nuernberg, Germany.
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

#*#%include %{_sourcedir}/FSP_macros

Name:           R-base
%define 	pname R-base
%define 	PNAME %(echo %{pname} | tr [a-z] [A-Z])

%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

#*#Name:		%{pname}%{PROJ_DELIM}
###%define release 1 
Release		1
Version:        3.1.3
###Release:        %release
Source:         R-%{version}.tar.gz
#Source: http://cran.r-project.org/src/base/R-2/R-%%{version}.tar.gz
# PATCH-FIX-UPSTREAM Fix tre when wchar_t is unsigned int
Patch:          tre.patch

Url:            http://www.r-project.org/

Summary:        R - statistics package (S-Plus like)
License:        GPL-2.0 or GPL-3.0
Group:          Productivity/Scientific/Math
###BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRoot:	%{_tmppath}/%{pname}-%{version}-%{release}-root

# Default library install path
%define		install_path %{FSP_PUB}/%{pname}/%version


BuildRequires:  cairo-devel
BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  gcc-fortran
BuildRequires:  libjpeg-devel
BuildRequires:  libpng-devel
BuildRequires:  libtiff-devel
BuildRequires:  perl
BuildRequires:  readline-devel
%if %suse_version <=1020
BuildRequires:  te_latex
BuildRequires:  tetex
%endif
%if %suse_version > 1020
BuildRequires:  fdupes
%if %suse_version < 1230
BuildRequires:  texlive-bin
BuildRequires:  texlive-bin-latex
#BuildRequires:  texlive-bin-metafont # evtl nur für 12.3 und später
BuildRequires:  texlive-latex
%if %suse_version > 1120 
BuildRequires:  texlive-fonts-extra
%endif
%else
BuildRequires:  texlive-bibtex
BuildRequires:  texlive-cm-super
BuildRequires:  texlive-latex
BuildRequires:  texlive-makeindex
BuildRequires:  texlive-metafont
BuildRequires:  texlive-psnfss
BuildRequires:  texlive-tex
BuildRequires:  texlive-times
BuildRequires:  xdg-utils
# No tex(inconsolata.sty) provided in SLE-12
%if %suse_version != 1315
BuildRequires:  tex(inconsolata.sty)
%endif
%endif
%endif
BuildRequires:  pango-devel
BuildRequires:  tcl-devel
BuildRequires:  texinfo
BuildRequires:  tk-devel
BuildRequires:  xorg-x11-devel
Requires:       R-base-devel = %{version}
Requires:       cairo >= 1.2
Requires:       fontconfig
Requires:       freetype2
Requires:       glibc-locale
Requires:       make
Requires:       readline
Requires:       xdg-utils
Requires:       xorg-x11-fonts-100dpi
Requires:       xorg-x11-fonts-75dpi

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

%description
R is a language which is not entirely unlike the S language developed at
AT&T Bell Laboratories by Rick Becker, John Chambers and Allan Wilks.

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

%setup -n R-%{version}
%patch -p1

%build 
export R_BROWSER="xdg-open"
export R_PDFVIEWER="xdg-open"

%define MKL -lmkl_intel_ilp64 -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm

%configure --enable-R-shlib LIBnn=%{_lib} --with-blas=%{MKL} --with-lapack

make %{?_smp_mflags}
make pdf
make info
# Convert to UTF-8
for i in doc/manual/R-intro.info doc/manual/R-FAQ.info doc/FAQ doc/manual/R-admin.info doc/manual/R-exts.info-1; do
  iconv -f iso-8859-1 -t utf-8 -o $i{.utf8,}
  mv $i{.utf8,}
done

%install 
make DESTDIR=%{buildroot} install
make DESTDIR=%{buildroot} install-pdf

# Installation of Info-files
%{__install} -m 755 -d %{_infodir}
make DESTDIR=%{buildroot} INFODIR=%{buildroot}%{_infodir} install-info
%{__rm} -f %{buildroot}%{_infodir}/dir
%{__rm} -f %{buildroot}%{_infodir}/dir.old

chmod +x %{buildroot}%{_libdir}/R/share/sh/echo.sh

chmod -x %{buildroot}%{_libdir}/R/library/mgcv/CITATION

# there is a backup file in survival for 3.1.3
%{__rm} -f %{buildroot}%{_libdir}/R/library/survival/NEWS.Rd.orig

%if %suse_version > 1020    
%fdupes -s $RPM_BUILD_ROOT  
%endif

# Install ld.so.conf.d file to ensure other applications access the shared lib
mkdir -p %{buildroot}/etc/ld.so.conf.d
cat << EOF >%{buildroot}/etc/ld.so.conf.d/R.conf
%{_libdir}/R/lib
EOF

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

%files -n R-base
%defattr(-, root, root)

## All languages files. Seems there is no short way to include all of them

# base
%dir %{_libdir}/R/library/base/
%{_libdir}/R/library/base/CITATION
%{_libdir}/R/library/base/demo/
%{_libdir}/R/library/base/DESCRIPTION
%{_libdir}/R/library/base/help/
%{_libdir}/R/library/base/html/
%{_libdir}/R/library/base/INDEX
%{_libdir}/R/library/base/Meta/
%{_libdir}/R/library/base/R/
# boot
%dir %{_libdir}/R/library/boot/
%{_libdir}/R/library/boot/bd.q
%{_libdir}/R/library/boot/CITATION
%{_libdir}/R/library/boot/data/
%{_libdir}/R/library/boot/DESCRIPTION
%{_libdir}/R/library/boot/help/
%{_libdir}/R/library/boot/html/
%{_libdir}/R/library/boot/INDEX
%{_libdir}/R/library/boot/Meta/
%{_libdir}/R/library/boot/NAMESPACE
%dir %{_libdir}/R/library/boot/po/
%lang(de) %{_libdir}/R/library/boot/po/de/
%lang(en) %{_libdir}/R/library/boot/po/en*/
%lang(fr) %{_libdir}/R/library/boot/po/fr/
%lang(ko) %{_libdir}/R/library/boot/po/ko/
%lang(pl) %{_libdir}/R/library/boot/po/pl/
%lang(ru) %{_libdir}/R/library/boot/po/ru/
%{_libdir}/R/library/boot/R/
# class
%dir %{_libdir}/R/library/class/
%{_libdir}/R/library/class/CITATION
%{_libdir}/R/library/class/DESCRIPTION
%{_libdir}/R/library/class/help/
%{_libdir}/R/library/class/html/
%{_libdir}/R/library/class/INDEX
%{_libdir}/R/library/class/libs/
#{_libdir}/R/library/class/LICENCE
%{_libdir}/R/library/class/Meta/
%{_libdir}/R/library/class/NAMESPACE
%{_libdir}/R/library/class/NEWS

%dir %{_libdir}/R/library/class/po/
%lang(de) %{_libdir}/R/library/class/po/de/
%lang(en) %{_libdir}/R/library/class/po/en*/
%lang(fr) %{_libdir}/R/library/class/po/fr/
%lang(ko) %{_libdir}/R/library/class/po/ko/
%lang(pl) %{_libdir}/R/library/class/po/pl/
%{_libdir}/R/library/class/R/
# cluster
%dir %{_libdir}/R/library/cluster/
%{_libdir}/R/library/cluster/CITATION
%{_libdir}/R/library/cluster/data/
%{_libdir}/R/library/cluster/DESCRIPTION
%{_libdir}/R/library/cluster/help/
%{_libdir}/R/library/cluster/html/
%{_libdir}/R/library/cluster/INDEX
%{_libdir}/R/library/cluster/libs/
%{_libdir}/R/library/cluster/Meta/
%{_libdir}/R/library/cluster/NAMESPACE
%{_libdir}/R/library/cluster/R/
%dir %{_libdir}/R/library/cluster/po/
%lang(de) %{_libdir}/R/library/cluster/po/de/
%lang(en) %{_libdir}/R/library/cluster/po/en*/
%lang(fr) %{_libdir}/R/library/cluster/po/fr/
%lang(pl) %{_libdir}/R/library/cluster/po/pl/
# codetools
%dir %{_libdir}/R/library/codetools/
%{_libdir}/R/library/codetools/DESCRIPTION
%{_libdir}/R/library/codetools/help/
%{_libdir}/R/library/codetools/html/
%{_libdir}/R/library/codetools/INDEX
%{_libdir}/R/library/codetools/Meta/
%{_libdir}/R/library/codetools/NAMESPACE
%{_libdir}/R/library/codetools/R/
# compiler
%dir %{_libdir}/R/library/compiler/
%{_libdir}/R/library/compiler/DESCRIPTION
%{_libdir}/R/library/compiler/help/
%{_libdir}/R/library/compiler/html/
%{_libdir}/R/library/compiler/INDEX
%{_libdir}/R/library/compiler/Meta/
%{_libdir}/R/library/compiler/NAMESPACE
%{_libdir}/R/library/compiler/R/
# datasets
%dir %{_libdir}/R/library/datasets/
%{_libdir}/R/library/datasets/data/
%{_libdir}/R/library/datasets/DESCRIPTION
%{_libdir}/R/library/datasets/help/
%{_libdir}/R/library/datasets/html/
%{_libdir}/R/library/datasets/INDEX
%{_libdir}/R/library/datasets/Meta/
%{_libdir}/R/library/datasets/NAMESPACE
# foreign
%dir %{_libdir}/R/library/foreign/
%{_libdir}/R/library/foreign/COPYRIGHTS
%{_libdir}/R/library/foreign/DESCRIPTION
%{_libdir}/R/library/foreign/files/
%{_libdir}/R/library/foreign/help/
%{_libdir}/R/library/foreign/html/
%{_libdir}/R/library/foreign/INDEX
%{_libdir}/R/library/foreign/libs/
%{_libdir}/R/library/foreign/Meta/
%{_libdir}/R/library/foreign/NAMESPACE
%dir %{_libdir}/R/library/foreign/po/
%lang(de) %{_libdir}/R/library/foreign/po/de/
%lang(en) %{_libdir}/R/library/foreign/po/en*/
%lang(fr) %{_libdir}/R/library/foreign/po/fr/
%lang(pl) %{_libdir}/R/library/foreign/po/pl/
%{_libdir}/R/library/foreign/R/
# graphics
%dir %{_libdir}/R/library/graphics/
%{_libdir}/R/library/graphics/demo/
%{_libdir}/R/library/graphics/DESCRIPTION
%{_libdir}/R/library/graphics/help/
%{_libdir}/R/library/graphics/html/
%{_libdir}/R/library/graphics/INDEX
%{_libdir}/R/library/graphics/libs/
%{_libdir}/R/library/graphics/Meta/
%{_libdir}/R/library/graphics/NAMESPACE
%{_libdir}/R/library/graphics/R/
# grDevices
%dir %{_libdir}/R/library/grDevices
%{_libdir}/R/library/grDevices/afm/
%{_libdir}/R/library/grDevices/DESCRIPTION
%dir %{_libdir}/R/library/grDevices/demo/
%{_libdir}/R/library/grDevices/demo/colors.R
%{_libdir}/R/library/grDevices/demo/hclColors.R
%{_libdir}/R/library/grDevices/enc/
%{_libdir}/R/library/grDevices/help/
%{_libdir}/R/library/grDevices/html/
%{_libdir}/R/library/grDevices/icc/
%{_libdir}/R/library/grDevices/INDEX
%{_libdir}/R/library/grDevices/libs/
%{_libdir}/R/library/grDevices/Meta/
%{_libdir}/R/library/grDevices/NAMESPACE
%{_libdir}/R/library/grDevices/R/
# grid
%dir %{_libdir}/R/library/grid/
%{_libdir}/R/library/grid/DESCRIPTION
%{_libdir}/R/library/grid/doc/
%{_libdir}/R/library/grid/help/
%{_libdir}/R/library/grid/html/
%{_libdir}/R/library/grid/INDEX
%{_libdir}/R/library/grid/libs/
%{_libdir}/R/library/grid/Meta/
%{_libdir}/R/library/grid/NAMESPACE
%{_libdir}/R/library/grid/R/
# KernSmooth
%dir %{_libdir}/R/library/KernSmooth/
%{_libdir}/R/library/KernSmooth/DESCRIPTION
%{_libdir}/R/library/KernSmooth/help/
%{_libdir}/R/library/KernSmooth/html/
%{_libdir}/R/library/KernSmooth/INDEX
%{_libdir}/R/library/KernSmooth/libs/
%{_libdir}/R/library/KernSmooth/Meta/
%{_libdir}/R/library/KernSmooth/NAMESPACE
%dir %{_libdir}/R/library/KernSmooth/po/
%lang(de) %{_libdir}/R/library/KernSmooth/po/de/
%lang(en) %{_libdir}/R/library/KernSmooth/po/en*/
%lang(fr) %{_libdir}/R/library/KernSmooth/po/fr/
%lang(pl) %{_libdir}/R/library/KernSmooth/po/pl/
%lang(ko) %{_libdir}/R/library/KernSmooth/po/ko/
%{_libdir}/R/library/KernSmooth/R/
# lattice
%dir %{_libdir}/R/library/lattice/
%{_libdir}/R/library/lattice/CITATION
%{_libdir}/R/library/lattice/data/
%{_libdir}/R/library/lattice/demo/
%{_libdir}/R/library/lattice/DESCRIPTION
%{_libdir}/R/library/lattice/help/
%{_libdir}/R/library/lattice/html/
%{_libdir}/R/library/lattice/INDEX
%{_libdir}/R/library/lattice/libs/
%{_libdir}/R/library/lattice/Meta/
%{_libdir}/R/library/lattice/NAMESPACE
%{_libdir}/R/library/lattice/NEWS
%dir %{_libdir}/R/library/lattice/po/
%lang(de) %{_libdir}/R/library/lattice/po/de/
%lang(en) %{_libdir}/R/library/lattice/po/en*/
%lang(fr) %{_libdir}/R/library/lattice/po/fr/
%lang(ko) %{_libdir}/R/library/lattice/po/ko/
%lang(pl) %{_libdir}/R/library/lattice/po/pl/
%{_libdir}/R/library/lattice/R/
# MASS
%dir %{_libdir}/R/library/MASS/
%{_libdir}/R/library/MASS/CITATION
%{_libdir}/R/library/MASS/data/
%{_libdir}/R/library/MASS/DESCRIPTION
%{_libdir}/R/library/MASS/help/
%{_libdir}/R/library/MASS/html/
%{_libdir}/R/library/MASS/INDEX
%{_libdir}/R/library/MASS/libs/
#{_libdir}/R/library/MASS/LICENCE
%{_libdir}/R/library/MASS/Meta/
%{_libdir}/R/library/MASS/NAMESPACE
%{_libdir}/R/library/MASS/NEWS
%dir %{_libdir}/R/library/MASS/po
%lang(de) %{_libdir}/R/library/MASS/po/de/
%lang(en) %{_libdir}/R/library/MASS/po/en*/
%lang(fr) %{_libdir}/R/library/MASS/po/fr/
%lang(ko) %{_libdir}/R/library/MASS/po/ko/
%lang(pl) %{_libdir}/R/library/MASS/po/pl/
%{_libdir}/R/library/MASS/R/
%{_libdir}/R/library/MASS/scripts/
# Matrix
%dir %{_libdir}/R/library/Matrix/
%{_libdir}/R/library/Matrix/Copyrights
%{_libdir}/R/library/Matrix/data/
%{_libdir}/R/library/Matrix/doc/
%{_libdir}/R/library/Matrix/DESCRIPTION
%{_libdir}/R/library/Matrix/Doxyfile
%{_libdir}/R/library/Matrix/external/
%{_libdir}/R/library/Matrix/help/
%{_libdir}/R/library/Matrix/html/
%{_libdir}/R/library/Matrix/INDEX
%{_libdir}/R/library/Matrix/libs/
%{_libdir}/R/library/Matrix/Meta/
%{_libdir}/R/library/Matrix/NAMESPACE
%dir %{_libdir}/R/library/Matrix/po/
%lang(de) %{_libdir}/R/library/Matrix/po/de/
%lang(en) %{_libdir}/R/library/Matrix/po/en*/
%lang(fr) %{_libdir}/R/library/Matrix/po/fr/
%lang(pl) %{_libdir}/R/library/Matrix/po/pl/
%{_libdir}/R/library/Matrix/R/
%{_libdir}/R/library/Matrix/test-tools.R
%{_libdir}/R/library/Matrix/test-tools-1.R
%{_libdir}/R/library/Matrix/test-tools-Matrix.R
# methods
%dir %{_libdir}/R/library/methods/
%{_libdir}/R/library/methods/DESCRIPTION
%{_libdir}/R/library/methods/help/
%{_libdir}/R/library/methods/html/
%{_libdir}/R/library/methods/INDEX
%{_libdir}/R/library/methods/libs/
%{_libdir}/R/library/methods/Meta/
%{_libdir}/R/library/methods/NAMESPACE
%{_libdir}/R/library/methods/R/
# mgcv
%dir %{_libdir}/R/library/mgcv/
%{_libdir}/R/library/mgcv/CITATION
%{_libdir}/R/library/mgcv/DESCRIPTION
%{_libdir}/R/library/mgcv/help/
%{_libdir}/R/library/mgcv/data
%{_libdir}/R/library/mgcv/html/
%{_libdir}/R/library/mgcv/INDEX
%{_libdir}/R/library/mgcv/libs/
%{_libdir}/R/library/mgcv/Meta/
%{_libdir}/R/library/mgcv/NAMESPACE
%{_libdir}/R/library/mgcv/R/
# nlme
%dir %{_libdir}/R/library/nlme/
%{_libdir}/R/library/nlme/CITATION
%{_libdir}/R/library/nlme/data/
%{_libdir}/R/library/nlme/DESCRIPTION
%{_libdir}/R/library/nlme/help/
%{_libdir}/R/library/nlme/html/
%{_libdir}/R/library/nlme/INDEX
%{_libdir}/R/library/nlme/libs/
#{_libdir}/R/library/nlme/LICENCE
%{_libdir}/R/library/nlme/Meta/
%{_libdir}/R/library/nlme/mlbook/
%{_libdir}/R/library/nlme/NAMESPACE
%dir %{_libdir}/R/library/nlme/po/
%lang(de) %{_libdir}/R/library/nlme/po/de/
%lang(en) %{_libdir}/R/library/nlme/po/en*/
%lang(fr) %{_libdir}/R/library/nlme/po/fr/
%lang(ko) %{_libdir}/R/library/nlme/po/ko/
%lang(pl) %{_libdir}/R/library/nlme/po/pl/
%{_libdir}/R/library/nlme/R/
%{_libdir}/R/library/nlme/scripts/
# nnet
%dir %{_libdir}/R/library/nnet/
%{_libdir}/R/library/nnet/CITATION
%{_libdir}/R/library/nnet/DESCRIPTION
%{_libdir}/R/library/nnet/help/
%{_libdir}/R/library/nnet/html/
%{_libdir}/R/library/nnet/INDEX
%{_libdir}/R/library/nnet/libs/
#{_libdir}/R/library/nnet/LICENCE
%{_libdir}/R/library/nnet/Meta/
%{_libdir}/R/library/nnet/NAMESPACE
%{_libdir}/R/library/nnet/NEWS
%dir %{_libdir}/R/library/nnet/po
%lang(de) %{_libdir}/R/library/nnet/po/de/
%lang(en) %{_libdir}/R/library/nnet/po/en*/
%lang(fr) %{_libdir}/R/library/nnet/po/fr/
%lang(ko) %{_libdir}/R/library/nnet/po/ko/
%lang(pl) %{_libdir}/R/library/nnet/po/pl/
%{_libdir}/R/library/nnet/R/
# parallel
%dir %{_libdir}/R/library/parallel/
%{_libdir}/R/library/parallel/DESCRIPTION
%{_libdir}/R/library/parallel/INDEX
%dir %{_libdir}/R/library/parallel/doc
%{_libdir}/R/library/parallel/doc/index.html
%{_libdir}/R/library/parallel/doc/parallel.pdf
%{_libdir}/R/library/parallel/doc/parallel.R
%{_libdir}/R/library/parallel/doc/parallel.Rnw
%dir %{_libdir}/R/library/parallel/Meta
%{_libdir}/R/library/parallel/Meta/Rd.rds
%{_libdir}/R/library/parallel/Meta/hsearch.rds
%{_libdir}/R/library/parallel/Meta/links.rds
%{_libdir}/R/library/parallel/Meta/nsInfo.rds
%{_libdir}/R/library/parallel/Meta/package.rds
%{_libdir}/R/library/parallel/Meta/vignette.rds
%{_libdir}/R/library/parallel/NAMESPACE
%dir %{_libdir}/R/library/parallel/R
%{_libdir}/R/library/parallel/R/parallel
%{_libdir}/R/library/parallel/R/parallel.rdb
%{_libdir}/R/library/parallel/R/parallel.rdx
%dir %{_libdir}/R/library/parallel/help
%{_libdir}/R/library/parallel/help/AnIndex
%{_libdir}/R/library/parallel/help/aliases.rds
%{_libdir}/R/library/parallel/help/parallel.rdb
%{_libdir}/R/library/parallel/help/parallel.rdx
%{_libdir}/R/library/parallel/help/paths.rds
%dir %{_libdir}/R/library/parallel/html
%{_libdir}/R/library/parallel/html/00Index.html
%{_libdir}/R/library/parallel/html/R.css
%dir %{_libdir}/R/library/parallel/libs
%{_libdir}/R/library/parallel/libs/parallel.so
# rpart
%dir %{_libdir}/R/library/rpart/
%{_libdir}/R/library/rpart/data/
%{_libdir}/R/library/rpart/doc/
%{_libdir}/R/library/rpart/DESCRIPTION
%{_libdir}/R/library/rpart/help/
%{_libdir}/R/library/rpart/html/
%{_libdir}/R/library/rpart/INDEX
%{_libdir}/R/library/rpart/libs/
%{_libdir}/R/library/rpart/Meta/
%{_libdir}/R/library/rpart/NAMESPACE
%{_libdir}/R/library/rpart/NEWS.Rd
%dir %{_libdir}/R/library/rpart/po
%lang(de) %{_libdir}/R/library/rpart/po/de/
%lang(en) %{_libdir}/R/library/rpart/po/en*/
%lang(fr) %{_libdir}/R/library/rpart/po/fr/
%lang(ko) %{_libdir}/R/library/rpart/po/ko/
%lang(pl) %{_libdir}/R/library/rpart/po/pl/
%lang(ru) %{_libdir}/R/library/rpart/po/ru/
%{_libdir}/R/library/rpart/R/
# spatial
%dir %{_libdir}/R/library/spatial/
%{_libdir}/R/library/spatial/CITATION
%{_libdir}/R/library/spatial/DESCRIPTION
%{_libdir}/R/library/spatial/help/
%{_libdir}/R/library/spatial/html/
%{_libdir}/R/library/spatial/INDEX
%{_libdir}/R/library/spatial/libs/
#{_libdir}/R/library/spatial/LICENCE
%{_libdir}/R/library/spatial/Meta/
%{_libdir}/R/library/spatial/NAMESPACE
%{_libdir}/R/library/spatial/NEWS
%dir %{_libdir}/R/library/spatial/po
%lang(de) %{_libdir}/R/library/spatial/po/de/
%lang(en) %{_libdir}/R/library/spatial/po/en*/
%lang(fr) %{_libdir}/R/library/spatial/po/fr/
%lang(ko) %{_libdir}/R/library/spatial/po/ko/
%lang(pl) %{_libdir}/R/library/spatial/po/pl/
%{_libdir}/R/library/spatial/ppdata/
%{_libdir}/R/library/spatial/PP.files
%{_libdir}/R/library/spatial/R/
# splines
%dir %{_libdir}/R/library/splines/
%{_libdir}/R/library/splines/DESCRIPTION
%{_libdir}/R/library/splines/help/
%{_libdir}/R/library/splines/html/
%{_libdir}/R/library/splines/INDEX
%{_libdir}/R/library/splines/libs/
%{_libdir}/R/library/splines/Meta/
%{_libdir}/R/library/splines/NAMESPACE
%{_libdir}/R/library/splines/R/
# stats
%dir %{_libdir}/R/library/stats/
%{_libdir}/R/library/stats/COPYRIGHTS.modreg
%{_libdir}/R/library/stats/demo/
%{_libdir}/R/library/stats/DESCRIPTION
%{_libdir}/R/library/stats/help/
%{_libdir}/R/library/stats/html/
%{_libdir}/R/library/stats/INDEX
%{_libdir}/R/library/stats/libs/
%{_libdir}/R/library/stats/Meta/
%{_libdir}/R/library/stats/NAMESPACE
%{_libdir}/R/library/stats/R/
%{_libdir}/R/library/stats/SOURCES.ts
# stats4
%dir %{_libdir}/R/library/stats4/
%{_libdir}/R/library/stats4/DESCRIPTION
%{_libdir}/R/library/stats4/help/
%{_libdir}/R/library/stats4/html/
%{_libdir}/R/library/stats4/INDEX
%{_libdir}/R/library/stats4/Meta/
%{_libdir}/R/library/stats4/NAMESPACE
%{_libdir}/R/library/stats4/R/
# survival
%dir %{_libdir}/R/library/survival/
%{_libdir}/R/library/survival/data/
%{_libdir}/R/library/survival/CITATION
%{_libdir}/R/library/survival/COPYRIGHTS
%{_libdir}/R/library/survival/doc/
%{_libdir}/R/library/survival/DESCRIPTION
%{_libdir}/R/library/survival/help/
%{_libdir}/R/library/survival/html/
%{_libdir}/R/library/survival/INDEX
%{_libdir}/R/library/survival/libs/
%{_libdir}/R/library/survival/Meta/
%{_libdir}/R/library/survival/NAMESPACE
%{_libdir}/R/library/survival/NEWS.Rd
%{_libdir}/R/library/survival/R/
# tcltk
%dir %{_libdir}/R/library/tcltk/
%{_libdir}/R/library/tcltk/demo/
%{_libdir}/R/library/tcltk/DESCRIPTION
%{_libdir}/R/library/tcltk/exec/
%{_libdir}/R/library/tcltk/help/
%{_libdir}/R/library/tcltk/html/
%{_libdir}/R/library/tcltk/INDEX
%{_libdir}/R/library/tcltk/libs/
%{_libdir}/R/library/tcltk/Meta/
%{_libdir}/R/library/tcltk/NAMESPACE
%{_libdir}/R/library/tcltk/R/
# tools
%dir %{_libdir}/R/library/tools/
%{_libdir}/R/library/tools/DESCRIPTION
%{_libdir}/R/library/tools/help/
%{_libdir}/R/library/tools/html/
%{_libdir}/R/library/tools/INDEX
%{_libdir}/R/library/tools/libs/
%{_libdir}/R/library/tools/Meta/
%{_libdir}/R/library/tools/NAMESPACE
%{_libdir}/R/library/tools/R/

# translations
%doc %{_libdir}/R/library/translations/DESCRIPTION
%dir %{_libdir}/R/library/translations/
%dir %{_libdir}/R/library/translations/da/
%dir %{_libdir}/R/library/translations/da/LC_MESSAGES/
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-base.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-compiler.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-grDevices.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-graphics.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-grid.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-methods.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-parallel.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-splines.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-stats.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-stats4.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-tcltk.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-tools.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R-utils.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/R.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/RGui.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/grDevices.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/graphics.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/grid.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/methods.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/parallel.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/splines.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/stats.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/tcltk.mo
%lang(da) %{_libdir}/R/library/translations/da/LC_MESSAGES/tools.mo
%dir %{_libdir}/R/library/translations/de
%dir %{_libdir}/R/library/translations/de/LC_MESSAGES
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-base.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-compiler.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-grDevices.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-graphics.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-grid.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-methods.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-parallel.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-splines.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-stats.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-stats4.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-tcltk.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-tools.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R-utils.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/R.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/RGui.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/grDevices.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/graphics.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/grid.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/methods.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/parallel.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/splines.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/stats.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/tcltk.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/tools.mo
%lang(de) %{_libdir}/R/library/translations/de/LC_MESSAGES/utils.mo
%dir %{_libdir}/R/library/translations/en
%dir %{_libdir}/R/library/translations/en/LC_MESSAGES
%lang(en) %{_libdir}/R/library/translations/en/LC_MESSAGES/R.mo
%dir %{_libdir}/R/library/translations/en@quot
%dir %{_libdir}/R/library/translations/en@quot/LC_MESSAGES
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-base.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-compiler.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-grDevices.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-graphics.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-grid.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-methods.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-parallel.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-splines.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-stats.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-stats4.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-tcltk.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-tools.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R-utils.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/R.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/grDevices.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/graphics.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/grid.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/methods.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/parallel.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/splines.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/stats.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/tcltk.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/tools.mo
%lang(en) %{_libdir}/R/library/translations/en@quot/LC_MESSAGES/utils.mo
%dir %{_libdir}/R/library/translations/en_GB
%dir %{_libdir}/R/library/translations/en_GB/LC_MESSAGES
%lang(en_GB) %{_libdir}/R/library/translations/en_GB/LC_MESSAGES/R-grDevices.mo
%lang(en_GB) %{_libdir}/R/library/translations/en_GB/LC_MESSAGES/R.mo
%lang(en_GB) %{_libdir}/R/library/translations/en_GB/LC_MESSAGES/grDevices.mo
%dir %{_libdir}/R/library/translations/es
%dir %{_libdir}/R/library/translations/es/LC_MESSAGES
%lang(es) %{_libdir}/R/library/translations/es/LC_MESSAGES/R.mo
%lang(es) %{_libdir}/R/library/translations/es/LC_MESSAGES/RGui.mo
%lang(es) %{_libdir}/R/library/translations/es/LC_MESSAGES/graphics.mo
%dir %{_libdir}/R/library/translations/fa
%dir %{_libdir}/R/library/translations/fa/LC_MESSAGES
%lang(fr) %{_libdir}/R/library/translations/fa/LC_MESSAGES/R-base.mo
%lang(fr) %{_libdir}/R/library/translations/fa/LC_MESSAGES/R-utils.mo
%lang(fr) %{_libdir}/R/library/translations/fa/LC_MESSAGES/R.mo
%lang(fr) %{_libdir}/R/library/translations/fa/LC_MESSAGES/RGui.mo
%dir %{_libdir}/R/library/translations/fr
%dir %{_libdir}/R/library/translations/fr/LC_MESSAGES
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-base.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-compiler.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-grDevices.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-graphics.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-grid.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-methods.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-parallel.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-splines.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-stats.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-stats4.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-tcltk.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-tools.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R-utils.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/R.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/RGui.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/grDevices.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/graphics.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/grid.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/methods.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/parallel.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/splines.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/stats.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/tcltk.mo
%lang(fr) %{_libdir}/R/library/translations/fr/LC_MESSAGES/tools.mo
%dir %{_libdir}/R/library/translations/it
%dir %{_libdir}/R/library/translations/it/LC_MESSAGES
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R-base.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R-grDevices.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R-graphics.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R-stats.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R-stats4.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R-tcltk.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R-tools.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/R.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/RGui.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/grDevices.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/graphics.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/grid.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/stats.mo
%lang(it) %{_libdir}/R/library/translations/it/LC_MESSAGES/tools.mo
%dir %{_libdir}/R/library/translations/ja
%dir %{_libdir}/R/library/translations/ja/LC_MESSAGES
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-base.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-compiler.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-grDevices.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-graphics.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-grid.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-methods.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-splines.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-stats.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-stats4.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-tcltk.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-tools.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R-utils.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/R.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/RGui.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/grDevices.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/graphics.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/grid.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/methods.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/splines.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/stats.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/tcltk.mo
%lang(ja) %{_libdir}/R/library/translations/ja/LC_MESSAGES/tools.mo
%dir %{_libdir}/R/library/translations/ko
%dir %{_libdir}/R/library/translations/ko/LC_MESSAGES
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-base.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-compiler.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-grDevices.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-graphics.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-grid.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-methods.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-parallel.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-splines.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-stats.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-stats4.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-tcltk.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-tools.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R-utils.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/R.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/RGui.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/grDevices.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/graphics.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/grid.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/methods.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/parallel.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/splines.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/stats.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/tcltk.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/tools.mo
%lang(ko) %{_libdir}/R/library/translations/ko/LC_MESSAGES/utils.mo
%dir %{_libdir}/R/library/translations/nn
%dir %{_libdir}/R/library/translations/nn/LC_MESSAGES
%lang(nn) %{_libdir}/R/library/translations/nn/LC_MESSAGES/R-base.mo
%lang(nn) %{_libdir}/R/library/translations/nn/LC_MESSAGES/R.mo
%lang(nn) %{_libdir}/R/library/translations/nn/LC_MESSAGES/RGui.mo
%lang(nn) %{_libdir}/R/library/translations/nn/LC_MESSAGES/graphics.mo
%dir %{_libdir}/R/library/translations/pl
%dir %{_libdir}/R/library/translations/pl/LC_MESSAGES
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-base.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-compiler.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-grDevices.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-graphics.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-grid.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-methods.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-parallel.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-splines.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-stats.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-stats4.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-tcltk.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-tools.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R-utils.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/R.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/RGui.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/graphics.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/grDevices.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/grid.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/methods.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/parallel.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/splines.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/stats.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/tcltk.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/tools.mo
%lang(pl) %{_libdir}/R/library/translations/pl/LC_MESSAGES/utils.mo
%dir %{_libdir}/R/library/translations/pt_BR
%dir %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-base.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-compiler.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-grDevices.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-graphics.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-grid.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-methods.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-splines.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-stats.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-stats4.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-tcltk.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-tools.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R-utils.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/R.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/RGui.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/grDevices.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/graphics.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/grid.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/methods.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/splines.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/stats.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/tcltk.mo
%lang(pt_BR) %{_libdir}/R/library/translations/pt_BR/LC_MESSAGES/tools.mo
%dir %{_libdir}/R/library/translations/ru
%dir %{_libdir}/R/library/translations/ru/LC_MESSAGES
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-base.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-compiler.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-grDevices.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-graphics.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-grid.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-methods.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-parallel.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-splines.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-stats.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-stats4.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-tcltk.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-tools.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R-utils.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/R.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/RGui.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/grDevices.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/graphics.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/grid.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/methods.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/parallel.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/splines.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/stats.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/tcltk.mo
%lang(ru) %{_libdir}/R/library/translations/ru/LC_MESSAGES/tools.mo
%dir %{_libdir}/R/library/translations/tr
%dir %{_libdir}/R/library/translations/tr/LC_MESSAGES
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/R-base.mo
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/R-stats.mo
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/R-stats4.mo
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/R-tools.mo
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/R-utils.mo
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/R.mo
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/RGui.mo
%lang(tr) %{_libdir}/R/library/translations/tr/LC_MESSAGES/graphics.mo
%dir %{_libdir}/R/library/translations/zh_CN
%dir %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-base.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-compiler.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-grDevices.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-graphics.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-grid.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-methods.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-parallel.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-splines.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-stats.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-stats4.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-tcltk.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-tools.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R-utils.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/R.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/RGui.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/grDevices.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/graphics.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/grid.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/methods.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/parallel.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/splines.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/stats.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/tcltk.mo
%lang(zh_CN) %{_libdir}/R/library/translations/zh_CN/LC_MESSAGES/tools.mo
%dir %{_libdir}/R/library/translations/zh_TW
%dir %{_libdir}/R/library/translations/zh_TW/LC_MESSAGES
%lang(zh_TW) %{_libdir}/R/library/translations/zh_TW/LC_MESSAGES/R.mo
%lang(zh_TW) %{_libdir}/R/library/translations/zh_TW/LC_MESSAGES/RGui.mo
%lang(zh_TW) %{_libdir}/R/library/translations/zh_TW/LC_MESSAGES/graphics.mo

# utils
%dir %{_libdir}/R/library/utils/
%{_libdir}/R/library/utils/DESCRIPTION
%{_libdir}/R/library/utils/help/
%{_libdir}/R/library/utils/html/
%{_libdir}/R/library/utils/iconvlist
%{_libdir}/R/library/utils/INDEX
%{_libdir}/R/library/utils/libs/
%{_libdir}/R/library/utils/Meta/
%{_libdir}/R/library/utils/misc/
%{_libdir}/R/library/utils/NAMESPACE
%{_libdir}/R/library/utils/R/
%{_libdir}/R/library/utils/Sweave/
%{_libdir}/R/library/utils/doc/

# R-base main part
%doc README
%{_mandir}/man1/R.1*
%{_mandir}/man1/Rscript.1*
%doc %{_libdir}/R/COPYING
%doc %{_libdir}/R/SVN-REVISION
%{_infodir}/*.gz
%doc %{_libdir}/R/doc/
%dir %{_libdir}/R
%{_bindir}/*
%{_libdir}/R/bin/
%{_libdir}/R/etc/
%{_libdir}/R/lib/
%{_libdir}/R/modules/
%dir %{_libdir}/R/share
%dir %{_libdir}/R/library
%{_libdir}/R/share/encodings/
%{_libdir}/R/share/java/

%dir %{_libdir}/R/share/dictionaries/
%{_libdir}/R/share/dictionaries/en_stats.rds
%{_libdir}/R/share/licenses/
%{_libdir}/R/share/make/
%{_libdir}/R/share/R/
%{_libdir}/R/share/sh/
%{_libdir}/R/share/texmf/

# ld.so.conf
%config /etc/ld.so.conf.d/R.conf

%files -n R-base-devel
%defattr(-, root, root)
%dir %{_libdir}/R/include/
%{_libdir}/R/include/R.h
%{_libdir}/R/include/Rconfig.h
%{_libdir}/R/include/Rdefines.h
%{_libdir}/R/include/Rembedded.h
%{_libdir}/R/include/Rinterface.h
%{_libdir}/R/include/Rinternals.h
%{_libdir}/R/include/Rmath.h
%{_libdir}/R/include/Rversion.h
%{_libdir}/R/include/S.h
%dir %{_libdir}/R/include/R_ext
%{_libdir}/R/include/R_ext/Applic.h
%{_libdir}/R/include/R_ext/Arith.h
%{_libdir}/R/include/R_ext/BLAS.h
%{_libdir}/R/include/R_ext/Boolean.h
%{_libdir}/R/include/R_ext/Callbacks.h
%{_libdir}/R/include/R_ext/Complex.h
%{_libdir}/R/include/R_ext/Connections.h
%{_libdir}/R/include/R_ext/Constants.h
%{_libdir}/R/include/R_ext/Error.h
%{_libdir}/R/include/R_ext/GetX11Image.h
%{_libdir}/R/include/R_ext/GraphicsDevice.h
%{_libdir}/R/include/R_ext/GraphicsEngine.h
%{_libdir}/R/include/R_ext/Itermacros.h
%{_libdir}/R/include/R_ext/Lapack.h
%{_libdir}/R/include/R_ext/Linpack.h
%{_libdir}/R/include/R_ext/MathThreads.h
%{_libdir}/R/include/R_ext/Memory.h
%{_libdir}/R/include/R_ext/Parse.h
%{_libdir}/R/include/R_ext/Print.h
%{_libdir}/R/include/R_ext/PrtUtil.h
%{_libdir}/R/include/R_ext/QuartzDevice.h
%{_libdir}/R/include/R_ext/R-ftp-http.h
%{_libdir}/R/include/R_ext/Rallocators.h
%{_libdir}/R/include/R_ext/RS.h
%{_libdir}/R/include/R_ext/RStartup.h
%{_libdir}/R/include/R_ext/Random.h
%{_libdir}/R/include/R_ext/Rdynload.h
%{_libdir}/R/include/R_ext/Riconv.h
%{_libdir}/R/include/R_ext/Utils.h
%{_libdir}/R/include/R_ext/Visibility.h
%{_libdir}/R/include/R_ext/eventloop.h
%{_libdir}/R/include/R_ext/libextern.h
%{_libdir}/R/include/R_ext/stats_package.h
%{_libdir}/R/include/R_ext/stats_stubs.h

%{_libdir}/pkgconfig/libR.pc
%dir %{_libdir}/R/library/Matrix/include/
%{_libdir}/R/library/Matrix/include/Matrix.h
%{_libdir}/R/library/Matrix/include/Matrix_stubs.c
%{_libdir}/R/library/Matrix/include/cholmod.h

%changelog
