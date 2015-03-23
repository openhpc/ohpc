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

%include %{_sourcedir}/FSP_macros

%define 	pname R-base
%define 	PNAME %(echo %{pname} | tr [a-z] [A-Z])

%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Name:		%{pname}%{PROJ_DELIM}
###%define release 1 
Release:	1
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
###Requires:       R-base-devel = %{version}
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

###%define MKL -lmkl_intel_ilp64 -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm
export BLAS_LIBS="-lmkl_intel_ilp64 -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm"

%configure --enable-R-shlib LIBnn=lib64 --with-blas --with-lapack

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

#*#%files -n R-base%files -n %{name}
%files
%defattr(-,root,root)
%{FSP_HOME}


# ld.so.conf
%config /etc/ld.so.conf.d/R.conf


%changelog
