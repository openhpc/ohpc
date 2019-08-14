#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# R build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

%define 	pname R

Name:		%{pname}-%{compiler_family}%{PROJ_DELIM}
Release:	1%{?dist}
Version:        3.5.3
Source:         https://cran.r-project.org/src/base/R-3/R-%{version}.tar.gz
Url:            http://www.r-project.org/
Summary:        R is a language and environment for statistical computing and graphics (S-Plus like).
License:        GPL-2.0 or GPL-3.0
Group:          %{PROJ_NAME}/serial-libs

# Default library install path
%define         install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

BuildRequires:  cairo-devel
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
BuildRequires:  xz-devel
BuildRequires:  pcre-devel
BuildRequires:  libcurl-devel
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
BuildRequires:  bzip2-devel
%endif

%if 0%{?suse_version} && 0%{?sle_version} < 150000
BuildRequires:  libicu52_1
Requires:	libicu52_1
%else
BuildRequires:  libicu
Requires:	libicu
%endif

Requires:       cairo >= 1.2
Requires:       fontconfig
Requires:       freetype2
Requires:       make
Requires:       readline
Requires:       xdg-utils
Requires:       lmod%{PROJ_DELIM} >= 7.6.1

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
which was developed at Bell Laboratories (formerly AT&T, now Lucent
Technologies) by John Chambers and colleagues.

R can be considered as a different implementation of S. There are some
important differences, but much code written for S runs unaltered under R.

R provides a wide variety of statistical (linear and nonlinear modelling,
classical statistical tests, time-series analysis, classification,
clustering, ...) and graphical techniques, and is highly extensible.

%prep
%setup -n R-%{version}

%build
# OpenHPC compiler designation
%ohpc_setup_compiler

export R_BROWSER="xdg-open"
export R_PDFVIEWER="xdg-open"

%if "%{compiler_family}" != "intel"
%if "%{compiler_family}" == "arm"
BLAS="-L${ARMPL_LIBRARIES} -larmpl_mp -fopenmp"
echo "ArmPL options flags .... ${BLAS} "
%else
module load openblas
BLAS="-L${OPENBLAS_LIB} -lopenblas"
%endif
%else
MKL_LIB_PATH=$MKLROOT/lib/intel64
BLAS="-L${MKL_LIB_PATH} -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -fopenmp -ldl -lpthread -lm"
echo "MKL options flag .... $MKL "
%endif

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm"
FFLAGS="-fPIC -DPIC -i4" \
FCFLAGS="-fPIC -DPIC -i4" \
FPICFLAGS="-i4" \
FCPICFLAGS="-i4" \
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
%ohpc_setup_compiler
make DESTDIR=%{buildroot} install
# there is a backup file in survival for 3.1.3
%{__rm} -f %{buildroot}%{_libdir}/R/library/survival/NEWS.Rd.orig

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

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm"
depends-on openblas
%endif

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%{OHPC_HOME}
%doc ChangeLog
%doc COPYING
%doc README
