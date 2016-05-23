#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# OCR build. This uses the compiler family defined by OHPC

#-ohpc-header-comp-begin----------------------------------------------

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}
#%define PROJ_DELIM -ohpc

# OpenHPC convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family:      %define mpi_family none}

# Lmod dependency (note that lmod is pre-populated in the OpenHPC OBS build
# environment; if building outside, lmod remains a formal build dependency.
%if !0%{?opensuse_bs}
BuildRequires: lmod%{PROJ_DELIM}
%endif
# Compiler dependencies
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{OHPC_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-ohpc-header-comp-end------------------------------------------------

# Base package name
%define pname ocr
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

#
# build options      .rpmmacros options      change to default action
# ===============    ====================    ========================
# --with mpi         %_with_mpi         1    build MPI OCR version
#
#  Allow defining --with and --without build options or %_with and %without in .rpmmacros
#    ocr_with    builds option by default unless --without is specified
#    ocr_without builds option iff --with specified
#  
%define ocr_with_opt() %{expand:%%{!?_without_%{1}:%%global ocr_with_%{1} 1}}
%define ocr_without_opt() %{expand:%%{?_with_%{1}:%%global ocr_with_%{1} 1}}
#  
#  with helper macro to test for ocr_with_*
#
%define ocr_with() %{expand:%%{?ocr_with_%{1}:1}%%{!?ocr_with_%{1}:0}}
   
#  Options that are off by default (enable with --with <opt>)
%ocr_without_opt mpi

Summary:   Open Community Runtime (OCR) for shared memory
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   1.0.1
Release:   1
License:   BSD
Group:     ohpc/runtimes
URL:       https://xstack.exascale-tech.com/wiki
#Source0:   OCR.tbz2 
Source0:   https://xstack.exascale-tech.com/git/public?p=ocr.git;a=snapshot;h=OCRv%{version}_ohpc;sf=tbz2
Source1:   OHPC_macros
Source2:   OHPC_setup_compiler
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
Requires:  %{pname}-support%{PROJ_DELIM}
DocDir:    %{OHPC_PUB}/doc/contrib

%description
The Open Community Runtime project is creating an application
building framework that explores new methods of high-core-count
programming. The initial focus is on HPC applications. Its goal
is to create a tool that helps app developers improve the power
efficiency, programmability, and reliability of their work
while maintaining app performance.

This version is for shared memory systems.

#!BuildIgnore: post-build-checks rpmlint-Factory

%package -n %{pname}-devel%{PROJ_DELIM}
Summary: Header files and other development tools needed for developing OCR applications
Group:   ohpc/runtimes
%description -n %{pname}-devel%{PROJ_DELIM}
OCR headers and development tools. You only need to install this package if you
want to develop applications using OCR.

%package -n %{pname}-support%{PROJ_DELIM}
Summary: Support files needed to run OCR applications.
Group:   ohpc/runtimes
%description -n %{pname}-support%{PROJ_DELIM}
OCR support files and scripts. This package is needed to both build and run
OCR applications. The scripts are installed in <prefix>/share/ocr/scripts

%if %{ocr_with mpi}
%package -n %{pname}_mpi-%{compiler_family}-%{mpi-family}%{PROJ_DELIM}
# MPI stuff for MPI version
# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi-devel%{PROJ_DELIM}
Requires:      intel-mpi-devel%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif
Requires: %{pname}-support%{PROJ_DELIM}
Summary:   Open Community Runtime (OCR) for clusters using MPI
Group:   ohpc/runtimes
%description -n %{pname}_mpi-%{compiler_family}-%{mpi-family}%{PROJ_DELIM}
The Open Community Runtime project is creating an application
building framework that explores new methods of high-core-count
programming. The initial focus is on HPC applications. Its goal
is to create a tool that helps app developers improve the power
efficiency, programmability, and reliability of their work
while maintaining app performance.

This version is for clusters using MPI.
%endif # End of %{ocr_with mpi}
%define debug_package %{nil}

%prep

%setup -q -n %{pname}

%build
cd ocr/build
# OpenHPC compiler/mpi designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

%if %{compiler_family} == intel
export CFLAGS="-fp-model strict $CFLAGS"
%endif

OCR_TYPE=x86 make %{?_smp_mflags} all
%if %{ocr_with mpi}
OCR_TYPE=x86-mpi make %{?_smp_mflags} all
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%install
cd ocr/build
# OpenHPC compiler designation
export OHPC_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/OHPC_setup_compiler

mkdir -p $RPM_BUILD_ROOT/%{install_path}
make OCR_TYPE=x86 OCR_INSTALL=$RPM_BUILD_ROOT/%{install_path} %{?_smp_mflags} install
%if %{ocr_with mpi}
make OCR_TYPE=x86-mpi OCR_INSTALL=$RPM_BUILD_ROOT/%{install_path} %{?_smp_mflags} install
%endif
# Remove static libraries
find "%buildroot" -type f -name "*.la" -print0 | xargs -0 rm -f
find "%buildroot" -type f -name "*.a" -print0 | xargs -0 rm -f


# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain for shared memory"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} for shared memory built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version		    %{version}

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          OCR_INSTALL         %{install_path}
setenv          OCR_TYPE            x86

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%if %{ocr_with mpi}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}-mpi/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain for clusters using MPI"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} for clusters using MPI built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version		    %{version}

prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          OCR_INSTALL         %{install_path}
setenv          OCR_TYPE            x86-mpi

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}-mpi/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-mpi-%{version}
##
set     ModulesVersion      "%{version}"
EOF
%endif

%clean
rm -rf $RPM_BUILD_ROOT

%files -n %{pname}-devel%{PROJ_DELIM}
%defattr(-,root,root,-)
%{install_path}/include

%files -n %{pname}-support%{PROJ_DELIM}
%defattr(-,root,root,-)
%{install_path}/share/ocr/scripts

%if %{ocr_with mpi}
%files -n %{pname}_mpi-%{compiler_family}-%{mpi-family}%{PROJ_DELIM}
%defattr(-,root,root,-)
%{install_path}/bin/ocrrun_mpi
%{install_path}/lib/libocr_mpi.*
%config %{install_path}/share/ocr/config/x86-mpi
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}-mpi/.version.%{version}
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}-mpi/%{version}
%endif

%files
%defattr(-,root,root,-)
%{install_path}/bin/ocrrun_x86
%{install_path}/lib/libocr_x86.*
%config %{install_path}/share/ocr/config/x86
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}

%changelog
