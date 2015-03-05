%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family variable via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?mpi_family: %define mpi_family openmpi}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

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

# MPI dependencies
%if %{mpi_family} == impi
BuildRequires: intel-mpi%{PROJ_DELIM}
Requires:      intel-mpi%{PROJ_DELIM}
%endif
%if %{mpi_family} == mvapich2
BuildRequires: mvapich2-%{compiler_family}%{PROJ_DELIM}
Requires:      mvapich2-%{compiler_family}%{PROJ_DELIM}
%endif
%if %{mpi_family} == openmpi
BuildRequires: openmpi-%{compiler_family}%{PROJ_DELIM}
Requires:      openmpi-%{compiler_family}%{PROJ_DELIM}
%endif

#-fsp-header-comp-end------------------------------------------------

# Base package name
%define pname tau
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Summary: Tuning and Analysis Utilities
Name: %{pname}%{PROJ_DELIM}
Version: 2.24
Release: 1%{?dist}
License: Tuning and Analysis Utilities License
Group: fsp/perf-tools
URL: http://www.cs.uoregon.edu/research/tau/home.php
Source0: %{pname}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

BuildRequires: binutils
%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{pname}/%version

%description
TAU Performance System is a portable profiling and tracing toolkit for performance 
analysis of parallel programs written in Fortran, C, C++, UPC, Java, Python.

%prep
%setup -q -n %{pname}-%{version}
%ifarch x86_64
sed -i -e 's/^BITS.*/BITS = 64/' src/Profile/Makefile.skel
sed -i 's|lib/libpdb\.a|lib64/libpdb.a|g' utils/Makefile
%endif

%install

# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
export FSP_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/FSP_setup_compiler
. %{_sourcedir}/FSP_setup_mpi

export BUILDROOT=%{buildroot}
./configure \
        -arch=%_arch \
        -c++=mpicxx \
        -cc=mpicc \
        -fortran=mpif90 \
        -prefix=%{buildroot}%{install_path} \
        -exec-prefix= \
        -mpi \
        -mpiinc=$MPI_DIR/include \
        -mpilib=$MPI_DIR/lib -mpilibrary="-lmpi" \
        -slog2 \
        -PROFILE -PROFILECALLPATH -PROFILEPARAM \
        -PROFILEMEMORY \
        -CPUTIME -MULTIPLECOUNTERS \
        -useropt="%optflags -I$PWD/include -fno-strict-aliasing" \
        -MPITRACE -DISABLESHARED \
        -openmp


#rm -rf $RPM_BUILD_ROOT

#sudo make DESTDIR=$RPM_BUILD_ROOT clean install
export BUILDROOTLIB=%buildroot%_libexecdir
%ifarch x86_64
export LIBSUFF=64
%endif
sed -i 's|^\(TAU_PREFIX_INSTALL_DIR\).*|\1=%{buildroot}%{install_path}|' \
    include/Makefile utils/Makefile
TOPDIR=$PWD
make install TOPDIR=$TOPDIR

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

    puts stderr " "
    puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
    puts stderr "toolchain and the %{mpi_family} MPI stack."
    puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

# Remove the static libraries. Static libraries are undesirable:
# https://fedoraproject.org/wiki/Packaging/Guidelines#Packaging_Static_Libraries
rm -rf $RPM_BUILD_ROOT%{_libdir}/*.a

# Remove buildroot references
pushd %{buildroot}%{install_path}/bin
sed -i 's|%{buildroot}||g' $(egrep -R '%{buildroot}' ./|awk -F : '{print $1}')
popd
rm -f %{buildroot}%{install_path}/include/Makefile*
rm -fR %{buildroot}%{install_path}/include/makefiles

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
