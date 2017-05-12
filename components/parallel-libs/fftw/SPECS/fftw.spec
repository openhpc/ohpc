#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# FFTW library that is is dependent on compiler toolchain and MPI

%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

%{!?mpi_family:      %global mpi_family openmpi}

# Base package name
%define pname fftw
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:   A Fast Fourier Transform library
Name:      %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   3.3.6
Release:   1%{?dist}
License:   GPLv2+
Group:     %{PROJ_NAME}/parallel-libs
URL:       http://www.fftw.org
Source0:   http://www.fftw.org/fftw-%{version}-pl2.tar.gz
Source1:   OHPC_macros
Source3:   OHPC_setup_mpi

%define openmp        1
%define mpi           1

BuildRequires:        perl
BuildRequires:        postfix
BuildRequires:        util-linux
Requires(post):       info
Requires(preun):      info


# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
FFTW is a C subroutine library for computing the Discrete Fourier
Transform (DFT) in one or more dimensions, of both real and complex
data, and of arbitrary input size.


%prep
%setup -q -n %{pname}-%{version}-pl2

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

BASEFLAGS="--enable-shared --disable-dependency-tracking --enable-threads"
%if %{openmp}
BASEFLAGS="$BASEFLAGS --enable-openmp"
%endif
%if %{mpi}
BASEFLAGS="$BASEFLAGS --enable-mpi"
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_mpi
%endif

./configure --prefix=%{install_path} ${BASEFLAGS} --enable-static=no || { cat config.log && exit 1; }

make

%install
# OpenHPC compiler designation
%ohpc_setup_compiler
export OHPC_MPI_FAMILY=%{mpi_family}
. %{_sourcedir}/OHPC_setup_mpi

make DESTDIR=$RPM_BUILD_ROOT install

# don't package static libs
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*la


# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}


%post
/sbin/ldconfig || exit 1
/sbin/install-info --section="Math" %{_infodir}/%{pname}.info.gz %{_infodir}/dir  2>/dev/null || :
exit 0

%preun
if [ "$1" = 0 ]; then
  /sbin/install-info --delete %{_infodir}/%{pname}.info.gz %{_infodir}/dir 2>/dev/null || :
fi

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc AUTHORS ChangeLog CONVENTIONS COPYING COPYRIGHT INSTALL NEWS README TODO

%changelog
* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 3.3.4-1
- Switching to %%ohpc_compiler macro

* Tue Aug  5 2014  <karl.w.schulz@intel.com> -
- Initial build.
