#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname dimemas

Summary:	Dimemas tool
Name:		%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:	5.4.2
Release:	1
License:	GNU
Group:		%{PROJ_NAME}/perf-tools
URL:		https://tools.bsc.es
Source0:	https://ftp.tools.bsc.es/dimemas/dimemas-%{version}-src.tar.bz2
Source1:        https://github.com/westes/flex/releases/download/v2.6.4/flex-2.6.4.tar.gz

BuildRequires: boost-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      boost-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires: bison flex
BuildRequires: autoconf automake
BuildRequires: libtool
BuildRequires: gettext gettext-devel help2man
#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Dimemas is a performance analysis tool for message-passing
programs. It enables the user to develop and tune parallel applications on a
workstation, while providing an accurate prediction of their performance on the
parallel target machine. The Dimemas simulator reconstructs the time behavior
of a parallel application on a machine modeled by a set of performance
parameters. Thus, performance experiments can be done easily. The supported
target architecture classes include networks of workstations, single and
clustered SMPs, distributed memory parallel computers, and even heterogeneous
systems.


%prep
%setup -a1 -q -n %{pname}-%{version}


%build
# Build temporary copy of flex to provide static libs
HOME=$(pwd)
cd flex-2.6.4
./configure --prefix=$HOME --enable-static=yes --enable-shared=no CFLAGS="-fPIC" || cat config.log
make %{?_smp_mflags}
make install
cd $HOME

%ohpc_setup_compiler
module load boost

# Add libfl.a to the library path
CFLAGS="-L${HOME}/lib ${CFLAGS}"
CXXFLAGS="-L${HOME}/lib ${CXXFLAGS}"
LDFLAGS="-L${HOME}/lib ${LDFLAGS}"

./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --enable-static=no \
            --enable-shared=yes \
            --with-boost=$BOOST_DIR || cat config.log


%install
export NO_BRP_CHECK_RPATH=true

# OpenHPC compiler designation
%ohpc_setup_compiler

make DESTDIR=%{buildroot} install

cp AUTHORS %{buildroot}%{install_path}
cp COPYING %{buildroot}%{install_path}
cp ChangeLog %{buildroot}%{install_path}

# OpenHPC module file
mkdir -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
cat << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {
puts stderr "\nThis module loads the %{pname} library"
puts stderr "built with the %{compiler_family} compiler"
puts stderr "and %{mpi_family} MPI library toolchain."
puts stderr "\nVersion %{version}\n"
}

module-whatis "Name: %{pname} (%{compiler_family} compiler/%{mpi_family})"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

# This build requires the boost library
depends-on boost

prepend-path    PATH                %{install_path}/bin

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include
EOF

cat << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF


%files
%{install_path}
%doc AUTHORS
%doc ChangeLog
%license COPYING
%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}

