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
%define pname wxparaver
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:	Paraver
Name:		%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:	4.7.2
Release:	4
License:	LGPL-2.1
Group:		%{PROJ_NAME}/perf-tools
URL:		https://tools.bsc.es
Source0:	https://ftp.tools.bsc.es/wxparaver/wxparaver-%{version}-src.tar.bz2

BuildRequires: boost-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires: bison
%if 0%{?suse_version}
BuildRequires: flex
BuildRequires: wxGTK3-3_2-devel
%else
BuildRequires: flex-devel
BuildRequires: wxGTK3-devel
%endif
BuildRequires: autoconf%{PROJ_DELIM}
BuildRequires: automake%{PROJ_DELIM}
BuildRequires: libtool%{PROJ_DELIM}
BuildRequires:	binutils-devel
BuildRequires:	libxml2-devel

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Paraver was developed to respond to the need to have a qualitative global perception of the application behavior by visual inspection and then to be able to focus on the detailed quantitative analysis of the problems. Expressive power, flexibility and the capability of efficiently handling large traces are key features addressed in the design of Paraver. The clear and modular structure of Paraver plays a significant role towards achieving these targets.

%prep
%setup -q -n %{pname}-%{version}



%build
%ohpc_setup_compiler
module load boost

%if 0%{?centos_version} || 0%{?rhel}
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-wx-config=/usr/libexec/wxGTK3/wx-config "
%endif
%if 0%{?suse_version}
CONFIGURE_OPTIONS="$CONFIGURE_OPTIONS --with-wx-config=/usr/bin/wx-config "
%endif


./configure --with-paraver=$RPM_BUILD_ROOT%{install_path} --prefix=%{install_path} --with-boost=$BOOST_DIR $CONFIGURE_OPTIONS


cd $RPM_BUILD_DIR/%{pname}-%{version}/src/paraver-kernel/

make %{?_smp_mflags} 

make DESTDIR=$RPM_BUILD_ROOT install


cd $RPM_BUILD_DIR/%{pname}-%{version}/

make %{?_smp_mflags} 

make DESTDIR=$RPM_BUILD_ROOT install


%install
export NO_BRP_CHECK_RPATH=true


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
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
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


%files
%{OHPC_PUB}

%doc
