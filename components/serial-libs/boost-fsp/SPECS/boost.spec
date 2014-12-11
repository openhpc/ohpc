# Serial boost build that is dependent on compiler toolchain

%define _unpackaged_files_terminate_build 0
%define build_mpi 0

#-fsp-header-comp-begin-----------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu   }
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
## Toolsets supported by boost script are:
##     acc, como, darwin, gcc, intel-darwin, intel-linux, kcc, kylix,
##     mipspro, mingw(msys), pathscale, pgi, qcc, sun, sunpro, tru64cxx, vacpp
%define toolset gcc 
%endif

%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers%{PROJ_DELIM}
## Toolsets supported by boost script are:
##     acc, como, darwin, gcc, intel-darwin, intel-linux, kcc, kylix,
##     mipspro, mingw(msys), pathscale, pgi, qcc, sun, sunpro, tru64cxx, vacpp
%define toolset intel-linux  
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif


#-fsp-header-comp-end-------------------------------


#Added FSP build convention
%define debug_package %{nil}
%define openmp        1

%define ver 1.57.0
%define bversion 1_57_0
%define short_version 1_57
%define lib_appendix 1_57_0


# Base package name
%define pname boost
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Summary:	Boost free peer-reviewed portable C++ source libraries
Name:		%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        1.57.0
Release:        0
License:        BSL-1.0
Group:		System Environment/Libraries
Url:            http://www.boost.org
Source0:	%{pname}_%{bversion}.tar.gz 
Source1:        boost-rpmlintrc
Source100:      baselibs.conf
Source101:	FSP_macros
Source102:	FSP_setup_compiler

BuildRequires:  libbz2-devel
BuildRequires:  libexpat-devel
BuildRequires:  libicu-devel >= 4.4
BuildRequires:  python-devel
BuildRequires:  xorg-x11-devel
BuildRequires:  zlib-devel

#!BuildIgnore: post-build-checks rpmlint-Factory

#FSP build root
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
#BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%include %{_sourcedir}/FSP_macros
#__Recommends:     %{all_libs}

#__%define _docdir %{_datadir}/doc/packages/boost-%{version}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{pname}/%version


%description
Boost provides free peer-reviewed portable C++ source libraries. The
emphasis is on libraries that work well with the C++ Standard Library.
One goal is to establish "existing practice" and provide reference
implementations so that the Boost libraries are suitable for eventual
standardization. Some of the libraries have already been proposed for
inclusion in the C++ Standards Committee's upcoming C++ Standard
Library Technical Report.

Although Boost was begun by members of the C++ Standards Committee
Library Working Group, membership has expanded to include nearly two
thousand members of the C++ community at large.

This package is mainly needed for updating from a prior version, the
dynamic libraries are found in their respective package. For development
using Boost, you also need the boost-devel package. For documentation,
see the boost-doc package.


%prep
%setup -q -n %{pname}_%{bversion} -b 3
#everything in the tarball has the executable flag set ...
find -type f ! \( -name \*.sh -o -name \*.py -o -name \*.pl \) -exec chmod -x {} +
#stupid build machinery copies .orig files
find . -name \*.orig -exec rm {} +

%build
# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

# End FSP #####################

find . -type f -exec chmod u+w {} +


# use supplied bootstrap.sh instead of mucking with old bjam
# see also: https://svn.boost.org/trac/boost/ticket/9304
LIBRARIES_FLAGS=--with-libraries=all
./bootstrap.sh $LIBRARIES_FLAGS --prefix=%{install_path} --with-toolset=%{toolset} || cat config.log


# perform the compilation
./b2 --a  %{?_smp_mflags} --prefix=%{install_path} --threading=multi || config.log


%install

# FSP compiler/mpi designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler


./b2 %{?_smp_mflags} install --prefix=%{buildroot}/%{install_path} --threading=multi 


# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain"
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version             %{version}
 
prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "boost"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{FSP_HOME}


%changelog
