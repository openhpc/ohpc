#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Serial HDF5 library build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname hdf5

Summary:   A general purpose library and file format for storing scientific data
Name:      %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:   1.14.5
Release:   1%{?dist}
License:   Hierarchical Data Format (HDF) Software Library and Utilities License
Group:     %{PROJ_NAME}/io-libs
URL:       http://www.hdfgroup.org/HDF5
Source0:   https://github.com/HDFGroup/%{pname}/archive/refs/tags/%{pname}_%{version}.tar.gz

BuildRequires: zlib-devel make
BuildRequires: perl(File::Compare)
BuildRequires: perl(File::Copy)

%if "%{compiler_family}" == "intel"
BuildRequires: libtool%{PROJ_DELIM}
%endif

#!BuildIgnore: post-build-checks rpmlint-Factory

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
HDF5 is a general purpose library and file format for storing scientific data.
HDF5 can store two primary objects: datasets and groups. A dataset is
essentially a multidimensional array of data elements, and a group is a
structure for organizing objects in an HDF5 file. Using these two basic
objects, one can create and store almost any kind of scientific data
structure, such as images, arrays of vectors, and structured and unstructured
grids. You can also mix and match them in HDF5 files according to your needs.


%prep
%setup -q -n %{pname}-%{pname}_%{version}

%build

# override with newer config.guess for aarch64
%ifarch aarch64 || ppc64le
%if 0%{?rhel} >= 9
cp /usr/lib/rpm/redhat/config.guess bin
%else
cp /usr/lib/rpm/config.guess bin
%endif
%endif

# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "intel"
export PATH=%{OHPC_UTILS}/autotools/bin:${PATH}
autoreconf -if
sed -e 's/NO_SYMBOLS_CFLAGS="-Wl,-s"/NO_SYMBOLS_CFLAGS=/g' -i config/intel-flags
sed -e 's/NO_SYMBOLS_CFLAGS="-Wl,-s"/NO_SYMBOLS_CFLAGS=/g' -i config/intel-cxxflags
# delete special flags no longer available
echo "" > config/intel-warnings/classic/18
sed '/-Wp64/d' -i config/intel-warnings/classic/15
%endif

./configure --prefix=%{install_path} \
	    --enable-fortran         \
            --enable-static=no       \
	    --enable-shared          \
	    --enable-cxx            || { cat config.log && exit 1; }

%if "%{compiler_family}" == "llvm" || "%{compiler_family}" == "arm1" || "%{compiler_family}" == "intel"
%{__sed} -i -e 's#wl=""#wl="-Wl,"#g' libtool
%{__sed} -i -e 's#pic_flag=""#pic_flag=" -fPIC -DPIC"#g' libtool
%endif

make %{?_smp_mflags}

%install

# OpenHPC compiler designation
%ohpc_setup_compiler

export NO_BRP_CHECK_RPATH=true

make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# Remove static libraries
find "%buildroot" -type f -name "*.la" | xargs rm -f
find "%buildroot"

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "hdf5"

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_PUB}
%doc COPYING
%doc README.md
