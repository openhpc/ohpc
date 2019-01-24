#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# scotch - Graph, mesh and hypergraph partitioning library (serial version)
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

%define pname scotch

Name:		%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:	6.0.6
Release:	1%{?dist}
Summary:	Graph, mesh and hypergraph partitioning library
License:	CeCILL-C
Group:		%{PROJ_NAME}/serial-libs
URL:		http://www.labri.fr/perso/pelegrin/%{pname}/
Source0:	https://gforge.inria.fr/frs/download.php/file/37622/scotch_6.0.6.tar.gz
Source1:	%{pname}-Makefile.%{compiler_family}.inc.in
Source2:	%{pname}-rpmlintrc
Patch0:     scotch-6.0.4-destdir.patch

BuildRequires:	flex bison
%if 0%{?suse_version} >= 1100
BuildRequires:  libbz2-devel
Requires:       libbz2-devel
BuildRequires:  zlib-devel
%else
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  bzip2
Requires:       bzip2
BuildRequires:  zlib-devel
%else
BuildRequires:  bzip2-devel
Requires:       bzip2-devel
BuildRequires:  zlib-devel
%endif
%endif

%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
Scotch is a software package for graph and mesh/hypergraph partitioning and
sparse matrix ordering.

%prep
%setup -q -n %{pname}_%{version}
%patch0 -p1
sed s/@RPMFLAGS@/'%{optflags} -fPIC'/ < %{SOURCE1} > src/Makefile.inc

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

pushd src
make %{?_smp_mflags}
popd

%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

pushd src
make prefix=%{buildroot}%{install_path} install

# make dynamic, remove static linkings
pushd %{buildroot}%{install_path}/lib
for static_lib in *.a; do \
    lib=`basename $static_lib .a`; \
    ar x $static_lib; \
    ${CC} -shared -Wl,-soname=$lib.so -o $lib.so *.o; \
    rm $static_lib *\.o; \
done; \
popd
install -d %{buildroot}%{install_path}/lib
popd

# Convert the license files to utf8
pushd doc
iconv -f iso8859-1 -t utf-8 < CeCILL-C_V1-en.txt > CeCILL-C_V1-en.txt.conv
iconv -f iso8859-1 -t utf-8 < CeCILL-C_V1-fr.txt > CeCILL-C_V1-fr.txt.conv
mv -f CeCILL-C_V1-en.txt.conv CeCILL-C_V1-en.txt
mv -f CeCILL-C_V1-fr.txt.conv CeCILL-C_V1-fr.txt
popd

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
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
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%files
%doc README.txt ./doc/*
%{OHPC_PUB}
