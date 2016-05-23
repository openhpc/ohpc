#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros

#-ohpc-header-comp-end------------------------------------------------

# Base package name
%define pname cubelib
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name: %{pname}%{PROJ_DELIM}

Version:   4.4
Release:   1%{?dist}
Summary:   General purpose C++ library for Score-P and Scalasca performance profiling.
License:   BSD-style license
Group:     ohpc/perf-tools
Url:       http://www.scalasca.org/software/cube-4.x/download.html
Source0:   http://apps.fz-juelich.de/scalasca/releases/cube/4.4/dist/cubelib-%{version}-TP1.tar.gz
Provides:  lib%PNAME.so()(64bit)
Provides:  cubelib
Conflicts: lib%pname < %version-%release
Obsoletes: lib%pname < %version-%release
DocDir:    %{OHPC_PUB}/doc/contrib


BuildRequires: zlib-devel
Requires: zlib
#BuildRequires: zlib-devel Rscript Rcpp Rinsign

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_PUB}/%{pname}/%version

%description
General purpose library written in C++ to generate and to real Scalasca's and 
Score-P's profiles.


%prep
%setup -q -n %{pname}-%{version}-TP1


%build

# module load qt

export BUILDROOT=%buildroot%{install_path}
./configure \
    -prefix=/tmp/%{install_path} \

make install
#make exports


rm -rf %buildroot
mkdir -p %buildroot%{install_path}
pushd /tmp
export tmp_path=%{install_path}
mv ${tmp_path#*/} %buildroot%{install_path}/..
popd
pushd %{buildroot}%{install_path}/bin
# ks 2/25/16 - following line breaks, commenting out.
# sed -i 's|/tmp/||g' $(egrep -IR '/tmp/' ./|awk -F : '{print $1}')
popd


rm -rf %{install_path}/examples
rm -rf %buildroot%{install_path}/examples
rm -f %{install_path}/.last_config
rm -f %{install_path}/.all_configs
rm -f %{install_path}/.active_stub*


# clean libs
pushd %buildroot%{install_path}/lib
popd


# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

    puts stderr " "
    puts stderr "This module loads the Cube C++ Library."
    puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: performance tools"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_HOME}
%{OHPC_PUB}
%doc ChangeLog AUTHORS COPYING INSTALL README*

%changelog
