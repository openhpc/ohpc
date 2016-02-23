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
%define pname jcuber
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])


Name: %{pname}%{PROJ_DELIM}

Version:   4.3.3
Release:   1%{?dist}
Summary:   Score-P and Scalasca performance report reader written in native Java
License:   BSD-style license
Group:     ohpc/perf-tools
Url:       http://www.scalasca.org/software/cube-4.x/download.html
Source0:   http://apps.fz-juelich.de/scalasca/releases/cube/4.3/dist/jcuber-%{version}.tar.gz
Provides:  CubeReader.jar
Provides:  CubeReader.jar
Conflicts: lib%pname < %version-%release
Obsoletes: lib%pname < %version-%release
DocDir:    %{OHPC_PUB}/doc/contrib


BuildRequires: javac jar
Requires: java 
BuildRequires: javac jar

%define debug_package %{nil}

# Default library install path
%define install_path %{OHPC_PUB}/%{pname}/%version

%description
Cube profile reader library written in native Java.


%prep
%setup -q -n %{pname}-%{version}


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
sed -i 's|/tmp/||g' $(egrep -IR '/tmp/' ./|awk -F : '{print $1}')
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
    puts stderr "This module loads the Cube performance reporter tool."
    puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: performance tools"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version                     %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    CLASSPATH           %{install_path}/lib

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
