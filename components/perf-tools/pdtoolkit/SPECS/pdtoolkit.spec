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
%ohpc_compiler

# Base package name
%define pname pdtoolkit
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name: %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        3.23
Release:        1%{?dist}
License:        Program Database Toolkit License
Summary:        PDT is a framework for analyzing source code
Url:            http://www.cs.uoregon.edu/Research/pdt
Group:          %{PROJ_NAME}/perf-tools
Source:         https://www.cs.uoregon.edu/research/paracomp/pdtoolkit/Download/pdtoolkit-%{version}.tar.gz
Source1:        OHPC_macros
Provides:       %{name} = %{version}%{release}
Provides:       %{name} = %{version}

#define _unpackaged_files_terminate_build      0
#define __check_files /bin/true

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version


%description
Program Database Toolkit (PDT) is a framework for analyzing source code written in several programming languages and for making rich program knowledge accessible to developers of static and dynamic analysis tools. PDT implements a standard program representation, the program database (PDB), that can be accessed in a uniform way through a class library supporting common PDB operations.

%prep
%setup -q -n %{pname}-%{version}


%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

./configure -prefix=%buildroot%{install_path} \
        -useropt=-fPIC \
%if "%{compiler_family}" == "intel"
        -icpc
%else
        -GNU
%endif

export DONT_STRIP=1
make %{?_smp_mflags}

%install
%ohpc_setup_compiler

./configure -prefix=%buildroot%{install_path} \
%if "%{compiler_family}" == "intel"
        -icpc
%else
        -GNU
%endif

export DONT_STRIP=1
make %{?_smp_mflags} install

rm -f %buildroot%{install_path}/craycnl
rm -f %buildroot%{install_path}/mic_linux
rm -f %buildroot%{install_path}/sparc64fx
rm -f %buildroot%{install_path}/xt3
rm -f %buildroot%{install_path}/contrib/rose/roseparse/config.log
rm -f %buildroot%{install_path}/contrib/rose/roseparse/config.status
rm -f %buildroot%{install_path}/contrib/rose/edg44/x86_64/roseparse/config.log
rm -f %buildroot%{install_path}/contrib/rose/edg44/x86_64/roseparse/config.status
rm -f %buildroot%{install_path}/.all_configs
rm -f %buildroot%{install_path}/.last_config

%ifarch aarch64
%define arch_dir arm64_linux
%else
%define arch_dir x86_64
%endif

pushd %buildroot%{install_path}/%{arch_dir}/lib
ar x libpdb.a
$CXX -z muldefs -shared -o libpdb.so *.o
rm libpdb.a *\.o
popd

pushd %buildroot%{install_path}/%{arch_dir}/bin
sed -i 's|%{buildroot}||g' $(egrep -IR '%{buildroot}' ./|awk -F : '{print $1}')
rm -f edg33-upcparse
%ifarch x86_64
ln -s ../../contrib/rose/roseparse/upcparse edg33-upcparse
sed -i 's|%buildroot||g' ../../contrib/rose/roseparse/upcparse
%endif
rm -f edg44-c-roseparse
%ifnarch aarch64
ln -s  ../../contrib/rose/edg44/%{arch_dir}/roseparse/edg44-c-roseparse
sed -i 's|%buildroot||g' ../../contrib/rose/edg44/%{arch_dir}/roseparse/edg44-c-roseparse
%endif
rm -f edg44-cxx-roseparse
%ifnarch aarch64
ln -s  ../../contrib/rose/edg44/%{arch_dir}/roseparse/edg44-cxx-roseparse
sed -i 's|%buildroot||g' ../../contrib/rose/edg44/%{arch_dir}/roseparse/edg44-cxx-roseparse
%endif
rm -f edg44-upcparse
%ifnarch aarch64
ln -s  ../../contrib/rose/edg44/%{arch_dir}/roseparse/edg44-upcparse
sed -i 's|%buildroot||g' ../../contrib/rose/edg44/%{arch_dir}/roseparse/edg44-upcparse
%endif
rm -f pebil.static
ln -s  ../../contrib/pebil/pebil/pebil.static
rm -f roseparse
%ifarch x86_64
ln -s  ../../contrib/rose/roseparse/roseparse
sed -i 's|%buildroot||g' ../../contrib/rose/roseparse/roseparse
%endif
sed -i 's|/usr/local/bin/perl|/usr/bin/perl|g' ../../contrib/rose/rose-header-gen/config/depend.pl
sed -i 's|/usr/local/bin/perl|/usr/bin/perl|g' ../../contrib/rose/rose-header-gen/config/cmp.pl
rm -f ../../contrib/rose/rose-header-gen/config.log
rm -f ../../contrib/rose/rose-header-gen/config.status
rm -f smaqao
ln -s  ../../contrib/maqao/maqao/smaqao
popd
pushd %buildroot%{install_path}/%{arch_dir}
rm -f include
ln -s ../include
popd
install -d %buildroot%{install_path}/include
install -d %buildroot%{install_path}/lib
install -d %buildroot%{install_path}/man

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

set     version                     %{version}

prepend-path    PATH                %{install_path}/%{arch_dir}/bin
prepend-path    MANPATH             %{install_path}/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/%{arch_dir}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/%{arch_dir}/bin
setenv          %{PNAME}_LIB        %{install_path}/%{arch_dir}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%files
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc CREDITS LICENSE README

%changelog
* Wed Feb 22 2017 Adrian Reber <areber@redhat.com> - 3.22-1
- Switching to %%ohpc_compiler macro
