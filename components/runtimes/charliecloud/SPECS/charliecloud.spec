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

# Base package name
%define pname charliecloud

Summary:   Lightweight user-defined software stacks for high-performance computing
Name:      %{pname}%{PROJ_DELIM}
Version:   0.9.7
Release:   1%{?dist}
License:   Apache-2.0
Group:     %{PROJ_NAME}/runtimes
URL:       https://hpc.github.io/charliecloud/
Source0:   https://github.com/hpc/charliecloud/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Source1:   ch-build.1
Source2:   ch-build2dir.1
Source3:   ch-docker2tar.1
Source4:   ch-fromhost.1
Source5:   ch-pull2dir.1
Source6:   ch-pull2tar.1
Source7:   ch-run.1
Source8:   ch-ssh.1
Source9:   ch-tar2dir.1
Source10:  charliecloud.1
Patch1:    charliecloud-language_highlight.patch
Patch2:    charliecloud-test-build.patch

BuildRequires: python
BuildRequires: rsync

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%description
Charliecloud provides user-defined software stacks (UDSS) for
high-performance computing (HPC) centers.

%prep
%setup -q -n %{pname}-%{version}
find doc-src -type f -print0 | xargs -0 sed -i '/.*:language: docker.*/d'
%patch1 -p1
%patch2 -p1

%build
%{__make} %{?mflags}

%install
PREFIX=%{install_path} DESTDIR=$RPM_BUILD_ROOT %{__make} install %{?mflags_install}

# install externally generated man pages
%{__mkdir} -p %{buildroot}%{install_path}/share/man/man1
%{__install} -m644 %{_sourcedir}/ch*1 %{buildroot}%{install_path}/share/man/man1

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

        puts stderr " "
        puts stderr "This module loads the %{pname} utility"
        puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: runtime"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%doc LICENSE README.rst examples
%{OHPC_PUB}
