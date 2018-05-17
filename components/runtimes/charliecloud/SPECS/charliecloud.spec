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
Version:   0.2.4
Release:   %{?dist}
License:   Apache-2.0
Group:     %{PROJ_NAME}/runtimes
URL:       https://hpc.github.io/charliecloud/
Source0:   https://github.com/hpc/charliecloud/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar.gz
Source1:   OHPC_macros
Patch1:    charliecloud-language_highlight.patch
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

BuildRequires: python 
BuildRequires: rsync
BuildRequires: python-sphinx_rtd_theme 
%if 0%{?suse_version}
BuildRequires: python-Sphinx 
%else
BuildRequires: python-sphinx
%endif

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
Charliecloud provides user-defined software stacks (UDSS) for
high-performance computing (HPC) centers.

%prep
%setup -q -n %{pname}-%{version}
find doc-src -type f -print0 | xargs -0 sed -i '/.*:language: docker.*/d'
%patch1 -p1

%build
%{__make} %{?mflags}
%{__make} -C doc-src %{?mflags}
mv doc html

%install
PREFIX=%{install_path} DESTDIR=$RPM_BUILD_ROOT %{__make} install %{?mflags_install}

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

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
%doc LICENSE README.rst examples
%{OHPC_PUB}

%changelog

