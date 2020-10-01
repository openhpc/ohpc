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

%define pname flex

Name:	  %{pname}%{PROJ_DELIM}
Version:  2.6.4
Release:  1%{?dist}
Summary:  Fast Lexical Analyzer Generator
Group:	  %{PROJ_NAME}/distro-packages
License:  BSD-3-Clause
URL:	  https://github.com/westes/flex
Source0:  https://github.com/westes/flex/releases/download/v%{version}/flex-%{version}.tar.gz

BuildRequires: gcc-c++
BuildRequires: help2man
BuildRequires: m4

# Install path
%define install_path %{OHPC_LIBS}/%{pname}%{OHPC_CUSTOM_PKG_DELIM}/%version

%description
FLEX is a tool for generating scanners: programs that recognize lexical
patterns in text.

%prep
%setup -q -n %{pname}-%{version}

%build
./configure CFLAGS="-D_GNU_SOURCE" \
       --prefix=%{install_path} \
       --disable-shared

make %{?_smp_mflags}

%install
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################
proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the flex lexical scanner"
puts stderr " "
puts stderr "Version %{version}"
puts stderr " "

}

module-whatis "Name: flex lexical scanner"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library and tools"
module-whatis "Description: tool and library for recognizing lexical patterns in text"
module-whatis "%{url}"

set     version                 %{version}


prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
set     ModulesVersion      "%{version}"
EOF


%files
%{OHPC_PUB}
%doc AUTHORS ChangeLog NEWS ONEWS README.md THANKS
%license COPYING