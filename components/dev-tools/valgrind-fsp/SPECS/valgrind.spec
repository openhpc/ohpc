%include %{_sourcedir}/FSP_macros

%define pname valgrind
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Summary:   Valgrind Memory Debugger
Name:      %{pname}%{PROJ_DELIM}
Version:   3.10.1
Release:   1
License:   GPL
URL:       http://www.valgrind.org/
Group:     fsp/dev-tools
Source:    valgrind-%{version}.tar.bz2
Source1:   FSP_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

# Default library install path
%define install_path %{FSP_PUB}/%{pname}/%version

%description 

Valgrind is an award-winning instrumentation framework for building dynamic
analysis tools. There are Valgrind tools that can automatically detect many
memory management and threading bugs, and profile your programs in detail. You
can also use Valgrind to build new tools.  Valgrind runs on the following
platforms: x86/Linux, AMD64/Linux, PPC32/Linux, PPC64/Linux, x86/MacOSX,
AMD64/MacOSX.

%prep
%setup -q -n %{pname}-%{version}

%build
./configure --prefix=%{install_path} || cat config.log
make %{?_smp_mflags}

%install
make install DESTDIR=$RPM_BUILD_ROOT

# modulefile

%{__mkdir} -p %{buildroot}/%{FSP_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################
# FSP %{pname} environment
#############################################################################

proc ModulesHelp { } {
puts stderr "This module loads the %{pname} package for performing dynamic analysis."
puts stderr " "
}

module-whatis "Name: Valgrind Memory Debugger"
module-whatis "Version: %{version}"
module-whatis "Category: utility, developer support"
module-whatis "Keywords: Debugging"
module-whatis "Description: Memory debugging utilities"

prepend-path    PATH             %{install_path}/bin
prepend-path    MANPATH          %{install_path}/share/man
prepend-path    PKG_CONFIG_PATH  %{install_path}/lib/pkgconfig
prepend-path    LD_LIBRARY_PATH  %{install_path}/lib/pkgconfig

setenv          %{PNAME}_DIR     %{install_path}
setenv          %{PNAME}_LIB     %{install_path}/lib/valgrind
setenv          %{PNAME}_INC     %{install_path}/include
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%defattr(-,root,root)
%{FSP_HOME}

%clean
rm -rf $RPM_BUILD_ROOT
