%include %{_sourcedir}/FSP_macros

%define pname valgrind
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

%if 0%{?PROJ_NAME:1}
%define rpmname %{pname}-%{PROJ_NAME}
%else
%define rpmname %{pname}
%endif

Summary:   Valgrind Memory Debugger
Name:      %{rpmname}
Version:   3.10.0
Release:   1
License:   GPL
URL:       http://www.valgrind.org/
Group:     Development/Debuggers
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
%setup -n valgrind-3.10.0

%build
./configure --prefix=%{install_path} || cat config.log
make %{?_smp_mflags}

%install
make install DESTDIR=$RPM_BUILD_ROOT

### makeinstall
### mkdir docs.installed
### mv $RPM_BUILD_ROOT%{_datadir}/doc/valgrind/* docs.installed/

# modulefile

%{__mkdir} -p %{buildroot}/%{FSP_MODULES}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/%{pname}
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

prepend-path    PATH            %{install_path}/bin
prepend-path    MANPATH         %{install_path}/share/man

setenv          %{PNAME}_DIR    %{install_path}
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
