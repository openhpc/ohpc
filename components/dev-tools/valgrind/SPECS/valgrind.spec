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

%define pname valgrind

Summary:   Valgrind Memory Debugger
Name:      %{pname}%{PROJ_DELIM}
Version:   3.15.0
Release:   1%{?dist}
License:   GPL
URL:       http://www.valgrind.org/
Group:     %{PROJ_NAME}/dev-tools
Source:    https://sourceware.org/pub/%{pname}/%{pname}-%{version}.tar.bz2

# Default library install path
%define install_path %{OHPC_UTILS}/%{pname}/%version

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
./configure --prefix=%{install_path} --libdir=%{install_path}/lib || { cat config.log && exit 1; }
make %{?_smp_mflags}

%install
make install DESTDIR=$RPM_BUILD_ROOT

# modulefile

%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

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

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_HOME}
%doc AUTHORS
%doc README_DEVELOPERS
%doc README
%doc COPYING.DOCS
%doc README_PACKAGERS
%doc README_MISSING_SYSCALL_OR_IOCTL
%doc FAQ.txt
%doc NEWS
%doc COPYING
