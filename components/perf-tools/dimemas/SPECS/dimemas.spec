#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Build that is dependent on compiler/mpi toolchains
%define ohpc_compiler_dependent 1
%define ohpc_mpi_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname dimemas

Summary:	Dimemas tool
Name:		%{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:	5.6.0
Release:	1
License:	GNU
Group:		%{PROJ_NAME}/perf-tools
URL:		https://tools.bsc.es
Source0:	https://ftp.tools.bsc.es/dimemas/dimemas-%{version}-src.tar.bz2

BuildRequires: boost-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Requires:      boost-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
BuildRequires: bison make
%if 0%{?rhel} || 0%{?openEuler}
BuildRequires: flex-devel
BuildRequires: flex
%endif
%if 0%{?suse_version}
BuildRequires: libfl-devel
BuildRequires: flex%{PROJ_DELIM}
%endif
BuildRequires: autoconf
BuildRequires: automake
BuildRequires: libtool
#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{mpi_family}/%{pname}/%version

%description
Dimemas is a performance analysis tool for message-passing
programs. It enables the user to develop and tune parallel applications on a
workstation, while providing an accurate prediction of their performance on the
parallel target machine. The Dimemas simulator reconstructs the time behavior
of a parallel application on a machine modeled by a set of performance
parameters. Thus, performance experiments can be done easily. The supported
target architecture classes include networks of workstations, single and
clustered SMPs, distributed memory parallel computers, and even heterogeneous
systems.

%prep
%setup -q -n %{pname}-%{version}


%build
%ohpc_setup_compiler
module load boost

#./bootstrap

%if 0%{?suse_version}
module load flex
export LDFLAGS="-L$FLEX_LIB"
%endif
# Silence warnings from dimemas' own source; these should really be
# fixed upstream rather than papered over here.
export CFLAGS="${CFLAGS} -Wno-implicit-int"
export CFLAGS="${CFLAGS} -Wno-implicit-function-declaration"
export CFLAGS="${CFLAGS} -Wno-unused-variable"
export CFLAGS="${CFLAGS} -Wno-unused-but-set-variable"
export CFLAGS="${CFLAGS} -Wno-unused-function"
export CFLAGS="${CFLAGS} -Wno-maybe-uninitialized"
export CFLAGS="${CFLAGS} -Wno-parentheses"
export CFLAGS="${CFLAGS} -Wno-sign-compare"
export CFLAGS="${CFLAGS} -Wno-format-overflow"
export CFLAGS="${CFLAGS} -Wno-comment"
export CFLAGS="${CFLAGS} -Wno-format"
export CFLAGS="${CFLAGS} -Wno-logical-not-parentheses"
export CFLAGS="${CFLAGS} -Wno-missing-braces"
export CXXFLAGS="${CXXFLAGS} -Wno-register"
export CXXFLAGS="${CXXFLAGS} -Wno-narrowing"
export CXXFLAGS="${CXXFLAGS} -Wno-unused-variable"
export CXXFLAGS="${CXXFLAGS} -Wno-unused-but-set-variable"
export CXXFLAGS="${CXXFLAGS} -Wno-unused-function"
export CXXFLAGS="${CXXFLAGS} -Wno-maybe-uninitialized"
export CXXFLAGS="${CXXFLAGS} -Wno-parentheses"
export CXXFLAGS="${CXXFLAGS} -Wno-sign-compare"
export CXXFLAGS="${CXXFLAGS} -Wno-format-overflow"
export CXXFLAGS="${CXXFLAGS} -Wno-comment"
export CXXFLAGS="${CXXFLAGS} -Wno-format"
export CXXFLAGS="${CXXFLAGS} -Wno-logical-not-parentheses"
export CXXFLAGS="${CXXFLAGS} -Wno-missing-braces"
%if "%{compiler_family}" == "intel"
# icpx pulls in Intel oneAPI TBB via libstdc++'s <execution> PSTL
# glue, and TBB's headers use "T" as a template parameter name.
# dimemas' own extern.h defines a function-like macro also named
# "T", so the two collide and the build fails with errors like
# "too many arguments provided to function-like macro invocation".
# Disable the TBB-based parallel backend so those headers are never
# pulled in; this should really be fixed upstream by avoiding the
# name clash.
export CXXFLAGS="${CXXFLAGS} -D_GLIBCXX_USE_TBB_PAR_BACKEND=0"
# Silence more warnings from dimemas' own source, specific to
# clang-based icx/icpx; these should really be fixed upstream too.
export CFLAGS="${CFLAGS} -Wno-incompatible-pointer-types-discards-qualifiers"
export CFLAGS="${CFLAGS} -Wno-string-plus-int"
export CFLAGS="${CFLAGS} -Wno-tautological-constant-compare"
export CXXFLAGS="${CXXFLAGS} -Wno-incompatible-pointer-types-discards-qualifiers"
export CXXFLAGS="${CXXFLAGS} -Wno-string-plus-int"
export CXXFLAGS="${CXXFLAGS} -Wno-tautological-constant-compare"
%else
# icx/icpx (clang-based) do not recognize these GCC-specific warning
# names and warn "unknown warning option" for each one.
export CFLAGS="${CFLAGS} -Wno-discarded-qualifiers"
export CFLAGS="${CFLAGS} -Wno-stringop-truncation"
export CFLAGS="${CFLAGS} -Wno-multistatement-macros"
export CFLAGS="${CFLAGS} -Wno-bool-compare"
export CXXFLAGS="${CXXFLAGS} -Wno-stringop-truncation"
export CXXFLAGS="${CXXFLAGS} -Wno-multistatement-macros"
export CXXFLAGS="${CXXFLAGS} -Wno-bool-compare"
%endif
%if "%{compiler_family}" == "arm1"
export CFLAGS="${CFLAGS} -fsimdmath"
export CXXFLAGS="${CXXFLAGS} -fsimdmath"
%endif
%if "%{compiler_family}" == "gnu15" || "%{compiler_family}" == "gnu16"
export CFLAGS="${CFLAGS} -std=gnu17"
%endif

./configure --prefix=%{install_path} \
            --with-boost=$BOOST_DIR || cat config.log

make %{?_smp_mflags}

%install
export NO_BRP_CHECK_RPATH=true

# OpenHPC compiler designation
%ohpc_setup_compiler

make DESTDIR=$RPM_BUILD_ROOT install

# these are data files, not scripts; drop the executable bit upstream
# ships them with to avoid an rpmbuild "no shebang" warning
chmod -x $RPM_BUILD_ROOT%{install_path}/share/cfgs/*.cfg

# don't package static libs
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*.la
rm -f $RPM_BUILD_ROOT%{install_path}/lib/*.a
rm -f $RPM_BUILD_ROOT%{install_path}/lib/dimemas/*.la

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain and the %{mpi_family} MPI stack."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler and %{mpi_family} MPI"
module-whatis "Version: %{version}"
module-whatis "Category: performance tool"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}

# Require boost
depends-on boost

prepend-path    PATH                %{install_path}/bin

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_INC        %{install_path}/include

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}-%{mpi_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p $RPM_BUILD_ROOT/%{_docdir}

%files
%{OHPC_PUB}
%doc
