#----------------------------------------------------------------------------bh-

# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%define no_ohpc_annobin 1
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros
%undefine _annotated_build

%define pname annobin

Name:    %{pname}-%{compiler_family}%{PROJ_DELIM}
Summary: Binary annotation plugin for GCC
Version: 8.69
Release: 1%{?dist}
License: GPLv3+
Group:   %{PROJ_NAME}/dev-tools
URL:     https://fedoraproject.org/wiki/Toolchain/Watermark
Source:  https://nickc.fedorapeople.org/annobin-%{version}.tar.xz
BuildRequires: gmp-devel
BuildRequires: binutils-devel
BuildRequires: rpm-devel
%if 0%{?rhel_version}
BuildRequires: gcc-plugin-devel
BuildRequires: elfutils-devel
%endif
%if 0%{?sle_version}
BuildRequires: libdwarf-devel
BuildRequires: libdw-devel
%endif

%description
Provides a plugin for GCC that records extra information in the files
that it compiles and a set of scripts that can analyze the recorded
information.

Note - the plugin is automatically enabled in gcc builds via flags
provided by the redhat-rpm-macros package.

%define install_path %{OHPC_COMPILERS}/annobin/%{version}


%prep
%setup -q -n annobin-%{version}
# The plugin has to be configured with the same arcane configure
# scripts used by gcc.  Hence we must not allow the Fedora build
# system to regenerate any of the configure files.
touch aclocal.m4 plugin/config.h.in
touch configure */configure Makefile.in */Makefile.in
# Similarly we do not want to rebuild the documentation.
touch doc/annobin.info


%build
%ohpc_setup_compiler
export ANNOBIN_PLUGIN_DIR=$(gcc --print-file-name=plugin)
mkdir BUILDTMP
export CFLAGS="$RPM_OPT_FLAGS"
export CXXFLAGS="%{optflags}" 
%if 0%{?sle_version}
export CFLAGS="$CFLAGS -I/usr/include/libdwarf"
export CXXFLAGS="$CXXFLAGS -I/usr/include/libdwarf" 
%endif

# Bootstrap build with OS-provided gcc and annobin
./configure --prefix=%{install_path} \
            --libdir=%{install_path}/lib \
            --with-gcc_plugin-dir=$ANNOBIN_PLUGIN_DIR
make %{_smp_mflags}

# Store a copy of the new annobin library
%{__mv} plugin/.libs/annobin.so.0.0.0 BUILDTMP/annobin.so

# Rebuild the plugin, this time using the plugin itself!  This
# ensures that the plugin works, and that it contains annotations
# of its own. 

make -C plugin clean

export ANNOFLAGS="-fplugin=annobin -iplugindir=$(pwd)/BUILDTMP -fplugin-arg-annobin-rename -fplugin-arg-annobin-verbose"
export CFLAGS="$CFLAGS $ANNOFLAGS"
export CXXFLAGS="$CXXFLAGS $ANNOFLAGS"
make -C plugin

%install
make DESTDIR=$RPM_BUILD_ROOT install
%{__rm} -f %{buildroot}%{_infodir}/dir

%files
%{OHPC_COMPILERS}
%doc COPYING3 LICENSE
