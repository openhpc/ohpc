#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# If building annobin the first time for a new compiler set this to 1
%define bootstrap_annobin 0
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros
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
BuildRequires: elfutils-devel
BuildRequires: binutils-devel
BuildRequires: rpm-devel

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

#---------------------------------------------------------------------------------

%build
%ohpc_setup_compiler
export ANNOBIN_PLUGIN_DIR=`gcc --print-file-name=plugin`
CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%{install_path} --quiet --with-gcc-plugin-dir=$ANNOBIN_PLUGIN_DIR
make %{_smp_mflags} CFLAGS="$RPM_OPT_FLAGS"
# Rebuild the plugin, this time using the plugin itself!  This
# ensures that the plugin works, and that it contains annotations
# of its own.  This could mean that we end up with a plugin with
# double annotations in it.  (If the build system enables annotations
# for plugins by default).  I have not tested this yet, but I think
# that it should be OK.
cp plugin/.libs/annobin.so.0.0.0 %{_tmppath}/tmp_annobin.so
make -C plugin clean
make -C plugin CXXFLAGS="%{optflags} -fplugin=%{_tmppath}/tmp_annobin.so -fplugin-arg-tmp_annobin-rename"
rm %{_tmppath}/tmp_annobin.so

%install
make DESTDIR=$RPM_BUILD_ROOT install
%{__rm} -f %{buildroot}%{_infodir}/dir

%files
%{OHPC_COMPILERS}
%doc COPYING3 LICENSE
