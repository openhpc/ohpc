#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Python build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname python

Name:           %{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        2.7.12
Release:        70.1%{?dist}
Summary:        Python Interpreter
License:        Python-2.0
Group:          %{PROJ_NAME}/distro-packages
Url:            http://www.python.org/
%define         tarversion %{version}
%define         tarname Python-%{tarversion}
Source0:        http://www.python.org/ftp/python/%{version}/%{tarname}.tar.xz
Source4:        http://www.python.org/ftp/python/%{version}/%{tarname}.tar.xz.asc
Source6:        python.keyring
Source1:        macros.python
Source2:        baselibs.conf
Source5:        local.pth
# COMMON-PATCH-BEGIN
Patch1:         python-2.7-dirs.patch
Patch2:         python-distutils-rpm-8.patch
Patch3:         python-2.7.5-multilib.patch
Patch4:         python-2.5.1-sqlite.patch
Patch5:         python-2.7.4-canonicalize2.patch
Patch7:         python-2.6-gettext-plurals.patch
Patch8:         python-2.6b3-curses-panel.patch
Patch10:        sparc_longdouble.patch
Patch13:        python-2.7.2-fix_date_time_compiler.patch
Patch17:        remove-static-libpython.diff
# PATCH-FEATURE-OPENSUSE python-bundle-lang.patch bnc#617751 dimstar@opensuse.org -- gettext: when looking in default_localedir also check in locale-bundle.
Patch20:        python-bundle-lang.patch
# PATCH-FIX-UPSTREAM Fix argument passing in libffi for aarch64
Patch22:        python-2.7-libffi-aarch64.patch
Patch24:        python-bsddb6.diff
# PATCH-FIX-UPSTREAM accept directory-based CA paths as well
Patch33:        python-2.7.9-ssl_ca_path.patch
# PATCH-FEATURE-SLE disable SSL verification-by-default in http clients
Patch34:        python-2.7.9-sles-disable-verification-by-default.patch
# PATCH-FIX-UPSTREAM python-ncurses-6.0-accessors.patch dimstar@opensuse.org -- Fix build with NCurses 6.0 and OPAQUE_WINDOW set to 1
Patch35:        python-ncurses-6.0-accessors.patch
Patch36:        python-2.7.10-overflow_check.patch
Patch37:        python-2.7.12-makeopcode.patch
# COMMON-PATCH-END
%define         python_version    %(echo %{tarversion} | head -c 3)
BuildRequires:  automake
%if 0%{?sle_version} || 0%{?suse_version}
BuildRequires:  fdupes
BuildRequires:  netcfg
BuildRequires:  libbz2-devel
BuildRequires:  pkg-config
%else
BuildRequires:  bzip2-devel
BuildRequires:  pkgconfig
%endif
BuildRequires:  xz
BuildRequires:  zlib-devel
Requires:       glibc-devel
# for the test suite
# explicitly, see bnc#697251:
# Requires:       libpython2_7-1_0 = %{version}
Provides:       %{name} = %{python_version}
# bug437293
%ifarch ppc64
Obsoletes:      python-64bit
%endif
Provides:       python-ctypes = 1.1.0
Obsoletes:      python-ctypes < 1.1.0

#!BuildIgnore:  post-build-checks
%global _python_bytecompile_errors_terminate_build 0
# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
Python is an interpreted, object-oriented programming language, and is
often compared to Tcl, Perl, Scheme, or Java.  You can find an overview
of Python in the documentation and tutorials included in the python-doc
(HTML) or python-doc-pdf (PDF) packages.

This package contains all of stand-alone Python files, minus binary
modules that would pull in extra dependencies.


%prep
%setup -q -n %{tarname}
# patching
# COMMON-PREP-BEGIN
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1
%patch5 -p1
%patch7 -p1
%patch8 -p1
%patch10 -p1
%patch13 -p1
%patch17 -p1
%patch20 -p1
%patch22 -p1
%patch24 -p1
%patch33 -p1
# %if %{suse_version} == 1315 && !0%{?is_opensuse}
# %patch34 -p1
# %endif
%patch35 -p1
%patch36
%patch37 -p1

# drop Autoconf version requirement
sed -i 's/^version_required/dnl version_required/' configure.ac
# COMMON-PREP-END

%build
# OpenHPC compiler designation
%ohpc_setup_compiler
export OPT="%{optflags} -DOPENSSL_LOAD_CONF"

autoreconf -f -i . # Modules/_ctypes/libffi

# provide a stable timestamp
touch -r %{SOURCE0} Makefile.pre.in

# prevent make from trying to rebuild asdl stuff, which requires existing
# python installation
touch Parser/asdl* Python/Python-ast.c Include/Python-ast.h

./configure \
    --prefix=%{install_path} \
    --libdir=%{install_path}/lib \
    --with-fpectl \
    --enable-ipv6 \
    --enable-shared \
    --enable-unicode=ucs4

%if 0%{?do_profiling}
target=profile-opt
%else
target=all
%endif
LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH \
    make %{?_smp_mflags} $target

%check
# on hppa, the threading of glibc is quite broken. The tests just stop
# at some point, and the machine does not build anything more until a
# timeout several hours later.
%ifnarch hppa
# test_file(2k) fails in autobuild env - "stdin.seek(-1)" wrongly succeeds. probably an issue with autobuild's stdin
# test_urllib2 relies on being able to resolve local address, which is notoriously impossible in autobuild
EXCLUDE="test_urllib2 test_file test_file2k"
# test_nis and test_threading are AWFULLY slow.
EXCLUDE="$EXCLUDE test_nis test_threading"
# test_gdb fails if gdb with (different) python support is part of the buildsystem
EXCLUDE="$EXCLUDE test_gdb"
%ifarch ia64
# test_smtplib's testSend is known to be broken and on ia64 it actually fails most of the time, preventing the build.
EXCLUDE="$EXCLUDE test_smtplib"
%endif
# test_unicode fails in Factory
EXCLUDE="$EXCLUDE test_unicode"
%if 0%{?qemu_user_space_build}
# test_asyncore fails because of unimplemented sockopt
EXCLUDE="$EXCLUDE test_asyncore test_mmap"
# emulation is unreliable
EXCLUDE="$EXCLUDE test_multiprocessing test_thread"
# qemu bug (siginterrupt handling)
EXCLUDE="$EXCLUDE test_signal"
%endif

# This test (part of test_uuid) requires real network interfaces
# so that ifconfig output has "HWaddr <something>".  Some kvm instances
# don't have any such interface breaking the uuid module test.
EXCLUDE="$EXCLUDE test_uuid"

# Limit virtual memory to avoid spurious failures
if test $(ulimit -v) = unlimited || test $(ulimit -v) -gt 10000000; then
  ulimit -v 10000000 || :
fi
# make test TESTOPTS="-l -x $EXCLUDE" TESTPYTHONOPTS="-R"
# use network, be verbose:
#make test TESTOPTS="-l -u network -v"
%endif

%install
# OpenHPC compiler designation
%ohpc_setup_compiler
# replace rest of /usr/local/bin/python or /usr/bin/python2.5 with /usr/bin/python
find . -name '*.py' -type f | grep -vE "^./Parser/|^./Python/" \
  | xargs grep -lE '^#! *(/usr/.*bin/(env +)?)?python' \
  | xargs sed -r -i -e '1s@^#![[:space:]]*(/usr/(local/)?bin/(env +)?)?python([0-9]+\.[0-9]+)?@#!/usr/bin/python@'
# the grep inbetween makes it much faster
########################################
# install it
########################################
make DESTDIR=$RPM_BUILD_ROOT install OPT="%{optflags} -fPIC"
mkdir -p %{buildroot}%{install_path}/bin
mkdir -p %{buildroot}%{install_path}/include
mkdir -p %{buildroot}%{install_path}/share/man/man1
mkdir -p %{buildroot}%{install_path}/%{_lib}/python%{python_version}/site-packages
install -m 644 %{SOURCE5} %{buildroot}%{install_path}/%{_lib}/python%{python_version}/site-packages/_local.pth
install -d -m 755 %{buildroot}%{_sysconfdir}/rpm
install -m 644 %{SOURCE1} %{buildroot}%{_sysconfdir}/rpm
# make sure /usr/lib/python/site-packages exists even on lib64 machines
mkdir -p %{buildroot}%{install_path}/lib/python%{python_version}/site-packages
########################################
# some cleanups
########################################
# remove rpm macros
rm -f %{buildroot}/etc/rpm/macros.python
# remove hard links and replace them with symlinks
for dir in bin include %{_lib} ; do
    rm -f %{buildroot}/%{install_path}/$dir/python
    ln -s python%{python_version} %{buildroot}/%{install_path}/$dir/python
done
CLEANUP_DIR="%{buildroot}%{install_path}/%{_lib}/python%{python_version}"
# don't distribute precompiled windows installers (duh)
rm -f $CLEANUP_DIR/distutils/command/*.exe
# kill imageop.so - it used to be insecure and it is deprecated anyway
rm -f $CLEANUP_DIR/lib-dynload/imageop.so
# # link shared library instead of static library that tools expect
mkdir -p %{buildroot}%{install_path}/%{_lib}/python%{python_version}/config
# ln -s ../../libpython%{python_version}.so %{buildroot}%{install_path}/%{_lib}/python%{python_version}/config/libpython%{python_version}.so
# remove various things that don't need to be in python
rm %{buildroot}%{install_path}/bin/idle
rm -rf $CLEANUP_DIR/{curses,bsddb,idlelib,lib-tk,sqlite3}
rm -f $CLEANUP_DIR/ssl.py*
#        does not work without _ssl.so anyway
%if 0%{?sles_version} || 0%{?suse_version}
# replace duplicate .pyo/.pyc with hardlinks
%fdupes %{buildroot}/%{install_path}/%{_lib}/python%{python_version}
%endif
########################################
# documentation
########################################
ln -s python%{python_version}.1.gz %{buildroot}%{install_path}/share/man/man1/python.1.gz
########################################
# devel
########################################
# install Makefile.pre.in and Makefile.pre
cp Makefile Makefile.pre.in Makefile.pre %{buildroot}%{install_path}/%{_lib}/python%{python_version}/config/

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

        puts stderr " "
        puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
        puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set             version         %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    PYTHONPATH          %{install_path}/lib/python2.7:%{install_path}/lib/python2.7/lib-dynload:/usr/lib64/python2.7/site-packages:/usr/lib/python2.7/site-packages
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          PYTHONHOME          %{install_path}
setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
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
%{OHPC_HOME}

%{OHPC_PUB}
%doc LICENSE README
