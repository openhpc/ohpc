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

%define pname sigar

Name:		%{pname}%{PROJ_DELIM}
Version:	1.6.5
Release:	0.11.git58097d9%{?dist}
Summary:	System Information Gatherer And Reporter

%global sigar_suffix  0-g4b67f57
%global sigar_hash    ad47dc3b494e9293d1f087aebb099bdba832de5e

Group:		%{PROJ_NAME}/distro-packages
License:	ASL 2.0
URL:		http://sigar.hyperic.com/
Provides:       %{pname}

# Once 1.6.5 is released, we can use tarballs from GitHub:
#    Source0:	http://download.github.com/hyperic-sigar-{name}-{version}-{sigar_suffix}.tar.gz
#
# Until then the tarball can be re-generated with:
#    git clone git://github.com/hyperic/sigar.git
#    cd sigar
#    git archive --prefix=sigar-1.6.5/ 833ca18 | bzip2 > sigar-1.6.5-833ca18.tbz2
#
# The diff from 1.6.4 is too huge to contemplate cherrypicking from
Source0:	https://github.com/hyperic/sigar/archive/%{sigar_hash}.tar.gz#/%{pname}-%{version}.tar.gz

BuildRequires:	gcc cmake

Patch100: bz714249-1-cpu-count.patch
Patch101: bz746288-1-cpu-count-arch.patch
Patch102: sigar-glibc-2.28.patch
Patch103: sigar-inline-functions.patch

%description
The Sigar API provides a portable interface for gathering system
information such as:
- System memory, swap, CPU, load average, uptime, logins
- Per-process memory, CPU, credential info, state, arguments,
  environment, open files
- File system detection and metrics
- Network interface detection, configuration info and metrics
- Network route and connection tables

This information is available in most operating systems, but each OS
has their own way(s) providing it. SIGAR provides developers with one
API to access this information regardless of the underlying platform.

#The core API is implemented in pure C with bindings currently
#implemented for Java, Perl and C#.

%package -n %{pname}-devel%{PROJ_DELIM}
License:	ASL 2.0
Group:		%{PROJ_NAME}/distro-packages
Summary:	SIGAR Development package - System Information Gatherer And Reporter
Requires:	%{name} = %{version}-%{release}
Provides:       %{pname}-devel

%description -n %{pname}-devel%{PROJ_DELIM}
Header files for developing against the Sigar API

%prep
# When using the GitHub tarballs, use:
# setup -q -n hyperic-{name}-{sigar_hash}
%setup -q -n %{pname}-%{sigar_hash}

%patch100 -p1 -b .bz714249
%patch101 -p1 -b .bz746288
%patch102 -p1
%patch103 -p1

%build

# Fix lib directory
sed -i.sed s:DESTINATION\ lib:DESTINATION\ %{_lib}: src/CMakeLists.txt
%cmake 
make %{?_smp_mflags}

%install
%cmake 
make install DESTDIR=$RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%doc ChangeLog README LICENSE NOTICE AUTHORS
%{_libdir}/libsigar.so

%files -n %{pname}-devel%{PROJ_DELIM}
%{_includedir}/sigar*.h
%doc LICENSE NOTICE AUTHORS
