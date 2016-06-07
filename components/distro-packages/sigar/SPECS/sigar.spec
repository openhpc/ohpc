#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

%define pname sigar
%define debug_package %{nil}

Name:		%{pname}%{PROJ_DELIM}
Version:	1.6.5
Release:	0.11.git58097d9%{?dist}
Summary:	System Information Gatherer And Reporter

%global sigar_suffix  0-g4b67f57
%global sigar_hash    58097d9

Group:		System Environment/Libraries
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
Source0:	%{pname}-%{version}-%{sigar_hash}.tbz2

BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildRequires:	gcc cmake

Patch100: bz714249-1-cpu-count.patch
Patch101: bz746288-1-cpu-count-arch.patch

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
%setup -q -n %{pname}-%{version}

%patch100 -p1 -b .bz714249
%patch101 -p1 -b .bz746288

%build

# Fix lib directory
sed -i.sed s:DESTINATION\ lib:DESTINATION\ %{_lib}: src/CMakeLists.txt

%cmake 
make %{?_smp_mflags}

%install
rm -rf $RPM_BUILD_ROOT
%cmake 
make install DESTDIR=$RPM_BUILD_ROOT

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%doc ChangeLog README LICENSE NOTICE AUTHORS
%{_libdir}/libsigar.so

%files -n %{pname}-devel%{PROJ_DELIM}
%defattr(-,root,root,-)
%{_includedir}/sigar*.h
%doc LICENSE NOTICE AUTHORS

%changelog
* Mon Aug 18 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.5-0.11.git58097d9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Sun Jun 08 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.5-0.10.git58097d9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Sun Aug 04 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.5-0.9.git58097d9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.5-0.8.git58097d9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Sat Jul 21 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.5-0.7.git58097d9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Sat Jan 14 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.5-0.6.git58097d9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Fri Oct 21 2011 Zane Bitter <zbitter@redhat.com> - 1.6.4-0.5.git833ca18
- Get correct CPU counts on non-x86 architectures

* Mon Aug 29 2011 Zane Bitter <zbitter@redhat.com> - 1.6.5-0.4.git833ca18
- Get CPU counts from /proc/cpuinfo
  Resolves: #714249

* Wed Feb 09 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.5-0.2.git833ca18
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Thu Dec 16 2010 Andrew Beekhof <andrew@beekhof.net> - 1.6.5-0.1.git833ca18
- Incorporate review feedback
  + Add calls to ldconfig
  + Fix package group
  + Resolve rpmlint warnings
  + Added LICENSE, NOTICE and AUTHORS to the doc list
  + Document how the tarball was generated
  + Update version number to be a .5 pre-release snapshot

* Wed Dec 1 2010 Andrew Beekhof <andrew@beekhof.net> - 1.6.4-1
- Initial checkin
