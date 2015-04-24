#
# spec file for package ganglia
#
# Copyright (c) 2013 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#


%{!?python_sitelib: %define python_sitelib %(%{__python} -c "from distutils.sysconfig import get_python_lib; print get_python_lib()")}

Summary:        A scalable distributed monitoring system for high-performance computing systems
License:        BSD-3-Clause
Group:          System/Monitoring
Name:           ganglia
Version:        3.7.1
Release:        0
%define lib_version 3_7_1-0
Url:            http://ganglia.info/
# The Release macro value is set in configure.in, please update it there.
Source:         %{name}-%{version}.tar.gz
# # PATCH-FIX-OPENSUSE ganglia-3.5.0-init.patch
# Patch0:         ganglia-3.5.0-init.patch
BuildRequires:  autoconf
BuildRequires:  automake
# BuildRequires:  fdupes
BuildRequires:  gcc-c++
BuildRequires:  libart_lgpl-devel
BuildRequires:  libconfuse-devel
BuildRequires:  libpng-devel
BuildRequires:  libtool
BuildRequires:  make
BuildRequires:  pcre-devel
BuildRequires:  pkgconfig
BuildRequires:  python-devel
BuildRequires:  freetype2-devel
# BuildRequires:  libapr1-devel
BuildRequires:  libexpat-devel
BuildRequires:  rrdtool-devel
%if 0%{?sles_version} || 0%{?suse_version}
PreReq:         %insserv_prereq  %fillup_prereq
%endif
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%if 0%{?sles_version} || 0%{?suse_version}
# define fdupes, clean up rpmlint errors
BuildRequires: fdupes
BuildRequires: libapr1-devel
%endif

# different package name with redhat
%if 0%{?rhel_version} || 0%{?centos_version}
BuildRequires:  apr-devel
%endif

%define gmond_conf %{_builddir}/%{?buildsubdir}/gmond/gmond.conf
%define generate_gmond_conf %(test -e %gmond_conf && echo 0 || echo 1)

# + /usr/lib/rpm/brp-python-bytecompile /usr/bin/python 1
# Compiling /home/abuild/rpmbuild/BUILDROOT/ganglia-3.7.1-19.1.x86_64/usr/lib64/ganglia/python_modules/DBUtil.py ...
#   File "/usr/lib64/ganglia/python_modules/DBUtil.py", line 272
#     (options, args) = parser.parse_args()
#                     ^
# SyntaxError: invalid syntax
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')


%description
Ganglia is a scalable distributed monitoring system for high-performance
computing systems such as clusters and Grids. It is based on a hierarchical
design targeted at federations of clusters. It leverages widely used
technologies such as XML for data representation, XDR for compact, portable
data transport, and RRDtool for data storage and visualization. It uses
carefully engineered data structures and algorithms to achieve very low
per-node overheads and high concurrency. The implementation is robust,
has been ported to an extensive set of operating systems and processor
architectures, and is currently in use on thousands of clusters around
the world. It has been used to link clusters across university campuses
and around the world and can scale to handle clusters with 2000 nodes.


%package gmetad
Summary:        Ganglia Meta daemon
Group:          System/Monitoring
Obsoletes:      ganglia-monitor-core < %{version}
Obsoletes:      ganglia-monitor-core-gmetad < %{version}
Provides:       ganglia-monitor-core = %{version}
Provides:       ganglia-monitor-core-gmetad = %{version}

%description gmetad
Ganglia is a scalable, real-time monitoring and execution environment
with all execution requests and statistics expressed in an open
well-defined XML format.

This gmetad daemon aggregates monitoring data from several clusters
to form a monitoring grid. It also keeps metric history using rrdtool.


%package gmond
Summary:        Ganglia Monitor daemon
Group:          System/Monitoring
Obsoletes:      ganglia-monitor-core < %{version}
Obsoletes:      ganglia-monitor-core-gmond < %{version}
Provides:       ganglia-monitor-core = %{version}
Provides:       ganglia-monitor-core-gmond = %{version}

%description gmond
Ganglia is a scalable, real-time monitoring and execution environment
with all execution requests and statistics expressed in an open
well-defined XML format.

This gmond daemon provides the ganglia service within a single cluster or
Multicast domain.

%package gmond-modules-python
Summary:        Ganglia Monitor daemon DSO/Python metric modules support
Group:          System/Monitoring
Requires:       ganglia-gmond
Requires:       python

%description gmond-modules-python
Ganglia is a scalable, real-time monitoring and execution environment
with all execution requests and statistics expressed in an open
well-defined XML format.

This gmond modules support package provides the capability of loading
gmetric/python modules via DSO at daemon start time instead of via gmetric.

%package devel
Summary:        Ganglia static libraries and header files
Group:          Development/Libraries/C and C++
Requires:       libganglia-%{lib_version} = %{version}
# Requires:       libapr1-devel
Requires:       libconfuse-devel
Requires:       libganglia-%{lib_version}
Requires:       libexpat-devel
# different package name with redhat
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires: libapr1-devel
%endif
%if 0%{?rhel_version} || 0%{?centos_version}
BuildRequires:  apr-devel
%endif


%description devel
The Ganglia Monitoring Core library provides a set of functions that programmers
can use to build scalable cluster or grid applications

%package -n libganglia-%{lib_version}
Summary:        Ganglia Shared Libraries http://ganglia.sourceforge.net/
Group:          System/Libraries
Provides:       libganglia-3_1_0 = %{version}
Obsoletes:      libganglia-3_1_0 < %{version}

%description -n libganglia-%{lib_version}
The Ganglia Shared Libraries contains common libraries required by both gmond and
gmetad packages


%prep
%setup -q
#%patch0 -p1

%build
%configure --with-gmetad \
           --enable-status \
           --sysconfdir=%{_sysconfdir}/%{name} \
           --enable-setuid=daemon \
           --enable-setgid=nogroup
make %{?_smp_mflags}

%install
# Create the directory structure
install -d -m 0755 %{buildroot}/etc/init.d
install -d -m 0755 %{buildroot}/etc/rc.d/init.d
install -d -m 0755 %{buildroot}/var/lib/ganglia/rrds

# Move the files into the structure
ls -l gmond
ls -l gmetad
%if 0%{?sles_version} || 0%{?suse_version}
cp -f gmond/gmond.init.SuSE %{buildroot}%{_initrddir}/ganglia-gmond
cp -f gmetad/gmetad.init.SuSE %{buildroot}%{_initrddir}/ganglia-gmetad
%endif
%if 0%{?rhel_version} || 0%{?centos_version}
cp -f gmond/gmond.init %{buildroot}%{_initrddir}/ganglia-gmond
cp -f gmetad/gmetad.init %{buildroot}%{_initrddir}/ganglia-gmetad
%endif
ls -l %{buildroot}%{_initrddir}

install -d -m 0755 %{buildroot}%{_sbindir}
ln -s %{_initrddir}/ganglia-gmond %{buildroot}%{_sbindir}/rcganglia-gmond
ln -s %{_initrddir}/ganglia-gmetad %{buildroot}%{_sbindir}/rcganglia-gmetad


install -d -m 0755 %{buildroot}%{_sysconfdir}/%{name}
install -d -m 0755 %{buildroot}%{_sysconfdir}/%{name}/conf.d
install -d -m 0755 %{buildroot}%{_libdir}/ganglia/python_modules

%if %generate_gmond_conf
# We just output the default gmond.conf from gmond using the '-t' flag
  gmond/gmond -t > %{buildroot}%{_sysconfdir}/%{name}/gmond.conf
%else
  cp -f %gmond_conf %{buildroot}%{_sysconfdir}/%{name}/gmond.conf
%endif
cp -f gmond/modules/conf.d/* %{buildroot}%{_sysconfdir}/%{name}/conf.d

# Copy the python metric modules and .conf files
cp -f gmond/python_modules/conf.d/*.pyconf* %{buildroot}%{_sysconfdir}/%{name}/conf.d/
cp -f gmond/python_modules/*/*.py %{buildroot}%{_libdir}/ganglia/python_modules/
python -c 'import compileall; compileall.compile_dir("%{buildroot}%{_libdir}/ganglia/python_modules/", 1, "/", 1)' > /dev/null

# Don't install the example modules
rm -f %{buildroot}%{_sysconfdir}/%{name}/conf.d/example.conf
rm -f %{buildroot}%{_sysconfdir}/%{name}/conf.d/example.pyconf
rm -f %{buildroot}%{_sysconfdir}/%{name}/conf.d/spfexample.pyconf

# Clean up the .conf.in files
rm -f %{buildroot}%{_sysconfdir}/%{name}/conf.d/*.conf.in

# Disable the multicpu module until it is configured properly
mv %{buildroot}%{_sysconfdir}/%{name}/conf.d/multicpu.conf %{buildroot}%{_sysconfdir}/%{name}/conf.d/multicpu.conf.disabled

make DESTDIR=%{buildroot} install
make -C gmond gmond.conf.5

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes %{buildroot}
%endif

%post   -n libganglia-%{lib_version} -p /sbin/ldconfig

%postun -n libganglia-%{lib_version} -p /sbin/ldconfig


%post gmetad
%fillup_and_insserv ganglia-gmetad

if [ -e /etc/gmetad.conf ]; then
  mv /etc/gmetad.conf %{_sysconfdir}/%{name}
fi

%post gmond
%fillup_and_insserv ganglia-gmond 

LEGACY_GMOND_CONF=%{_sysconfdir}/%{name}/gmond.conf
if [ -e /etc/gmond.conf ];
then
  LEGACY_GMOND_CONF=/etc/gmond.conf
fi

METRIC_LIST="`%{_sbindir}/gmond -c ${LEGACY_GMOND_CONF} -m`"
if [[ $? != 0 ]]; then
  # They may have an old configuration file format
  echo "-----------------------------------------------------------"
  echo "IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT"
  echo "-----------------------------------------------------------"
  echo "Parsing your gmond.conf file failed"
  echo "It appears that you are upgrading from ganglia gmond version"
  echo "2.5.x.  The configuration file has changed and you need to "
  echo "convert your old 2.5.x configuration file to the new format."
  echo ""   
  echo "To convert your old configuration file to the new format"
  echo "simply run the command:"
  echo ""
  echo "% gmond --convert old.conf > new.conf"
  echo ""
  echo "This conversion was not made automatic to prevent unknowningly"
  echo "altering your configuration without your notice."
else
  if [ `echo "$METRIC_LIST" | wc -l` -eq 0 ];
  then
    echo "-----------------------------------------------------------"
    echo "IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT"
    echo "-----------------------------------------------------------"
    echo "No metrics detected - perhaps you are using a gmond.conf"
    echo "file from Ganglia 3.0 or earlier."
    echo "Please see the README file for details about how to"
    echo "create a valid configuration."
  else
    if [ -e /etc/gmond.conf ]; then
      mv /etc/gmond.conf %{_sysconfdir}/%{name}
    fi
  fi
fi

%preun gmetad
%stop_on_removal ganglia-gmetad

%preun gmond
%stop_on_removal ganglia-gmond

%postun gmetad
%insserv_cleanup
%restart_on_update ganglia-gmetad

%postun gmond
%insserv_cleanup
%restart_on_update ganglia-gmond
 
%postun
%insserv_cleanup
%restart_on_update 


%files gmetad
%defattr(-,root,root)
%attr(0755,daemon,nogroup) /var/lib/ganglia/
%{_sbindir}/gmetad
%{_sbindir}/rcganglia-gmetad
%{_initrddir}/ganglia-gmetad
%{_mandir}/man1/gmetad*1*
%config(noreplace) %{_sysconfdir}/%{name}/gmetad.conf

%files gmond
%defattr(-,root,root)
%{_bindir}/gmetric
%{_bindir}/gstat
%{_sbindir}/gmond
%{_sbindir}/rcganglia-gmond
%{_initrddir}/ganglia-gmond
%{_mandir}/man1/gmetric.1*
%{_mandir}/man1/gmond.1*
%{_mandir}/man1/gstat.1*
%{_mandir}/man5/gmond.conf.5*
%config(noreplace) %{_sysconfdir}/%{name}/gmond.conf
%dir %{_sysconfdir}/%{name}
%dir %{_sysconfdir}/%{name}/conf.d/
%config(noreplace) %{_sysconfdir}/%{name}/conf.d/modgstatus.conf
%config(noreplace) %{_sysconfdir}/%{name}/conf.d/multicpu.conf.disabled
%dir %{_libdir}/ganglia/
%{_libdir}/ganglia/modmulticpu.so*
%{_sysconfdir}/%{name}/conf.d/multicpu.conf*
%{_libdir}/ganglia/modcpu.so*
%{_libdir}/ganglia/moddisk.so*
%{_libdir}/ganglia/modgstatus.so
%{_libdir}/ganglia/modload.so*
%{_libdir}/ganglia/modmem.so*
%{_libdir}/ganglia/modnet.so*
%{_libdir}/ganglia/modproc.so*
%{_libdir}/ganglia/modsys.so*

%files gmond-modules-python
%defattr(-,root,root,-)
%dir %{_libdir}/ganglia/python_modules/
%{_libdir}/ganglia/python_modules/*.py*
%{_libdir}/ganglia/modpython.so*
%config(noreplace) %{_sysconfdir}/%{name}/conf.d/modpython.conf
%config(noreplace) %{_sysconfdir}/%{name}/conf.d/*.pyconf*

%files devel
%defattr(-,root,root,-)
%{_includedir}/ganglia.h
%{_includedir}/ganglia_gexec.h
%{_includedir}/gm_file.h
%{_includedir}/gm_metric.h
%{_includedir}/gm_mmn.h
%{_includedir}/gm_msg.h
%{_includedir}/gm_protocol.h
%{_includedir}/gm_value.h
%{_libdir}/libganglia*.so
%{_libdir}/libganglia*.*a
%{_bindir}/ganglia-config

%files -n libganglia-%{lib_version}
%defattr(-,root,root,-)
%{_libdir}/libganglia*.so.*

%changelog
