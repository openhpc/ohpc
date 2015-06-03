#
# spec file for package nagios
#
# Copyright (c) 2014 SUSE LINUX Products GmbH, Nuernberg, Germany.
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


Name:           nagios
Version:        3.5.1
Release:        0
Summary:        The Nagios Network Monitor
License:        GPL-2.0+
Group:          System/Monitoring
Url:            http://www.nagios.org/
Source0:        %{name}-%{version}.tar.bz2
Source1:        rc%{name}
Source3:        %{name}.sysconfig
Source4:        suse.de-nagios
Source5:        nagios.8
Source6:        nagiosstats.8
Source7:        nagios-htpasswd.users
#
Source10:       %{name}-README.SuSE
Source11:       %{name}-html-pages.tar.bz2
Source12:       %{name}-3.0.6-docs.tar.bz2
Source20:       %{name}-rpmlintrc
# PATCH-FIX-UPSTREAM Fixes the output of spurious $ signs in commandoutput (deb#480001)
Patch2:         nagios-fix_spurious_dollar_signs_added_to_command_lines.patch
# PATCH-FIX-UPSTREAM unescape hex characters in CGI input - avoid addional '+'
Patch3:         nagios-fix_encoding_trends.cgi.patch
# PATCH-FIX-OPENSUSE openSUSE uses a special location for p1.pl
Patch10:        nagios-p1.pl-location.patch
# PATCH-FIX-OPENSUSE disable Nagios online update checks for distributed packages
Patch11:        nagios-disable_phone_home.patch
# PATCH-FIX-UPSTREAM fix CVE-2013-2214
Patch12:        nagios-CVE-2013-2214.patch
# PATCH-FIX-UPSTREAM fix CVE-2013-7108
Patch13:        nagios-CVE-2013-7108.patch
# PATCH-FIX-UPSTREAM fix CVE-2014-1878
Patch14:        nagios-CVE-2014-1878.patch
BuildRequires:  apache2-devel
BuildRequires:  freetype2-devel
BuildRequires:  gd-devel
BuildRequires:  iputils
BuildRequires:  libjpeg-devel
BuildRequires:  libpng-devel
BuildRequires:  mailx
BuildRequires:  nagios-rpm-macros
BuildRequires:  net-tools
BuildRequires:  openssl-devel
BuildRequires:  pcre-devel
BuildRequires:  zlib-devel
Requires(pre):  %fillup_prereq
Requires(pre):  %insserv_prereq
Requires(pre):  /bin/logger
Requires(pre):  coreutils
Requires(pre):  grep
Requires(pre):  pwdutils
Requires(pre):  sed
Provides:       monitoring_daemon
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
%if 0%{?suse_version} < 1010
BuildRequires:  XFree86-devel
BuildRequires:  XFree86-libs
BuildRequires:  libapr0
Requires:       cron
%else
BuildRequires:  xorg-x11-devel
BuildRequires:  pkgconfig(apr-util-1)
Recommends:     %{name}-www
Recommends:     cron
# this package contains shared tools with icinga
Recommends:     monitoring-tools
# we enable nagios embedded perl, but if people disable it...
Recommends:     perl = %{perl_version}
# as long as it is enabled we need the following requires
%{?libperl_requires}
%endif
Recommends:     icinga-monitoring-tools
%if 0%{?suse_version} > 1020
Recommends:     %{name}-plugins
%else
Requires:       %{name}-plugins
%endif
Requires:       mailx
%define         nslockfile_dir /var/run/%{name}
%define         nslockfile %nslockfile_dir/%{name}.pid
%define         apache2_sysconfdir %(/usr/sbin/apxs2 -q SYSCONFDIR)/conf.d
# Macro that print mesages to syslog at package (un)install time
%define         nnmmsg logger -t %{name}/rpm

%description
Nagios is a program that will monitor hosts and services on your
network. It has the ability to email or page you when a problem arises
and when a problem is resolved. Nagios is written in C and is designed
to run under Linux (and some other *NIX variants) as a background
process, intermittently running checks on various services that you
specify.

The actual service checks are performed by separate "plugin" programs
which return the status of the checks to Nagios. The plugins are
available at http://sourceforge.net/projects/nagiosplug

This package provides core programs for Nagios. The web interface,
documentation, and development files are built as separate packages

%package www
Summary:        Provides the HTML and CGI files for the Nagios web interface
Group:          System/Monitoring
Requires(pre):  /bin/logger
Requires(pre):  apache2
Requires(pre):  coreutils
Requires(pre):  gawk
Requires(pre):  grep
Requires(pre):  pwdutils
Requires(pre):  sed
Requires:       %{name} = %{version}
Requires:       gd
Requires:       mod_php_any
Requires:       php
Provides:       monitoring_webfrontend

%description www
Nagios is a program that will monitor hosts and services on your
network. It has the ability to email or page you when a problem arises
and when a problem is resolved. Nagios is written in C and is designed
to run under Linux (and some other *NIX variants) as a background
process, intermittently running checks on various services that you
specify.

Several CGI programs are included with Nagios in order to allow you to
view the current service status, problem history, notification history,
and log file via the web. This package provides the HTML and CGI files
for the Nagios web interface. In addition, HTML documentation is
included in this package.

%package www-dch
Summary:        HTML and CGI files that do not call home
Group:          System/Monitoring
Requires:       %{name}-www = %{version}

%description www-dch
Several CGI programs are included with Nagios in order to allow you to
view the current service status, problem history, notification history,
and log file via the web.

Since Version 3.1, some of those CGI files and the Nagios process itself
try to detect the latest version and fetching news feeds from the upstream
server www.nagios.org.

This additional package provides simply HTML files that do not "call
home" and also allow to run the web interface without PHP support.

There is also an offline version of the documentation included in this
package.

Please enable the flag "NAGIOSDCH" in the apache configuration.
Note: The HTML pages use 'side' and 'main' and frame targets.

%package devel
Summary:        Development files for Nagios
Group:          Development/Libraries/C and C++
Requires:       %{name} = %{version}
Requires:       gcc

%description devel
Nagios is a program that will monitor hosts and services on your
network. It has the ability to email or page you when a problem arises
and when a problem is resolved. Nagios is written in C and is designed
to run under Linux (and some other *NIX variants) as a background
process, intermittently running checks on various services that you
specify.

This package provides include files that Nagios-related applications
may compile against.

%prep
%setup -q -n %{name}
%patch2 -p0
%patch3 -p0
%patch10 -p0
%patch11 -p0
%patch12 -p1
%patch13 -p1
%patch14 -p1
find -name ".gitignore" | xargs rm
# fix p1.pl [dos]
perl -p -i -e 's|\r\n|\n|' contrib/p1.pl
# fix file permissions
chmod -x Changelog LEGAL LICENSE README

%build
# if the following command fails, then apache dependencies are not met
/usr/sbin/apxs2 -q DATADIR >/dev/null
%configure \
	--prefix=%{_prefix} \
	--exec-prefix=%{_sbindir} \
	--bindir=%{_sbindir} \
	--sbindir=%{nagios_cgidir} \
	--libexecdir=%{nagios_plugindir} \
	--datadir=%{nagios_datadir} \
	--sysconfdir=%{nagios_sysconfdir} \
	--with-init-dir=%{_sysconfdir}/init.d \
	--localstatedir=%{nagios_localstatedir} \
	--with-cgiurl=/%{name}/cgi-bin \
	--with-htmurl=/%{name} \
	--with-httpd-conf=%{apache2_sysconfdir} \
	--with-checkresult-dir=%{nagios_spooldir} \
	--with-lockfile=%{nslockfile} \
	--with-nagios-user=%{nagios_user} \
	--with-nagios-group=%{nagios_group} \
	--with-command-user=%{nagios_command_user} \
	--with-command-group=%{nagios_command_group} \
	--with-gd-lib=%{_libdir} \
	--with-gd-inc=%{_includedir} \
	--with-template-objects \
	--with-template-extinfo \
	--with-perlcache \
	--enable-event-broker \
	--enable-embedded-perl
#
# /usr/bin/p1.pl is not a good place for a perl-_module_!
#
sed -i 's#@p1pldir@#%{nagios_libdir}#g' Makefile include/locations.h sample-config/nagios.cfg
#
# fix p1.pl debug-path
#
sed -i 's#/usr/local/nagios/var/#/var/log/%{name}/#g' p1.pl
# make rpmlint happy: add shebang to p1.pl
echo "#!/usr/bin/perl" > p1.pl.new
cat p1.pl >> p1.pl.new
mv p1.pl.new p1.pl
#
# make daemonchk.cgi and event handlers
#
make %{?_smp_mflags} all
make %{?_smp_mflags} -C contrib
pushd contrib/eventhandlers 1>/dev/null
for f in `find . -type f` ; do
    F=`mktemp temp.XXXXXX`
    sed "s=/usr/local/nagios/var/rw/=%{nagios_spooldir}/=; \
         s=NscaBin\=\"/usr/local/nagios/libexec/send_nsca\"=NscaBin\=/usr/bin/send_nsca=; \
         s=/usr/local/nagios/libexec/eventhandlers/=%{nagios_eventhandlerdir}/=; \
         s=/usr/local/nagios/libexec/=%{nagios_plugindir}/=; \
         s=/usr/local/nagios/etc/=%{nagios_sysconfdir}/=; \
         s=/usr/local/nagios/test/var=%{nagios_logdir}/=" ${f} > ${F}

    mv ${F} ${f}
done
popd 1>/dev/null

%install
mkdir -p %{buildroot}/%{nagios_logdir}/archives
mkdir -p %{buildroot}%{_sbindir}
mkdir -p %{buildroot}%apache2_sysconfdir/
mkdir -p %{buildroot}%nslockfile_dir
make install install-commandmode install-config install-webconf install-classicui\
    DESTDIR=%{buildroot} \
    INSTALL_OPTS="" \
    COMMAND_OPTS="" \
	CGICFGDIR="%{nagios_sysconfdir}" \
    INIT_OPTS=""
make install -C contrib \
    DESTDIR=%{buildroot} \
    INSTALL_OPTS=""
# install event handlers
install -d -m0755 %{buildroot}/%{nagios_eventhandlerdir}
cp -afpv contrib/eventhandlers/* %{buildroot}%{nagios_eventhandlerdir}/
find %{buildroot}%{nagios_eventhandlerdir}/ -type f -exec chmod +x {} \;
# install directory for event brokers like ndoutils
install -d -m0755 %{buildroot}%{nagios_localstatedir}/brokers
# install headers for development package
install -d -m0755 %{buildroot}%{_includedir}/%{name}/
pushd include
for file in *.h; do
    if test "x${file}" != "xconfig.h" -a "x${file}" != "xsnprintf.h" -a "x${file}" != "xcgiutils.h" ; then
        install -m 644 $file %{buildroot}%{_includedir}/%{name}/
    fi;
done
popd
#
# cleanup sample-conf dir for including in docdir
#
find sample-config/ -name "*.in" -exec rm {} \;
find sample-config/ -name "*.in.orig" -exec rm {} \;
sed -e 's|command_file=.*|command_file=%{nagios_command_file}|g' \
    -e 's|log_file=/var/lib/nagios/nagios.log|log_file=%{nagios_logdir}/nagios.log|g' \
    -e 's|log_archive_path=/var/lib/nagios/archives|log_archive_path=%{nagios_logdir}/archives|g' \
    -e 's|^lock_file=.*|lock_file=%nslockfile|g' \
    %{buildroot}/%{nagios_sysconfdir}/nagios.cfg > %{buildroot}%{_sysconfdir}/%{name}/nagios.cfg.tmp
mv %{buildroot}/%{nagios_sysconfdir}/nagios.cfg.tmp %{buildroot}%{_sysconfdir}/%{name}/nagios.cfg
#
# install SuSE specials
#
# README.SuSE file
sed -e 's@DATADIR@%{_datadir}/%{name}@g' -e 's@SYSCONFDIR@%{nagios_sysconfdir}@g' %{SOURCE10} > %_builddir/%{name}/README.SuSE
# init-script
install -D -m 0755 %{SOURCE1} %{buildroot}%{_sysconfdir}/init.d/%{name}
ln -sf ../../etc/init.d/%{name} %{buildroot}%{_sbindir}/rc%{name}
# sysconfig script
install -D -m 0644 %{SOURCE3} %{buildroot}%{_localstatedir}/adm/fillup-templates/sysconfig.%{name}
# install cronjob (gzip' the logfiles)
install -D -m 0755 %{SOURCE4} %{buildroot}%{_sysconfdir}/cron.weekly/%{name}
# install htpasswd file
install -m 0640 %{SOURCE7} %{buildroot}%{_sysconfdir}/%{name}/htpasswd.users
# important ghost files
touch %{buildroot}%{nagios_state_retention_file}
touch %{buildroot}%{nagios_status_file}
touch %{buildroot}%{nagios_logdir}/config.err
touch %{buildroot}%{nslockfile}
# install manpages
install -Dm644 %{SOURCE5} %{buildroot}%{_mandir}/man8/%{name}.8
install -Dm644 %{SOURCE6} %{buildroot}%{_mandir}/man8/nagiostats.8
# fixing permissions the dirty way....
for file in "README sample-config/README sample-config/template-object/README"; do
       chmod 644 $RPM_BUILD_DIR/%{name}/$file
done
# we use nagios_spooldir for this
test -d %{buildroot}%{nagios_localstatedir}/rw && rmdir %{buildroot}%{nagios_localstatedir}/rw
# install plain html files to allow users to use Nagios without internet connection
# and without PHP at all
pushd %{buildroot}%{nagios_datadir} >/dev/null
tar -xf %{SOURCE11}
tar -xf %{SOURCE12}
sed -i "s|Version 3.4.3|Version %{version}|g; \
		s|version=3.4.3|version=%{version}|g" main.html
# jip: ugly at the moment
#LINES=$(echo $(wc -l %%{buildroot}%%{apache2_sysconfdir}/%%{name}.conf | awk '" " { print $1}')-2 | bc)
LINES=40
head -n $LINES %{buildroot}%{apache2_sysconfdir}/%{name}.conf > %{buildroot}%{apache2_sysconfdir}/%{name}.conf.in
cat >> %{buildroot}%{apache2_sysconfdir}/%{name}.conf.in << EOF
    <IfDefine KOHANA2>
      DirectoryIndex index.html index.php
    </IfDefine>
</Directory>
EOF
mv -f %{buildroot}%{apache2_sysconfdir}/%{name}.conf.in %{buildroot}%{apache2_sysconfdir}/%{name}.conf
popd >/dev/null
# delete monitoring-tools because they are provided by monitoring-tools
rm -f %{buildroot}/%{_sbindir}/convertcfg
rm -f %{buildroot}/%{_sbindir}/mini_epn
rm -f %{buildroot}/%{_sbindir}/new_mini_epn

%clean
rm -rf %{buildroot}

%pre
# Create user and group on the system if necessary
%nagios_user_group_add
%nagios_command_user_group_add

# update?
if [ ${1:-0} -gt 1 ]; then
  # in the past, group www was used as nagios_command_group - now we use the default: nagcmd
  if id -Gn %{nagios_user} 2>/dev/null | grep -q %{nagios_command_group} >/dev/null 2>&1 ; then
    : # %%{nagios_user} is already in %%nagios_command_group group
  else
    # Add %%{nagios_user} to %%nagios_command_group.
    %if 0%{?suse_version} > 1220
      usermod -a -G %{nagios_command_group} %{nagios_user}
    %else
      groupmod -A %{nagios_user} %{nagios_command_group} 2>/dev/null
    %endif
    %{nnmmsg} "Added %{nagios_user} to %{nagios_command_group}"
  fi
fi

%post
# Update ?
if [ ${1:-0} -gt 1 ]; then
 if [ -f '%{nagios_sysconfdir}/nagios.cfg' ]; then
  DATE=$(date "+%Y-%m-%d-%H:%M")
  TMPFILE=$(mktemp /tmp/%{name}-XXXXXX)
  NAGIOS_CFG='%{nagios_sysconfdir}/nagios.cfg'
  cp -f $NAGIOS_CFG ${NAGIOS_CFG}_${DATE}
  if ! grep -q ^lock_file=%{nslockfile} "$NAGIOS_CFG" ; then
        echo "- updating pid lock_file= to %{nslockfile} in $NAGIOS_CFG" >> $TMPFILE
        sed -i "s@^lock_file=.*@lock_file=%{nslockfile}@" "$NAGIOS_CFG"
  fi
  if grep -q ^service_reaper_frequency "$NAGIOS_CFG" ; then
        echo "- renaming service_reaper_frequency to check_result_reaper_frequency in $NAGIOS_CFG" >> $TMPFILE
        sed -i "s@service_reaper_frequency@check_result_reaper_frequency@" "$NAGIOS_CFG"
  fi
  if grep -q ^aggregate_status_updates "$NAGIOS_CFG" ; then
        echo "- aggregate_status_updates option has been removed from $NAGIOS_CFG" >> $TMPFILE
        sed -i "s@^aggregate_status_updates@# aggregate_status_updates@" "$NAGIOS_CFG"
  fi
  if grep -q ^downtime_file "$NAGIOS_CFG" ; then
        set -- $(grep ^downtime_file "$NAGIOS_CFG" | sed 's@=@ @')
        shift
        file=$(echo $*)
        if [ -n "$file" ]; then
            if [ -f "$file" ]; then
                set -- $(grep ^state_retention_file "$NAGIOS_CFG" | sed 's@=@ @')
                shift
                state_retention_file=$(echo $*)
                echo "- adding the content of of $file to $state_retention_file" >> $TMPFILE
                cat "$file" >> "$state_retention_file"
            fi
        fi
        echo "- removing downtime_file variable (no longer supported) in $NAGIOS_CFG" >> $TMPFILE
        sed -i "s@^downtime_file@# downtime_file@" "$NAGIOS_CFG"
  fi
  if grep -q ^comment_file "$NAGIOS_CFG" ; then
        set -- $(grep ^comment_file "$NAGIOS_CFG" | sed 's@=@ @')
        shift
        file=$(echo $*)
        if [ -n "$file" ]; then
            if [ -f "$file" ]; then
                echo "- adding the content of $file to $state_retention_file" >> $TMPFILE
                cat "$file" >> "$state_retention_file"
            fi
        fi
        echo "- removing comment_file variable (no longer supported) in $NAGIOS_CFG" >> $TMPFILE
        sed -i "s@^comment_file@# comment_file@" "$NAGIOS_CFG"
  fi
  #{nnmmsg} $(cat $TMPFILE)
  rm $TMPFILE
 fi
else
  # First installation: create an alias for the default nagiosadmin user
  if [ -r etc/aliases ]; then
    if ! grep -q "^nagiosadmin:" etc/aliases; then
        echo -e "nagiosadmin:\troot" >> etc/aliases
	    %{nnmmsg} "Added alias for user nagiosadmin to /etc/aliases"
        if [ -x usr/bin/newaliases ]; then
            usr/bin/newaliases &>/dev/null || true
        fi
    fi
  fi
fi
%{fillup_and_insserv nagios}

%preun
%stop_on_removal %{name}

%postun
%restart_on_update %{name}
%{insserv_cleanup}

%post www
wwwusr=%{nagios_command_user}
if [ -f etc/apache2/uid.conf ]; then
    # If apache is installed, and we can find the apache user, set a shell var
    wwwusr=$(awk '/^[ \t]*User[ \t]+[a-zA-Z0-9]+/ {print $2}' etc/apache2/uid.conf)
fi
# if apache user is not in nagios_command_group, add it
if id -Gn $wwwusr 2>/dev/null | grep -q %{nagios_command_group} >/dev/null 2>&1 ; then
    : # $wwwusr (default: %%nagios_command_user) is already in Nagios cmd group
else
    # modify apache user, adding it to nagios_command_group
  %if 0%{?suse_version} > 1220
    usermod -a -G %{nagios_command_group} $wwwusr
  %else
    groupmod -A $wwwusr %{nagios_command_group} 2>/dev/null
  %endif
  %{nnmmsg} "User $wwwusr added to group %{nagios_command_group} so sending commands to Nagios from the CGI is possible."
fi
# Update ?
if [ ${1:-0} -eq 1 ]; then
	if [ -x %{_sbindir}/a2enmod ]; then
		# enable authentification in apache config
		%{_sbindir}/a2enmod authn_file >/dev/null
        %{_sbindir}/a2enmod auth_basic >/dev/null
        %{_sbindir}/a2enmod authz_user >/dev/null
		# enable php5 in apache config
		%{_sbindir}/a2enmod php5
	fi
fi
%restart_on_update apache2

%post www-dch
# Update ?
if [ ${1:-0} -eq 1 ]; then
    if [ -x %{_sbindir}/a2enflag ]; then
		# enable NAGIOSDCH flag in apache configuration
		%{_sbindir}/a2enflag NAGIOSDCH >/dev/null
	fi
fi
%restart_on_update apache2

%preun www
%restart_on_update apache2

%files
%defattr(0644,root,root,0755)
%doc Changelog LEGAL LICENSE README README.SuSE sample-config/
%dir %{nagios_libdir}
%dir %{nagios_plugindir}
%dir %{nagios_eventhandlerdir}
%exclude %{nagios_cgidir}/*
%{_mandir}/man8/%{name}*
%{_sbindir}/rc%{name}
%{_localstatedir}/adm/fillup-templates/sysconfig.%{name}
%attr(0755,root,root) %{_sysconfdir}/init.d/%{name}
%attr(0755,root,root) %{_sysconfdir}/cron.weekly/*
%attr(0755,root,root) %{nagios_eventhandlerdir}/*
%config(noreplace) %{nagios_sysconfdir}/*.cfg
%config(noreplace) %{nagios_sysconfdir}/objects/*.cfg
%ghost %config(missingok,noreplace) %{nagios_logdir}/config.err
# directories with special handling:
%attr(0755,root,%{nagios_command_group})           %dir %{nagios_sysconfdir}
%attr(0755,root,%{nagios_command_group})           %dir %{nagios_sysconfdir}/objects
%attr(0775,%{nagios_user},%{nagios_command_group}) %dir %{nagios_spooldir}
%attr(0775,%{nagios_user},%{nagios_command_group}) %dir %{nagios_localstatedir}
%attr(0755,%{nagios_user},%{nagios_group})         %dir %{nagios_logdir}
%attr(0755,%{nagios_user},%{nagios_group})         %dir %{nagios_logdir}/archives
%ghost                                             %dir %{nslockfile_dir}
# files with special handling
%config(noreplace) %attr(0640,root,%{nagios_group}) %{nagios_sysconfdir}/resource.cfg
%attr(0755,root,root) %{nagios_libdir}/p1.pl
%attr(0644,%{nagios_user},%{nagios_group}) %verify(not md5 size mtime) %ghost %config(missingok,noreplace) %{nslockfile}
%attr(0600,%{nagios_user},%{nagios_group}) %verify(not md5 size mtime) %ghost %config(missingok,noreplace) %{nagios_state_retention_file}
%attr(0664,%{nagios_user},%{nagios_group}) %verify(not md5 size mtime) %ghost %config(missingok,noreplace) %{nagios_status_file}
%attr(0750,root,%{nagios_command_group}) %{_sbindir}/%{name}
%attr(0750,root,%{nagios_command_group}) %{_sbindir}/nagiostats

%files www
%defattr(0644,root,root,0755)
%dir %{nagios_cgidir}
%dir %{nagios_datadir}
%attr(0755,root,root) %{nagios_cgidir}/*
%{nagios_datadir}/*
%config(noreplace) %{apache2_sysconfdir}/%{name}.conf
%attr(0640,root,%nagios_command_group) %config(missingok,noreplace) %{_sysconfdir}/%{name}/htpasswd.users
%exclude %{nagios_datadir}/*.html
%exclude %{nagios_datadir}/docs/*

%files www-dch
%defattr(0644,root,root,0755)
%{nagios_datadir}/*.html
%{nagios_datadir}/docs/

%files devel
%defattr(644,root,root,0755)
%{_includedir}/%{name}/

%changelog
