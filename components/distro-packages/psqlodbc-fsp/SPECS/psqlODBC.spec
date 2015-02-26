#
# spec file for package psqlODBC
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

%define pname psqlODBC
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

Name:           %{pname}%{PROJ_DELIM}
BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  libtool
%if 0%{?rhel_version} || 0%{?centos_version}
BuildRequires:  libtool-ltdl
%endif
BuildRequires:  openssl-devel
BuildRequires:  postgresql-devel
BuildRequires:  unixODBC-devel
Url:            http://pgfoundry.org/projects/psqlodbc
%define       tarname psqlodbc
Summary:        ODBC Driver for PostgreSQL
License:        LGPL-2.1+
Group:          fsp/distro-packages
Version:        08.03.0200
Release:        33.1
Source0:        %tarname-%{version}.tar.bz2
Patch1:         psqlODBC-literal.patch
Patch2:         psqlodbc-08.03.0200-build.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
PreReq:         /usr/bin/odbcinst
Obsoletes:      pg_odbc
Obsoletes:      postgresql-odbc
Provides:       pg_iface:/usr/lib/pgsql/odbcinst.ini
Provides:       pg_odbc
Provides:       postgresql-odbc
Requires:       libltdl7

%description
This package contains the ODBC (Open DataBase Connectivity) driver and
sample configuration files needed for applications to access a
PostgreSQL database using ODBC.



Authors:
--------
    Dave Page
    Peter Eisentraut
    Ludek Finstrle
    Bruce Momjian
    Anoop Kumar
    Hiroshi Saito

%prep
%setup -q -n %tarname-%version
%patch1
%if 0%{?suse_version} > 1230
%patch2
%endif

%build
autoreconf -fi
# they don't ship configure.in, so we have to patch configure :(
sed -i '/LDFLAGS=/s/\$pg_libs//' configure
export CFLAGS="%optflags -fno-strict-aliasing -I/usr/include/pgsql"
%configure --with-unixodbc || cat config.log

%install
make DESTDIR=%buildroot install
rm -f %buildroot%_libdir/*.la

%clean
rm -rf %buildroot

%post
/sbin/ldconfig
# odbcinst uses reference counting, so we don't
# need to take care for the update case here.
odbcinst -i -l -d %_libdir/psqlodbcw.so -r <<EOF
[PSQL]
Description = PostgreSQL
%ifarch x86_64 ppc64 ia64 s390x
Driver64 = %_libdir/psqlodbcw.so
%else
Driver = %_libdir/psqlodbcw.so
%endif
EOF

%postun
/sbin/ldconfig
# odbcinst uses reference counting, so we don't
# need to take care for the update case here.
odbcinst -u -l -d %_libdir/psqlodbcw.so -n PSQL

%files
%defattr(-,root,root,-)
%doc *.txt docs/*
%_libdir/psql*

%changelog
* Tue Apr 23 2013 mhrusecky@suse.com
- fix build on openSUSE > 12.3 with new unixODBC
* Tue Nov  3 2009 coolo@novell.com
- updated patches to apply with fuzz=0
* Wed Nov 12 2008 max@suse.de
- Automate registering and unregistering the driver with unixODBC
  on installation and uninstallation (bnc#420850).
* Thu Sep 11 2008 max@suse.de
- New version: 08.03.0200. For details on the countless
  improvements and bug fixes, see:
  http://psqlodbc.projects.postgresql.org/release.html or
  /usr/share/doc/packages/psqlODBC/release.html
* Mon Jul 30 2007 max@suse.de
- New version: 08.02.0400. Changes include:
  * Ditinguish the indicaitor and the octet_length field of APD
    clearly.
  * Handle @@IDENTITY more generally.
  * Take outer join into account so as to evaluate nullability.
  * Fix a bug about Keyset-driven cursors.
  * Change to use NULL indicator instead of the length buffer in
    SQLFetch.
  * Fix a bug which forgets unnamed plans too early especially when
    handling large objects.
  * Don't treat charcters whose value >= 128 as alphanumeric in case
    of conversion of binary data to bytea.
  * Change ConfigDSN() so that it takes the options in Setup Dialog
    page 1 into account.
  * Simplify the memory management of statements'columns info so as to
    prevent memory leaks or a crash in parse_statement etc.
  * SQLTables("", SQL_ALL_SCHEMAS. "", ..) now returns a list of valid
    schemas.
  * SQLTables("", "", "", SQL_ALL_TABLE_TYPES) now returns a list of
    valid table types.
  * SQLGetInfo SQL_DATABASE_NAME now returns the database name.
  * Treat the tables in information_schema as system tables.
  * Correct the precision of SQL_NUMERIC_STRUCT.
  * Change the default max varchar size from 254 to 255.
  * Reset the fields information properly in case of SQLMoreResults.
  * Implement SQLDescribeParam() also in case of multi-command queries.
  * Handle dollar-quotes more properly.
  * Append DETAIL messages to GetDiag...() messages.
  * Remove WSAStartup() and WSACleanup() from DllMain.
  * Load libpq from the driver's folder.
  * Improve the implemetation of SQLSetPos(.., SQL_ADD/SQL_UPDATE)
    using the 8.2 new feature INSERT/UPDATE .. returning.
* Wed Dec 13 2006 max@suse.de
- Avoid literal string comparison (#228225)
- New version: 08.02.0200
  - Added support for SSL and Kerberos
  - Reduce compiler warnings on x86_64
  - Many more improvements
* Wed Jan 25 2006 mls@suse.de
- converted neededforbuild to BuildRequires
* Fri Jan 13 2006 max@suse.de
- New version: 08.01.0102
* Mon Jul  4 2005 max@suse.de
- Re-added -fno-strict-aliasing .
* Fri Jul  1 2005 max@suse.de
- New version: 08.00.0101
- Building with support for unixODBC.
* Thu Nov 11 2004 ro@suse.de
- fixed file list
* Tue May 25 2004 max@suse.de
- Adapted a patch from Debian to fix a buffer overflow in ODBC driver
  (src/interfaces/odbc/): added parameter for target buffer size to
  make_string() to prevent buffer overflows and corrected all calls to
  it (http://bugs.debian.org/247306, SuSE Bugzilla #40714).
  With previous versions it was possible to crash (and possibly
  exploit) e. g. apache if a PHP script connected to a ODBC database
  with very long credential strings (DSN, username, password, etc.).
* Fri Feb  6 2004 max@suse.de
- Added -fno-strict-aliasing to CFLAGS.
* Thu Jan 15 2004 max@suse.de
- New version: 07.03.0200
* Sun Jan 11 2004 adrian@suse.de
- add %%run_ldconfig
* Thu Jan  9 2003 max@suse.de
- psqlODBC (formerly postgresql-odbc) becomes a package of it's
  own, because it has been removed from the PostgreSQL
  core distribution and is now a separate project.
- New version: 7.2.5.
