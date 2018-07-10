#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%define pname   psqlODBC
%define tarname psqlodbc

Name:           %{pname}%{PROJ_DELIM}
BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  libtool
%if 0%{?rhel_version} || 0%{?centos_version}
BuildRequires:  libtool-ltdl
Requires:       libtool-ltdl
%endif
BuildRequires:  openssl-devel
BuildRequires:  postgresql-devel
BuildRequires:  unixODBC-devel

%if 0%{?suse_version} == 1315
Requires:       libltdl7
%endif

Url:            http://pgfoundry.org/projects/psqlodbc

Summary:        ODBC Driver for PostgreSQL
License:        LGPL-2.1+
Group:          %{PROJ_NAME}/distro-packages
Version:        09.03.0400
Release:        33.1
Source0:        %tarname-%{version}.tar.gz
PreReq:         /usr/bin/odbcinst
Obsoletes:      pg_odbc
Obsoletes:      postgresql-odbc
Provides:       pg_iface:/usr/lib/pgsql/odbcinst.ini
Provides:       pg_odbc
Provides:       postgresql-odbc



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

%build
autoreconf -fi
# they don't ship configure.in, so we have to patch configure :(
sed -i '/LDFLAGS=/s/\$pg_libs//' configure
export CFLAGS="%optflags -fno-strict-aliasing -I/usr/include/pgsql"
%configure --with-unixodbc || { cat config.log && exit 1; }

%install
make DESTDIR=%buildroot install
rm -f %buildroot%_libdir/*.la

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
%doc *.txt docs/*
%_libdir/psql*
