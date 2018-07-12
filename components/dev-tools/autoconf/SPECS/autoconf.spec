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

%define pname autoconf

Summary:   A GNU tool for automatically configuring source code
Name:      %{pname}%{PROJ_DELIM}
Version:   2.69
Release:   1%{?dist}
License:   GNU GPL
Group:     %{PROJ_NAME}/dev-tools
URL:       http://www.gnu.org/software/autoconf/
Source0:   https://ftp.gnu.org/gnu/autoconf/autoconf-%{version}.tar.gz

Requires: m4

%if 0%{?rhel_version} || 0%{?centos_version} || 0%{?rhel}
BuildRequires: m4
BuildRequires: perl-macros
BuildRequires: perl(Data::Dumper)
# from f19, Text::ParseWords is not the part of 'perl' package
BuildRequires: perl(Text::ParseWords)
%endif

%define install_path %{OHPC_UTILS}/autotools

%description
GNU Autoconf is a tool for configuring source code and Makefiles.
Using Autoconf, programmers can create portable and configurable
packages, since the person building the package is allowed to
specify various configuration options.

You should install Autoconf if you are developing software and
would like to create shell scripts that configure your source code
packages. If you are installing Autoconf, you will also need to
install the GNU m4 package.

Note that the Autoconf package is not required for the end-user who
may be configuring software with an Autoconf-generated script;
Autoconf is only required for the generation of the scripts, not
their use.

%prep
%setup -n autoconf-%{version}

%build
%ifarch ppc64le
cp /usr/lib/rpm/config.guess build-aux
%endif

./configure --prefix=%{install_path}

%install
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

# remove share/info/dir to avoid conflict with other package installs
rm -f $RPM_BUILD_ROOT/%{install_path}/share/info/dir

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%dir %{OHPC_HOME}
%{OHPC_UTILS}
%doc THANKS
%doc NEWS
%doc ChangeLog.2
%doc ChangeLog
%doc COPYING
%doc ChangeLog.3
%doc README
%doc AUTHORS
%doc COPYINGv3
%doc ChangeLog.0
%doc ChangeLog.1
%doc TODO
%doc COPYING.EXCEPTION
