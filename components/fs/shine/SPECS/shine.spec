%include %{_sourcedir}/OHPC_macros

# Base package name
%define pname shine

Name:      %{pname}%{PROJ_DELIM}
Summary:   Lustre administration utility
Version:   1.5
Release:   1%{?dist}
Source0:   https://github.com/cea-hpc/%{pname}/archive/v%{version}.tar.gz
Patch1:    29fbd8ca10bf6d672d25439a025c460001fad33e.patch 
License:   GPLv2
Group:     %{PROJ_NAME}/lustre
Vendor:    CEA
Url:       http://lustre-shine.sourceforge.net/
BuildArch: noarch
BuildRequires: python
#!BuildIgnore: post-build-checks
Requires:  clustershell%{PROJ_DELIM} >= 1.5.1
Requires:  lmod%{PROJ_DELIM} >= 7.6.1
Provides:  %{pname} = %{version}

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%description
Lustre administration utility.

%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build
export SHINEVERSION=%{version}
python setup.py build

%install
export SHINEVERSION=%{version}
#python setup.py install --root=%{buildroot} --record=INSTALLED_FILES
python setup.py install --root=%{buildroot} --prefix=%{install_path}
mkdir -p %{buildroot}%{install_path}/%{_sysconfdir}/shine/models
cp conf/*.conf* %{buildroot}%{install_path}/%{_sysconfdir}/shine
cp conf/models/* %{buildroot}%{install_path}/%{_sysconfdir}/shine/models
# relocate the unrelocateable
mkdir -p %{buildroot}%{install_path}/sbin
mkdir -p %{buildroot}%{install_path}/share
mv %{buildroot}/usr/sbin/shine %{buildroot}%{install_path}/sbin/.
mv %{buildroot}/usr/share/shine/shine.init.redhat %{buildroot}%{install_path}/share/.
mv %{buildroot}/usr/share/vim %{buildroot}%{install_path}/share/.

# man pages
mkdir -p %{buildroot}%{install_path}/share/man/{man1,man5}
gzip -c doc/shine.1 >%{buildroot}%{install_path}/share/man/man1/shine.1.gz
gzip -c doc/shine.conf.5 >%{buildroot}%{install_path}/share/man/man5/shine.conf.5.gz

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_ADMIN}/modulefiles/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_ADMIN}/modulefiles/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

        puts stderr " "
        puts stderr "This module loads the %{pname} utility"
        puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/sbin
prepend-path    PYTHONPATH          %{install_path}/lib/python2.7/site-packages
prepend-path    MANPATH             %{install_path}/share/man

setenv          %{pname}_DIR        %{install_path}

depends-on clustershell
EOF

%files
%doc LICENSE README ChangeLog
%{OHPC_ADMIN}
%{OHPC_PUB}
/var/cache/shine/conf/README
