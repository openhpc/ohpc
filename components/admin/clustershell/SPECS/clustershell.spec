%include %{_sourcedir}/OHPC_macros

%define pname clustershell

Name:          clustershell%{PROJ_DELIM}
Version:       1.8
Release:       1%{?dist}
Summary:       Python framework for efficient cluster administration

Group:         %{PROJ_NAME}/admin
License:       LGPLv2.1
URL:           http://clustershell.sourceforge.net/
Source0:       https://github.com/cea-hpc/%{pname}/archive/v%{version}.tar.gz
Patch1:        clustershell-1.8-no-requires.patch

# Default library install path
%define install_path %{OHPC_LIBS}/%{pname}/%version

%if 0%{?suse_version} == 1110
BuildArch:     x86_64
%else
BuildArch:     noarch
%endif

BuildRequires: python-devel python-setuptools
#!BuildIgnore: post-build-checks

%description
Tools and event-based Python library to execute commands on cluster nodes in
parallel depending on selected engine and worker mechanisms. The library
provides also advanced NodeSet and NodeGroups handling methods to ease and
improve administration of large compute clusters or server farms. Three
convenient command line utilities, clush, clubak and nodeset, allow traditional
shell scripts to benefit some useful features offered by the library.

%package -n vim-%{name}
Summary:       VIM files for ClusterShell
Group:         System Environment/Base
Requires:      clustershell%{PROJ_DELIM} = %{version}-%{release}, vim-common

%description -n vim-%{name}
Syntax highlighting in the VIM editor for ClusterShell configuration files.


%prep
%setup -q -n %{pname}-%{version}
%patch1 -p1

%build
%{__python} setup.py build

%install
%{__python} setup.py install -O1 --prefix=%{install_path} --skip-build --root %{buildroot}

# config files
mkdir -p %{buildroot}/%{_sysconfdir}/clustershell
mv  %{buildroot}/%{install_path}/etc/clustershell/groups.conf.d %{buildroot}/%{_sysconfdir}/clustershell
mv %{buildroot}/%{install_path}/etc/clustershell/groups.d %{buildroot}/%{_sysconfdir}/clustershell
mv conf/*.conf %{buildroot}/%{_sysconfdir}/clustershell/
mv conf/groups.conf.d/*.conf.example %{buildroot}/%{_sysconfdir}/clustershell/groups.conf.d

# man pages
install -d %{buildroot}/%{install_path}/share/man/man1
install -d %{buildroot}/%{install_path}/share/man/man5
install -p -m 0644 doc/man/man1/clubak.1 %{buildroot}/%{install_path}/share/man/man1/
install -p -m 0644 doc/man/man1/clush.1 %{buildroot}/%{install_path}/share/man/man1/
install -p -m 0644 doc/man/man1/nodeset.1 %{buildroot}/%{install_path}/share/man/man1/
install -p -m 0644 doc/man/man5/clush.conf.5 %{buildroot}/%{install_path}/share/man/man5/
install -p -m 0644 doc/man/man5/groups.conf.5 %{buildroot}/%{install_path}/share/man/man5/

# vim addons
%define vimdatadir %{install_path}/share/vim/vimfiles
install -d %{buildroot}/%{vimdatadir}/{ftdetect,syntax}
install -p -m 0644 doc/extras/vim/ftdetect/clustershell.vim %{buildroot}/%{vimdatadir}/ftdetect/
install -p -m 0644 doc/extras/vim/syntax/clushconf.vim %{buildroot}/%{vimdatadir}/syntax/
install -p -m 0644 doc/extras/vim/syntax/groupsconf.vim %{buildroot}/%{vimdatadir}/syntax/

# OpenHPC module file
%{__mkdir_p} %{buildroot}%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
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

prepend-path    PATH                %{install_path}/bin
prepend-path    PYTHONPATH          %{install_path}/lib/python2.7/site-packages
prepend-path    MANPATH             %{install_path}/share/man

setenv          %{pname}_DIR        %{install_path}
setenv          %{pname}_BIN        %{install_path}/bin

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}


%files
%doc ChangeLog COPYING.LGPLv2.1
%doc doc/examples
%{OHPC_PUB}
%exclude %{vimdatadir}
%exclude %{install_path}/share/vim/
%dir %{_sysconfdir}/clustershell
%{_sysconfdir}/clustershell/clush.conf
%{_sysconfdir}/clustershell/groups.conf
%config(noreplace) %{_sysconfdir}/clustershell/clush.conf
%config(noreplace) %{_sysconfdir}/clustershell/groups.conf
%config(noreplace) %{_sysconfdir}/clustershell/groups.d/local.cfg
%dir %{_sysconfdir}/clustershell/groups.conf.d
%dir %{_sysconfdir}/clustershell/groups.d
%doc %{_sysconfdir}/clustershell/groups.d/README
%doc %{_sysconfdir}/clustershell/groups.d/*.example
%doc %{_sysconfdir}/clustershell/groups.conf.d/README
%doc %{_sysconfdir}/clustershell/groups.conf.d/*.example

%files -n vim-%{name}
%dir %{install_path}/share/vim/
%dir %{vimdatadir}
%dir %{vimdatadir}/ftdetect
%dir %{vimdatadir}/syntax
%{vimdatadir}/ftdetect/clustershell.vim
%{vimdatadir}/syntax/clushconf.vim
%{vimdatadir}/syntax/groupsconf.vim
