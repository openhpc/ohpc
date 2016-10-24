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
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

%define pname prun

Summary:   Convenience utility for parallel job launch
Name:      %{pname}%{PROJ_DELIM}
Version:   1.0
Release:   1
License:   BSD
Group:     %{PROJ_NAME}/admin
BuildArch: noarch
URL:       https://github.com/openhpc/ohpc
Source0:   %{pname}
Source1:   OHPC_macros
Source2:   LICENSE      

BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%define package_target %{OHPC_PUB}/%{pname}/%{version}

%description

prun provides a unified, script-based wrapper for launching parallel jobs
within a resource manager for a variety of MPI families.

%prep

%{__cp} %SOURCE2 .


%build
# Binary pass-through - empty build section

%install

rm -rf $RPM_BUILD_ROOT

%{__mkdir} -p %{buildroot}/%{package_target}
install -D -m 0755 %SOURCE0 %{buildroot}/%{package_target}

# OpenHPC modulefile

%{__mkdir} -p %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################
proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the prun job launch utility"
puts stderr " "
puts stderr "Version %{version}"
puts stderr " "

}

module-whatis "Name: prun job launch utility"
module-whatis "Version: %{version}"
module-whatis "Category: resource manager tools"
module-whatis "Description: job launch utility for multiple MPI families"

set     version                 %{version}

prepend-path    PATH            %{package_target}

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT

%post

%postun


%files
%defattr(-,root,root,-)
%dir %{OHPC_HOME}
%doc LICENSE
%{OHPC_HOME}




