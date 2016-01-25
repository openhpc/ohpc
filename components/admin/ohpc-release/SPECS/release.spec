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
%{!?PROJ_DELIM: %define PROJ_DELIM -ohpc}

Summary:  OpenHPC release files
Name:     ohpc-release
Version:  %{ohpc_version}
Release:  1
License:  BSD-3
Group:    ohpc/admin
URL:      https://github.com/openhpc/ohpc

Provides: ohpc-release = %{version}

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

%description

Collection of OpenHPC release files including package repository definition.

%prep

%build

%install

%{__mkdir} %{buildroot}/etc

cat >> %{buildroot}/etc/ohpc-release <<EOF
OpenHPC release %{version} 
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/etc/ohpc-release

%changelog

