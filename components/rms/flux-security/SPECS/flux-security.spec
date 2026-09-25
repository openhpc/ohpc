#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-
# OpenHPC:check-updates:version-pin 0.15.0

%include %{_sourcedir}/OHPC_macros
%global pname flux-security

Summary:	Independent security component for the Flux resource manager
Name:		%{pname}%{PROJ_DELIM}
Version:	0.15.0
Release:	1%{?dist}
License:	LGPL-3.0
Group:		%{PROJ_NAME}/rms
URL:		https://github.com/flux-framework/flux-security
Source0:	https://github.com/flux-framework/flux-security/releases/download/v%{version}/%{pname}-%{version}.tar.gz

BuildRequires:	make
BuildRequires:	gcc
BuildRequires:	pkgconfig
BuildRequires:	libsodium-devel
BuildRequires:	jansson-devel
BuildRequires:	libuuid-devel
BuildRequires:	munge-devel
BuildRequires:	pam-devel
BuildRequires:	python3-sphinx
BuildRequires:	python3-docutils
Requires:	munge

#!BuildIgnore: post-build-checks

%description
flux-security is an independent project providing security-sensitive code
for the Flux resource manager framework, kept separate from flux-core so
its privileged components can be reviewed and audited on their own.

It provides:
  - libflux-security, an API for signing and verifying job requests
  - the Independent Minister of Privilege (IMP), a setuid-root helper
    that flux-core's job shell uses to start job processes as the
    submitting user

This package is a required build- and run-time dependency of
flux-core when it is configured with --with-flux-security, which is
necessary for flux-core to execute jobs as users other than the one
running the Flux instance.

%prep
%setup -q -n %{pname}-%{version}

%build
%configure \
	--disable-static \
	--enable-pam
%{__make} %{?_smp_mflags}

%install
%{__make} install DESTDIR=%{buildroot}

# Upstream deliberately does not install the IMP setuid so packagers
# make that choice explicitly; it is the multi-user privilege
# boundary flux-core relies on to start job processes as the
# submitting user.
chmod 4755 %{buildroot}%{_libexecdir}/flux/flux-imp

# upstream ships no default IMP exec-as-user config, so "make
# install" never creates this directory; site admins are expected to
# populate it (e.g. with a sudo/setuid exec method)
%{__mkdir_p} %{buildroot}%{_sysconfdir}/flux/imp/conf.d

# don't package static libs
find %{buildroot} -name '*.la' -delete

%files
%{_libdir}/libflux-security*.so*
%{_libdir}/pkgconfig/flux-security.pc
%{_mandir}/man*/*
%attr(4755, root, root) %{_libexecdir}/flux/flux-imp
%dir %{_sysconfdir}/flux
%dir %{_sysconfdir}/flux/security
%dir %{_sysconfdir}/flux/security/conf.d
%config(noreplace) %{_sysconfdir}/flux/security/conf.d/sign.toml
%dir %{_sysconfdir}/flux/imp
%dir %{_sysconfdir}/flux/imp/conf.d
%dir %{_includedir}/flux
%{_includedir}/flux/security
%doc README.md NEWS.md
%license LICENSE
