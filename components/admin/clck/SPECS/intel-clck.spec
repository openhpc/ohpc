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

Summary:   Intel(R) Cluster Checker
Name:      intel-clck%{PROJ_DELIM}
Version:   3.1.2
Release:   1
License:   Intel
URL:       https://clusterready.intel.com/intel-cluster-checker/
Group:     %{PROJ_NAME}/admin
BuildArch: x86_64
Source1:   OHPC_macros
Source2:   OHPC_mod_generator.sh
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
AutoReq:   no
Requires:  time

# 09/16/15 karl.w.schulz@intel.com - patch to ignore XDG_SESSION_ID variable during env test
Source3:   XDG_SESSION.patch

%define __spec_install_post /usr/lib/rpm/brp-strip-comment-note /bin/true
%define __spec_install_post /usr/lib/rpm/brp-compress /bin/true
%define __spec_install_post /usr/lib/rpm/brp-strip /bin/true

#!BuildIgnore: post-build-checks rpmlint-Factory
%define debug_package %{nil}

%description

Intel cluster checker.

%prep

%build



%install

%{__mkdir_p} %{buildroot}
cd %{buildroot}
%{__tar} xfz $RPM_SOURCE_DIR/intel-clck%{PROJ_DELIM}-%{version}.tar.gz

# OpenHPC patches
# %%{__patch} -p0 < %{SOURCE3}

cd -

# OpenHPC module file
%{__mkdir} -p %{buildroot}/%{OHPC_ADMIN}/modulefiles/clck
%{__cat} << EOF > %{buildroot}/%{OHPC_ADMIN}/modulefiles/clck/%{version}
#%Module1.0#####################################################################

module-whatis "Name: Intel Cluster Checker"
module-whatis "Version: %{version}"
module-whatis "Category: diagnostics"
module-whatis "Description: Intel Cluster Checker"
module-whatis "URL: https://clusterready.intel.com/intel-cluster-checker/"

set     version			    %{version}

EOF

# Parse shell script to derive module settings

%{__chmod} 700 %{_sourcedir}/OHPC_mod_generator.sh 
%{_sourcedir}/OHPC_mod_generator.sh %{buildroot}/%{OHPC_ADMIN}/clck/%{version}/bin/clckvars.sh >> %{buildroot}/%{OHPC_ADMIN}/modulefiles/clck/%{version}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{OHPC_HOME}

%changelog

