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

Summary:   OpenHPC default login environments
Name:      lmod-defaults-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:   1.3.3
Release:   1
License:   Apache-2.0
Group:     %{PROJ_NAME}/admin
URL:       https://github.com/openhpc/ohpc
BuildArch: noarch
Requires: lmod%{PROJ_DELIM}


%description

Provides default login configuration using the %{compiler_family} compiler
toolchain and %{mpi_family} MPI environment.

%prep

%build

%install

mkdir -p %{buildroot}/%{OHPC_MODULES}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/ohpc
#%Module1.0#####################################################################
# Default OpenHPC environment
#############################################################################

proc ModulesHelp { } {
puts stderr "Setup default login environment"
}

#
# Load Desired Modules
#

prepend-path     PATH   %{OHPC_PUB}/bin

if { [ expr [module-info mode load] || [module-info mode display] ] } {
        prepend-path MANPATH /usr/local/share/man:/usr/share/man/overrides:/usr/share/man/en:/usr/share/man
        module try-add autotools
        module try-add prun
        module try-add %{compiler_family}
        module try-add %{mpi_family}
}

if [ module-info mode remove ] {
        module del %{mpi_family}
        module del %{compiler_family}
        module del prun
        module del autotools
}
EOF

%files
%dir %{OHPC_HOME}
%dir %{OHPC_PUB}
%{OHPC_MODULES}
