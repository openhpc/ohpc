#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%if "%{mpi_family}" == "impi"
# For the Intel MPI based combinations, this adds a 'Requires:' to the compiler.
# Usually the appropriate compiler is pulled in via the MPI packages, but this
# does not happen for the Intel MPI compatibility package.
# Unfortunately this also adds a 'BuildRequires:' which means some unnecessary
# packages are downloaded during the build, but the package does not change.
%define ohpc_compiler_dependent 1
%endif

%include %{_sourcedir}/OHPC_macros
%{!?transport: %global transport %{nil}}

# Define commonly used paths to reduce repetition
%define mpi_module_dir %{OHPC_MODULEDEPS}/%{compiler_family}/%{mpi_family}

Summary:   OpenHPC default login environments
Name:      lmod-defaults-%{compiler_family}-%{mpi_family}%{transport}%{PROJ_DELIM}
Version:   2.0
Release:   1
License:   Apache-2.0
Group:     %{PROJ_NAME}/admin
URL:       https://github.com/openhpc/ohpc
BuildArch: noarch
Requires:  lmod%{PROJ_DELIM}

%if "%{mpi_family}" == "impi"
Requires:   intel-mpi-devel%{PROJ_DELIM}
%else
Requires:  %{mpi_family}%{transport}-%{compiler_family}%{PROJ_DELIM}
%endif

%description
Provides default login configuration using the %{compiler_family} compiler
toolchain and %{mpi_family} MPI environment.

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

# include local packages installed via spack
prepend-path MODULEPATH %{OHPC_MODULEDEPS}/spack/

# Define modules to load/unload in order
set modules_list [list autotools prun %{compiler_family} %{mpi_family}]

if { [ expr [module-info mode load] || [module-info mode display] ] } {
        prepend-path MANPATH /usr/local/share/man:/usr/share/man/overrides:/usr/share/man/en:/usr/share/man
        foreach mod \$modules_list {
                module try-add \$mod
        }
}

if [ module-info mode remove ] {
        foreach mod [lreverse \$modules_list] {
                module del \$mod
        }
}
EOF

# Additional logic for mpich-(ucx|ofi) variants
%if "%{mpi_family}" == "mpich"
%post

if [ "${1}" -ge 1 ]; then
    # Get the latest version of the MPI package
    version=$(rpm -q --qf '%%{VERSION}\n' "%{mpi_family}%{transport}-%{compiler_family}%{PROJ_DELIM}" | sort -rV | head -1)

    if [ -n "${version}" ]; then
        moduleFile="${version}%{transport}"
        module_dir="%{mpi_module_dir}"

        if [ -f "${module_dir}/${moduleFile}" ]; then
            pushd "${module_dir}" > /dev/null || exit 1
            # Remove previous default if it exists
            [ -L "default" ] && rm -f "default"
            # Establish latest default
            ln -s "${moduleFile}" "default"
            popd > /dev/null
        fi
    fi
fi

%preun

if [ "${1}" -ge 0 ]; then
    default_link="%{mpi_module_dir}/default"
    if [ -L "${default_link}" ]; then
        rm -f "${default_link}"
    fi
fi

%endif


%files
%dir %{OHPC_HOME}
%dir %{OHPC_PUB}
%{OHPC_MODULES}
