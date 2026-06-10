#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# Numpy python library build that is dependent on compiler toolchain
%define ohpc_compiler_dependent 1
%define ohpc_python_dependent 1
%include %{_sourcedir}/OHPC_macros

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname numpy

# numpy 2.1+ requires Python >= 3.10; use 2.0.x for Leap (Python 3.9)
%define numpy_version_leap 2.0.2
%define numpy_version_el 2.4.4
%if 0%{?suse_version}
%define numpy_version %{numpy_version_leap}
%else
%define numpy_version %{numpy_version_el}
%endif

Name:           %{python_prefix}-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        %{numpy_version}
Release:        1%{?dist}
Url:            https://github.com/numpy/numpy
Summary:        NumPy array processing for numbers, strings, records and objects
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
# OBS requires all sources to be listed; select the right one at build time
Source0:        https://github.com/numpy/numpy/releases/download/v%{numpy_version_leap}/numpy-%{numpy_version_leap}.tar.gz
Source10:       https://github.com/numpy/numpy/releases/download/v%{numpy_version_el}/numpy-%{numpy_version_el}.tar.gz
%define mesonpy_version 0.19.0
Source1:        https://files.pythonhosted.org/packages/source/m/meson-python/meson_python-%{mesonpy_version}.tar.gz
%define pyproject_metadata_version 0.9.0
Source2:        https://files.pythonhosted.org/packages/source/p/pyproject-metadata/pyproject_metadata-%{pyproject_metadata_version}.tar.gz
%define meson_version 1.11.1
Source3:        https://github.com/mesonbuild/meson/releases/download/%{meson_version}/meson-%{meson_version}.tar.gz
%define flit_core_version 3.12.0
Source4:        https://files.pythonhosted.org/packages/source/f/flit-core/flit_core-%{flit_core_version}.tar.gz
%define packaging_version 24.2
Source5:        https://github.com/pypa/packaging/archive/refs/tags/%{packaging_version}.tar.gz#/packaging-%{packaging_version}.tar.gz
%define wheel_version 0.45.1
Source6:        https://github.com/pypa/wheel/archive/refs/tags/%{wheel_version}.tar.gz#/wheel-%{wheel_version}.tar.gz
%define tomli_version 2.2.1
Source7:        https://files.pythonhosted.org/packages/source/t/tomli/tomli-%{tomli_version}.tar.gz
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  %{python_prefix}-Cython%{PROJ_DELIM}
BuildRequires:  %{python_prefix}-pip
%if 0%{?suse_version}
BuildRequires:  ninja
%else
BuildRequires:  ninja-build
%endif
BuildRequires:  pkg-config
BuildRequires:  fdupes gcc
#!BuildIgnore: post-build-checks

# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
NumPy is a general-purpose array-processing package designed to
efficiently manipulate large multi-dimensional arrays of arbitrary
records without sacrificing too much speed for small multi-dimensional
arrays.  NumPy is built on the Numeric code base and adds features
introduced by numarray as well as an extended C-API and the ability to
create arrays of arbitrary type which also makes NumPy suitable for
interfacing with general-purpose data-base applications.

There are also basic facilities for discrete fourier transform,
basic linear algebra and random number generation.

%prep
%if 0%{?suse_version}
%setup -q -T -b 0 -n %{pname}-%{version}
%else
%setup -q -T -b 10 -n %{pname}-%{version}
%endif

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

# Ensure the cython matching our python version is found first
mkdir -p .bin
ln -sf %{_bindir}/cython-%{python_ver} .bin/cython
export PATH=$(pwd)/.bin:$PATH

# Install meson build dependencies from bundled sources
# flit_core first: it is self-bootstrapping and needed by wheel and tomli
pushd /tmp && tar xzf %{SOURCE4} && cd flit_core-* && \
%__python -m flit_core.wheel && \
%__python -m pip install --no-build-isolation dist/flit_core-*.whl && \
popd
%__python -m pip install --no-build-isolation %{SOURCE6}
# tomli: build wheel via flit_core directly to avoid old pip TOML parser bug
pushd /tmp && tar xzf %{SOURCE7} && cd tomli-* && \
%__python -m flit_core.wheel && \
%__python -m pip install --no-build-isolation dist/tomli-*.whl && \
popd
%__python -m pip install --no-build-isolation %{SOURCE5}
%__python -m pip install --no-build-isolation %{SOURCE3}
# Ensure meson executable is on PATH (pip installs it to ~/.local/bin)
export PATH="$HOME/.local/bin:$PATH"
%__python -m pip install --no-build-isolation %{SOURCE2}
%__python -m pip install --no-build-isolation %{SOURCE1}

# Configure meson options via pyproject.toml to avoid pip -C flag
# (not supported by older pip on Leap 15).
# If [tool.meson-python.args] exists, insert setup line into it;
# otherwise append a new section at the end of the file.
%if "%{compiler_family}" == "arm1"
%global _meson_setup setup = ["-Dallow-noblas=true"]
%endif
%if "%{compiler_family}" == "intel"
%global _meson_setup setup = ["-Dblas=mkl", "-Dlapack=mkl", "-Dallow-noblas=false", "-Ddisable-svml=true"]
%endif
%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
%global _meson_setup setup = ["-Dblas=openblas", "-Dlapack=openblas", "-Dallow-noblas=false"]
%endif

if grep -q '\[tool\.meson-python\.args\]' pyproject.toml; then
    sed -i '/\[tool\.meson-python\.args\]/a %{_meson_setup}' pyproject.toml
else
    printf '\n[tool.meson-python.args]\n%s\n' '%{_meson_setup}' >> pyproject.toml
fi

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
PKG_CONFIG_PATH="${OPENBLAS_LIB}/pkgconfig:${PKG_CONFIG_PATH}" \
%endif
%__python -m pip wheel --no-build-isolation --wheel-dir=dist .


%install
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%__python -m pip install --prefix=%{install_path} --root=%{buildroot} \
	--no-index --find-links=dist --no-deps numpy

%if 0%{?suse_version}
%fdupes -s %{buildroot}%{install_path}
%endif

# The default python3 binary is too old. This package uses a newer
# version than the default. Let's point the default python3 binary
# to that newer version.
%{__mkdir_p} %{buildroot}/%{install_path}/bin
ln -sn "$(realpath -m --relative-to='%{install_path}/bin' '%{_bindir}/%{__python}')" %{buildroot}/%{install_path}/bin/%{python_family}

# OpenHPC module file
%{!?compiler_family: %global compiler_family gnu}
%{__mkdir_p} %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{python_module_prefix}%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{python_module_prefix}%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with %{python_prefix}"
puts stderr "and the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{python_prefix}-%{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: python module"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

family                      numpy
set     version             %{version}

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
# Require openblas for gnu and llvm compiler families
depends-on openblas
%endif

prepend-path    PATH                %{install_path}/bin
prepend-path    PYTHONPATH          %{install_path}/lib64/%{python_lib_dir}/site-packages

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin

EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{python_module_prefix}%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir_p} ${RPM_BUILD_ROOT}/%{_docdir}

%files
%{OHPC_PUB}
%doc INSTALL.rst
%doc README.md
%doc LICENSE.txt
%doc PKG-INFO
%doc THANKS.txt
