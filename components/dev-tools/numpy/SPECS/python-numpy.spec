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

Name:           %{python_prefix}-%{pname}-%{compiler_family}%{PROJ_DELIM}
Version:        2.4.4
Release:        1%{?dist}
Url:            https://github.com/numpy/numpy
Summary:        NumPy array processing for numbers, strings, records and objects
License:        BSD-3-Clause
Group:          %{PROJ_NAME}/dev-tools
Source0:        https://github.com/numpy/numpy/releases/download/v%{version}/numpy-%{version}.tar.gz
Requires:       lmod%{PROJ_DELIM} >= 7.6.1
BuildRequires:  %{python_prefix}-Cython%{PROJ_DELIM}
BuildRequires:  python3-meson-python
BuildRequires:  %{python_prefix}-pip
BuildRequires:  ninja-build
BuildRequires:  pkg-config
BuildRequires:  gcc
%if 0%{?suse_version}
BuildRequires:  fdupes
%endif
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
%setup -q -n %{pname}-%{version}
# Convert PEP 639 license string to old-style dict for older meson-python
sed -i "s|^license = '\(.*\)'|license = {text = '\1'}|" pyproject.toml
%ifarch ppc64le
# Fix VSX3 intrinsics used when building with the -mcpu=power9 baseline (numpy PR #29627).
# Applied as asserted in-place edits rather than a context patch: each substitution is
# checked so a numpy %{version} layout drift fails the build loudly instead of silently
# miscompiling the VSX3 half-precision paths (a no-op sed would otherwise pass unnoticed).
_assert_patched() {
	# $1 = file, $2 = literal string that MUST be present after the edit
	if ! grep -qF -- "$2" "$1"; then
		echo "ERROR: ppc64le numpy VSX3 fix did not apply to $1 (numpy %{version} drift?)" >&2
		exit 1
	fi
}

sed -i "s|args: {'val': '-mcpu=power8', 'match': '.*vsx'}|args: ['-mcpu=power8', '-mvsx']|" meson_cpu/ppc64/meson.build
_assert_patched meson_cpu/ppc64/meson.build "args: ['-mcpu=power8', '-mvsx']"

sed -i 's|defined(NPY_HAVE_VSX3) && defined(NPY_HAVE_VSX_ASM)|defined(NPY_HAVE_VSX3) \&\& defined(NPY_HAVE_VSX_ASM) \&\& defined(NPY__CPU_TARGET_VSX3)|g' numpy/_core/src/common/half.hpp
sed -i 's|defined(NPY_HAVE_VSX3) && defined(vec_extract_fp_from_shorth)|defined(NPY_HAVE_VSX3) \&\& defined(vec_extract_fp_from_shorth) \&\& defined(NPY__CPU_TARGET_VSX3)|g' numpy/_core/src/common/half.hpp
sed -i 's|defined(NPY_HAVE_VSX3) && defined(NPY_HAVE_VSX3_HALF_DOUBLE)|defined(NPY_HAVE_VSX3) \&\& defined(NPY_HAVE_VSX3_HALF_DOUBLE) \&\& defined(NPY__CPU_TARGET_VSX3)|g' numpy/_core/src/common/half.hpp
_assert_patched numpy/_core/src/common/half.hpp "&& defined(NPY__CPU_TARGET_VSX3)"

# numpy/distutils/ccompiler_opt.py ships in some numpy layouts (incl. 2.4.4) but is not
# guaranteed across versions; patch it only when this tree provides it so a future
# relayout degrades to a no-op instead of a hard sed error.
if [ -f numpy/distutils/ccompiler_opt.py ]; then
	sed -i "s|flags=\"-mcpu=power8\", implies_detect=False|flags=\"-mcpu=power8 -mvsx\", implies_detect=False|" numpy/distutils/ccompiler_opt.py
	_assert_patched numpy/distutils/ccompiler_opt.py 'flags="-mcpu=power8 -mvsx"'
fi
%endif

%build
# OpenHPC compiler/mpi designation
%ohpc_setup_compiler

%if "%{compiler_family}" == "arm1"
%__python -m pip wheel --no-build-isolation --wheel-dir=dist \
	-Csetup-args=-Dallow-noblas=true \
	.
%endif

%if "%{compiler_family}" == "intel"
%__python -m pip wheel --no-build-isolation --wheel-dir=dist \
	-Csetup-args=-Dblas=mkl \
	-Csetup-args=-Dlapack=mkl \
	-Csetup-args=-Dallow-noblas=false \
	-Csetup-args=-Ddisable-svml=true \
	.
%endif

%if "%{compiler_family}" != "intel" && "%{compiler_family}" != "arm1"
module load openblas
PKG_CONFIG_PATH="${OPENBLAS_LIB}/pkgconfig:${PKG_CONFIG_PATH}" \
%__python -m pip wheel --no-build-isolation --wheel-dir=dist \
	-Csetup-args=-Dblas=openblas \
	-Csetup-args=-Dlapack=openblas \
	-Csetup-args=-Dallow-noblas=false \
	.
%endif


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
ln -sn "$(realpath -m --relative-to='%{install_path}/bin' '%{__python}')" %{buildroot}/%{install_path}/bin/%{python_family}

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
