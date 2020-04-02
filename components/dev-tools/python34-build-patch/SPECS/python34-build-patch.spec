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

%global python_lib_path /usr/lib64/python3.4
%global file_to_patch %{python_lib_path}/distutils/unixccompiler.py

%define pname python34-build-patch

Summary:   OpenHPC build package for python34 modules
Name:      %{pname}%{PROJ_DELIM}
Version:   0.1
Release:   1
License:   Apache-2.0
URL:       https://github.com/openhpc/ohpc
Group:     %{PROJ_NAME}/dev-tools
BuildArch: noarch
Source2:   unixccompiler.patch

#!BuildIgnore: brp-check-suse
#!BuildIgnore: post-build-checks

Requires: python34-libs
Requires: patch

Provides: %{pname}%{PROJ_DELIM}

%description

Reverts the patch https://src.fedoraproject.org/rpms/python3/blob/master/f/00001-rpath.patch
to allow builds of numpy with Intel PSXE on RHEL-like systems.

%prep

%build

%install

install -D -m644 %{SOURCE2}  $RPM_BUILD_ROOT%{OHPC_ADMIN}/compat/python34-distutils-unixccompiler.patch

%post

if [ -f %{file_to_patch} ];then
    echo "Patching %{file_to_patch}"
    pushd %{python_lib_path}/distutils
    patch -p0 < %{OHPC_ADMIN}/compat/python34-distutils-unixccompiler.patch
    popd
    exit 0
else
    echo "Unable to find %{file_to_patch}"
    exit 1
fi

%preun

if [ -f %{file_to_patch} ];then
    echo "Reversing patch %{file_to_patch}"
    pushd %{python_lib_path}
    patch -R -p0 < %{OHPC_ADMIN}/compat/python34-distutils-unixccompiler.patch
    popd
    exit 0
else
    echo "Unable to find %{file_to_patch}"
    exit 1
fi


%files
%{OHPC_ADMIN}/compat/python34-distutils-unixccompiler.patch
