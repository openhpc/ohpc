#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the Performance Peak project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package openblas
#
# Copyright (c) 2015 SUSE LINUX GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

#-fsp-header-comp-begin-----------------------------

%include %{_sourcedir}/FSP_macros

# FSP convention: the default assumes the gnu toolchain and openmpi
# MPI family; however, these can be overridden by specifing the
# compiler_family and mpi_family variables via rpmbuild or other
# mechanisms.

%{!?compiler_family: %define compiler_family gnu}
%{!?PROJ_DELIM:      %define PROJ_DELIM      %{nil}}

# Compiler dependencies
BuildRequires: lmod%{PROJ_DELIM} coreutils
%if %{compiler_family} == gnu
BuildRequires: gnu-compilers%{PROJ_DELIM}
Requires:      gnu-compilers%{PROJ_DELIM}
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ intel-compilers-devel%{PROJ_DELIM}
Requires:      gcc-c++ intel-compilers-devel%{PROJ_DELIM}
%if 0%{?FSP_BUILD}
BuildRequires: intel_licenses
%endif
%endif

#-fsp-header-comp-end-------------------------------

# Base package name
%define pname openblas
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:           %{pname}-%{compiler_family}-%{mpi_family}%{PROJ_DELIM}
Version:        0.2.14
Release:        21.1
Summary:        An optimized BLAS library based on GotoBLAS2
License:        BSD-3-Clause
Group:          Productivity/Scientific/Math
Url:            http://www.openblas.net
Source0:        https://github.com/xianyi/OpenBLAS/archive/v%{version}.tar.gz#/%{pname}-%{version}.tar.gz
# PATCH-FIX-UPSTREAM openblas-libs.patch: Link against libgfortran
Patch0:         openblas-libs.patch
# PATCH-FIX-UPSTREAM c_xerbla_no-void-return.patch
Patch1:         c_xerbla_no-void-return.patch
# PATCH-FIX-UPSTREAM openblas-noexecstack.patch
Patch2:         openblas-noexecstack.patch
# PATCH-FIX-UPSTREAM openblas-arm64-build.patch
Patch3:         openblas-arm64-build.patch
BuildRoot:      %{_tmppath}/%{pname}-%{version}-build
ExclusiveArch:  %ix86 ia64 ppc ppc64 x86_64 aarch64
BuildRequires:  update-alternatives
Requires(post): update-alternatives
Requires(preun): update-alternatives
DocDir:        %{FSP_PUB}/doc/contrib

%description
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

%package     -n lib%{pname}_serial0
Summary:        An optimized BLAS library based on GotoBLAS2, serial version
Group:          System/Libraries
Requires(post): update-alternatives
Requires(preun): update-alternatives
# TODO set <= 0.2.14
Obsoletes:      lib%{pname}0 < %{version}-%{release}
Provides:       lib%{pname}0 = %{version}-%{release}

%description -n lib%{pname}_serial0
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

%package     -n lib%{pname}_serial-devel
Summary:        Development libraries for OpenBLAS, serial version
Group:          Development/Libraries/C and C++
Requires:       %{pname}-devel-headers = %{version}
Requires:       lib%{pname}_serial0 = %{version}

%description -n lib%{pname}_serial-devel
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains the development libraries for serial OpenBLAS version.

%package     -n lib%{pname}_openmp0
Summary:        An optimized BLAS library based on GotoBLAS2, OpenMP version
Group:          System/Libraries
Requires(post): update-alternatives
Requires(preun): update-alternatives
# TODO set <= 0.2.14
Obsoletes:      lib%{pname}0 < %{version}-%{release}
Provides:       lib%{pname}0 = %{version}-%{release}

%description -n lib%{pname}_openmp0
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains the library compiled with OpenMP support.

%package     -n lib%{pname}o0
Summary:        An optimized BLAS library based on GotoBLAS2, OpenMP version
Group:          System/Libraries
Requires:       lib%{pname}_openmp0 = %{version}

%description -n lib%{pname}o0
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains files for backward compatibility.

%package     -n lib%{pname}_openmp-devel
Summary:        Development libraries for OpenBLAS, OpenMP version
Group:          Development/Libraries/C and C++
Requires:       %{pname}-devel-headers = %{version}
Requires:       lib%{pname}_openmp0 = %{version}

%description -n lib%{pname}_openmp-devel
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains the development libraries for OpenMP OpenBLAS version.

%package     -n lib%{pname}_pthreads0
Summary:        An optimized BLAS library based on GotoBLAS2, pthreads version
Group:          System/Libraries
Requires(post): update-alternatives
Requires(preun): update-alternatives
# TODO set <= 0.2.14
Obsoletes:      lib%{pname}0 < %{version}-%{release}
Provides:       lib%{pname}0 = %{version}-%{release}

%description -n lib%{pname}_pthreads0
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains the library compiled with threading support.

%package     -n lib%{pname}p0
Summary:        An optimized BLAS library based on GotoBLAS2, pthreads version
Group:          System/Libraries
Requires:       lib%{pname}_pthreads0 = %{version}

%description -n lib%{pname}p0
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains files for backward compatibility.

%package     -n lib%{pname}_pthreads-devel
Summary:        Development headers and libraries for OpenBLAS, pthreads version
Group:          Development/Libraries/C and C++
Requires:       %{pname}-devel-headers = %{version}
Requires:       lib%{pname}_pthreads0 = %{version}

%description -n lib%{pname}_pthreads-devel
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains the development libraries for pthreads OpenBLAS version.

%package        devel
Summary:        Development headers and libraries for OpenBLAS
Group:          Development/Libraries/C and C++
Requires:       %{pname}-devel-headers = %{version}
%ifarch %ix86 x86_64
Requires:       lib%{pname}_pthreads-devel = %{version}
%else
Requires:       lib%{pname}_openmp-devel = %{version}
%endif

%description    devel
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains the development libraries and headers for OpenBLAS.

%package        devel-headers
Summary:        Development headers for OpenBLAS
Group:          Development/Libraries/C and C++
# TODO set <= 0.2.14
Conflicts:      %{pname}-devel < %{version}-%{release}
BuildArch:      noarch

%description    devel-headers
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains headers for OpenBLAS.

%package        devel-static
Summary:        Static version of OpenBLAS
Group:          Development/Libraries/C and C++
Requires:       %{pname}-devel = %{version}

%description    devel-static
OpenBLAS is an optimized BLAS library based on GotoBLAS2 1.13 BSD version.

This package contains the static libraries.

%define debug_package %{pnil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%version

%prep
%setup -q -c -T

# Untar source
tar -zxf %{SOURCE0}
cd OpenBLAS-%{version}
%patch0 -p1
%patch1 -p1
%patch2 -p1
%patch3 -p1
cd ..

# Prepare build for serial, pthreads and OpenMP versions
cp -ar OpenBLAS-%{version} openmp
cp -ar OpenBLAS-%{version} pthreads
mv OpenBLAS-%{version} serial

%build

# Only *86 CPUs support DYNAMIC_ARCH
%ifarch %ix86 x86_64
%define openblas_target DYNAMIC_ARCH=1
%endif
# Temporary fix, OpenBLAS does not autodetect aarch64
%ifarch aarch64
%define openblas_target TARGET=ARMV8
%endif
# Make serial, threaded and OpenMP versions
make -C serial %{?openblas_target} USE_THREAD=0 USE_OPENMP=0 LIBNAMESUFFIX=serial \
        FC=gfortran CC=gcc COMMON_OPT="%{optflags}" NUM_THREADS=64
make -C openmp %{?openblas_target} USE_THREAD=1 USE_OPENMP=1 LIBNAMESUFFIX=openmp \
        FC=gfortran CC=gcc COMMON_OPT="%{optflags}" NUM_THREADS=64
make -C pthreads %{?openblas_target} USE_THREAD=1 USE_OPENMP=0 LIBNAMESUFFIX=pthreads \
        FC=gfortran CC=gcc COMMON_OPT="%{optflags}" NUM_THREADS=64

%install
# Install serial library and headers
make -C serial USE_THREAD=0 LIBNAMESUFFIX=serial \
        OPENBLAS_LIBRARY_DIR=%{buildroot}%{_libdir} \
        OPENBLAS_INCLUDE_DIR=%{buildroot}%{_includedir}/%{pname} \
        OPENBLAS_CMAKE_DIR=%{buildroot}%{_libdir}/cmake/%{pname} \
        PREFIX=%{buildroot}%{_prefix} install

# Put libraries in correct location
rm -rf %{buildroot}%{_libdir}/lib%{pname}*

# Install the serial library
install -D -p -m 755 serial/lib%{pname}_serial.so %{buildroot}%{_libdir}/lib%{pname}_serial.so.0
install -D -p -m 644 serial/lib%{pname}_serial.a %{buildroot}%{_libdir}/lib%{pname}_serial.a

# Install the OpenMP library
install -D -p -m 755 openmp/lib%{pname}_openmp.so %{buildroot}%{_libdir}/lib%{pname}_openmp.so.0
install -D -p -m 644 openmp/lib%{pname}_openmp.a %{buildroot}%{_libdir}/lib%{pname}_openmp.a

# Install the threaded library
install -D -p -m 755 pthreads/lib%{pname}_pthreads.so %{buildroot}%{_libdir}/lib%{pname}_pthreads.so.0
install -D -p -m 644 pthreads/lib%{pname}_pthreads.a %{buildroot}%{_libdir}/lib%{pname}_pthreads.a

# Fix source permissions (also applies to LAPACK)
find -name \*.f -exec chmod 644 {} +

# Dummy target for update-alternatives
install -d %{buildroot}/%{_sysconfdir}/alternatives
ln -s lib%{pname}.so.0 %{buildroot}/%{_libdir}/lib%{pname}.so.0
ln -s lib%{pname}.so.0 %{buildroot}/%{_sysconfdir}/alternatives/lib%{pname}.so.0
ln -s lib%{pname}.so.0 %{buildroot}/%{_libdir}/libblas.so.3
ln -s lib%{pname}.so.0 %{buildroot}/%{_libdir}/libcblas.so.3
ln -s lib%{pname}.so.0 %{buildroot}/%{_libdir}/liblapack.so.3
ln -s lib%{pname}.so.0 %{buildroot}/%{_sysconfdir}/alternatives/libblas.so.3
ln -s lib%{pname}.so.0 %{buildroot}/%{_sysconfdir}/alternatives/libcblas.so.3
ln -s lib%{pname}.so.0 %{buildroot}/%{_sysconfdir}/alternatives/liblapack.so.3

# Fix symlinks
pushd %{buildroot}%{_libdir}
ln -sf lib%{pname}.so.0 lib%{pname}.so
# Serial libraries
ln -sf lib%{pname}_serial.so.0 lib%{pname}_serial.so
# OpenMP libraries
ln -sf lib%{pname}_openmp.so.0 lib%{pname}_openmp.so
ln -sf lib%{pname}_openmp.so.0 lib%{pname}o.so.0
ln -sf lib%{pname}o.so.0 lib%{pname}o.so
# Threaded libraries
ln -sf lib%{pname}_pthreads.so.0 lib%{pname}_pthreads.so
ln -sf lib%{pname}_pthreads.so.0 lib%{pname}p.so.0
ln -sf lib%{pname}p.so.0 lib%{pname}p.so

# Fix cmake config file
sed -i 's|%{buildroot}||g' %{buildroot}%{_libdir}/cmake/%{pname}/*.cmake
sed -i 's|_serial||g' %{buildroot}%{_libdir}/cmake/%{pname}/*.cmake

# Delete info about host cpu
%ifarch %ix86 x86_64
sed -i '/#define OPENBLAS_NEEDBUNDERSCORE/,/#define OPENBLAS_VERSION/{//!d}' %{buildroot}%{_includedir}/%{pname}/openblas_config.h
%endif

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library built with the %{compiler_family} compiler toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{PNAME} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "%{url}"

set     version             %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    INCLUDE             %{install_path}/include
prepend-path    LD_LIBRARY_PATH     %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

family "metis"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%{__mkdir} -p %{buildroot}/%{_docdir}

%post -n lib%{pname}_serial0
%{_sbindir}/update-alternatives --install \
   %{_libdir}/libblas.so.3 libblas.so.3 %{_libdir}/lib%{pname}_serial.so.0  20
%{_sbindir}/update-alternatives --install \
   %{_libdir}/libcblas.so.3 libcblas.so.3 %{_libdir}/lib%{pname}_serial.so.0  20
%{_sbindir}/update-alternatives --install \
   %{_libdir}/liblapack.so.3 liblapack.so.3 %{_libdir}/lib%{pname}_serial.so.0  20
%{_sbindir}/update-alternatives --install \
   %{_libdir}/lib%{pname}.so.0 lib%{pname}.so.0 %{_libdir}/lib%{pname}_serial.so.0  20
/sbin/ldconfig

%preun -n lib%{pname}_serial0
if [ "$1" = 0 ] ; then
   %{_sbindir}/update-alternatives --remove libblas.so.3 %{_libdir}/lib%{pname}_serial.so.0
   %{_sbindir}/update-alternatives --remove libcblas.so.3 %{_libdir}/lib%{pname}_serial.so.0
   %{_sbindir}/update-alternatives --remove liblapack.so.3 %{_libdir}/lib%{pname}_serial.so.0
   %{_sbindir}/update-alternatives --remove lib%{pname}.so.0 %{_libdir}/lib%{pname}_serial.so.0
fi

%postun -n lib%{pname}_serial0 -p /sbin/ldconfig

%posttrans -n lib%{pname}_serial0
if [ "$1" = 0 ] ; then
  if ! [ -f %{_libdir}/lib%{pname}.so.0 ] ; then
      %{_sbindir}/update-alternatives --auto lib%{pname}.so.0
  fi
fi

%post -n lib%{pname}_openmp0
# Check config
config_blas_mode=$(%{_sbindir}/update-alternatives --query libblas.so.3 | grep 'Status:' | sed -e 's/Status: //g')
config_blas_value=$(%{_sbindir}/update-alternatives --query libblas.so.3 | grep 'Value:' | grep -o 'openblas.')
config_lapack_mode=$(%{_sbindir}/update-alternatives --query liblapack.so.3 | grep 'Status:' | sed -e 's/Status: //g')
config_lapack_value=$(%{_sbindir}/update-alternatives --query liblapack.so.3 | grep 'Value:' | grep -o 'openblas.')
# Install
%{_sbindir}/update-alternatives --install \
   %{_libdir}/libblas.so.3 libblas.so.3 %{_libdir}/lib%{pname}_openmp.so.0  20
%{_sbindir}/update-alternatives --install \
   %{_libdir}/libcblas.so.3 libcblas.so.3 %{_libdir}/lib%{pname}_openmp.so.0  20
%{_sbindir}/update-alternatives --install \
   %{_libdir}/liblapack.so.3 liblapack.so.3 %{_libdir}/lib%{pname}_openmp.so.0  20
%{_sbindir}/update-alternatives --install \
%ifarch %ix86 x86_64
   %{_libdir}/lib%{pname}.so.0 lib%{pname}.so.0 %{_libdir}/lib%{pname}_openmp.so.0  20
%else
   %{_libdir}/lib%{pname}.so.0 lib%{pname}.so.0 %{_libdir}/lib%{pname}_openmp.so.0  50
%endif
# Reconfigure
if [ ${config_blas_mode} = "manual" ] && [ ${config_blas_value} = "openblaso" ]; then
    %{_sbindir}/update-alternatives --set libblas.so.3 %{_libdir}/lib%{pname}_openmp.so.0
fi
if [ ${config_lapack_mode} = "manual" ] && [ ${config_lapack_value} == "openblaso" ]; then
    %{_sbindir}/update-alternatives --set liblapack.so.3 %{_libdir}/lib%{pname}_openmp.so.0
fi
# Remove old variants
%{_sbindir}/update-alternatives --remove libblas.so.3 %{_libdir}/lib%{pname}o.so.0
%{_sbindir}/update-alternatives --remove liblapack.so.3 %{_libdir}/lib%{pname}o.so.0
/sbin/ldconfig

%preun -n lib%{pname}_openmp0
if [ "$1" = 0 ] ; then
   %{_sbindir}/update-alternatives --remove libblas.so.3 %{_libdir}/lib%{pname}_openmp.so.0
   %{_sbindir}/update-alternatives --remove libcblas.so.3 %{_libdir}/lib%{pname}_openmp.so.0
   %{_sbindir}/update-alternatives --remove liblapack.so.3 %{_libdir}/lib%{pname}_openmp.so.0
   %{_sbindir}/update-alternatives --remove lib%{pname}.so.0 %{_libdir}/lib%{pname}_openmp.so.0
fi

%postun -n lib%{pname}_openmp0 -p /sbin/ldconfig

%posttrans -n lib%{pname}_openmp0
if [ "$1" = 0 ] ; then
  if ! [ -f %{_libdir}/lib%{pname}.so.0 ] ; then
      %{_sbindir}/update-alternatives --auto lib%{pname}.so.0
  fi
fi

%post -n lib%{pname}_pthreads0
# Check config
config_blas_mode=$(%{_sbindir}/update-alternatives --query libblas.so.3 | grep 'Status:' | sed -e 's/Status: //g')
config_blas_value=$(%{_sbindir}/update-alternatives --query libblas.so.3 | grep 'Value:' | grep -o 'openblas.')
config_lapack_mode=$(%{_sbindir}/update-alternatives --query liblapack.so.3 | grep 'Status:' | sed -e 's/Status: //g')
config_lapack_value=$(%{_sbindir}/update-alternatives --query liblapack.so.3 | grep 'Value:' | grep -o 'openblas.')
# Install
%{_sbindir}/update-alternatives --install \
   %{_libdir}/libblas.so.3 libblas.so.3 %{_libdir}/lib%{pname}_pthreads.so.0  20
%{_sbindir}/update-alternatives --install \
   %{_libdir}/libcblas.so.3 libcblas.so.3 %{_libdir}/lib%{pname}_pthreads.so.0  20
%{_sbindir}/update-alternatives --install \
   %{_libdir}/liblapack.so.3 liblapack.so.3 %{_libdir}/lib%{pname}_pthreads.so.0  20
%{_sbindir}/update-alternatives --install \
%ifarch %ix86 x86_64
   %{_libdir}/lib%{pname}.so.0 lib%{pname}.so.0 %{_libdir}/lib%{pname}_pthreads.so.0  50
%else
   %{_libdir}/lib%{pname}.so.0 lib%{pname}.so.0 %{_libdir}/lib%{pname}_pthreads.so.0  20
%endif
# Reconfigure
if [ ${config_blas_mode} = "manual" ] && [ ${config_blas_value} = "openblasp" ]; then
    %{_sbindir}/update-alternatives --set libblas.so.3 %{_libdir}/lib%{pname}_pthreads.so.0
fi
if [ ${config_lapack_mode} = "manual" ] && [ ${config_lapack_value} == "openblasp" ]; then
    %{_sbindir}/update-alternatives --set liblapack.so.3 %{_libdir}/lib%{pname}_pthreads.so.0
fi
# Remove old variants
%{_sbindir}/update-alternatives --remove libblas.so.3 %{_libdir}/lib%{pname}p.so.0
%{_sbindir}/update-alternatives --remove liblapack.so.3 %{_libdir}/lib%{pname}p.so.0
/sbin/ldconfig

%preun -n lib%{pname}_pthreads0
if [ "$1" = 0 ] ; then
   %{_sbindir}/update-alternatives --remove libblas.so.3 %{_libdir}/lib%{pname}_pthreads.so.0
   %{_sbindir}/update-alternatives --remove libcblas.so.3 %{_libdir}/lib%{pname}_pthreads.so.0
   %{_sbindir}/update-alternatives --remove liblapack.so.3 %{_libdir}/lib%{pname}_pthreads.so.0
   %{_sbindir}/update-alternatives --remove lib%{pname}.so.0 %{_libdir}/lib%{pname}_pthreads.so.0
fi

%postun -n lib%{pname}_pthreads0 -p /sbin/ldconfig

%posttrans -n lib%{pname}_pthreads0
if [ "$1" = 0 ] ; then
  if ! [ -f %{_libdir}/lib%{pname}.so.0 ] ; then
      %{_sbindir}/update-alternatives --auto lib%{pname}.so.0
  fi
fi

%files -n lib%{pname}_serial0
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}_serial.so.0
%ghost %{_libdir}/lib%{pname}.so.0
%ghost %{_libdir}/libblas.so.3
%ghost %{_libdir}/libcblas.so.3
%ghost %{_libdir}/liblapack.so.3
%ghost %{_sysconfdir}/alternatives/lib%{pname}.so.0
%ghost %{_sysconfdir}/alternatives/libblas.so.3
%ghost %{_sysconfdir}/alternatives/libcblas.so.3
%ghost %{_sysconfdir}/alternatives/liblapack.so.3

%files -n lib%{pname}_serial-devel
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}_serial.so

%files -n lib%{pname}_openmp0
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}_openmp.so.0
%ghost %{_libdir}/lib%{pname}.so.0
%ghost %{_libdir}/libblas.so.3
%ghost %{_libdir}/libcblas.so.3
%ghost %{_libdir}/liblapack.so.3
%ghost %{_sysconfdir}/alternatives/lib%{pname}.so.0
%ghost %{_sysconfdir}/alternatives/libblas.so.3
%ghost %{_sysconfdir}/alternatives/libcblas.so.3
%ghost %{_sysconfdir}/alternatives/liblapack.so.3

%files -n lib%{pname}o0
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}o.so.0

%files -n lib%{pname}_openmp-devel
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}_openmp.so
%{_libdir}/lib%{pname}o.so

%files -n lib%{pname}_pthreads0
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}_pthreads.so.0
%ghost %{_libdir}/lib%{pname}.so.0
%ghost %{_libdir}/libblas.so.3
%ghost %{_libdir}/libcblas.so.3
%ghost %{_libdir}/liblapack.so.3
%ghost %{_sysconfdir}/alternatives/lib%{pname}.so.0
%ghost %{_sysconfdir}/alternatives/libblas.so.3
%ghost %{_sysconfdir}/alternatives/libcblas.so.3
%ghost %{_sysconfdir}/alternatives/liblapack.so.3

%files -n lib%{pname}p0
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}p.so.0

%files -n lib%{pname}_pthreads-devel
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}_pthreads.so
%{_libdir}/lib%{pname}p.so

%files devel
%defattr(-,root,root,-)
%doc serial/Changelog.txt serial/GotoBLAS* serial/LICENSE serial/README.md README.SUSE
%{_libdir}/lib%{pname}.so
%dir %{_libdir}/cmake
%{_libdir}/cmake/%{pname}/

%files devel-headers
%defattr(-,root,root,-)
%{_includedir}/%{pname}/

%files devel-static
%defattr(-,root,root,-)
%{_libdir}/lib%{pname}_serial.a
%{_libdir}/lib%{pname}_openmp.a
%{_libdir}/lib%{pname}_pthreads.a

%changelog
* Wed Jul 29 2015 dmitry_r@opensuse.org
- Change library name suffix
  * drop openblas-soname.patch
- Add RPM %%post script for manual BLAS/LAPACK update-alternatives
  configuration update
- Use update-alternatives mechanism for OpenBLAS variants (serial,
  openmp, pthreads). pthreads variant is default for x86 and x86_64,
  OpenMP for other architectures.
- Fix build on ARM64
  * openblas-arm64-build.patch
- Add update-alternatives mechanism for CBLAS
- Provide cmake module
- Delete info about host cpu from openblas_config.h for dynamic arch
- Add update-alternatives to 'preup' and 'post' requires list for
  libraries
- Add README.SUSE
* Wed Mar 25 2015 dmitry_r@opensuse.org
- Update to version 0.2.14
  * Improve ger and gemv for small matrices by stack allocation.
    e.g. make -DMAX_STACK_ALLOC=2048
  * Introduce openblas_get_num_threads and openblas_get_num_procs.
  * Add ATLAS-style ?geadd function.
  * Fix c/zsyr bug with negative incx.
  * Fix race condition during shutdown causing a crash in
    gotoblas_set_affinity().
  x86/x86-64:
  * Support AMD Streamroller.
  ARM:
  * Add Cortex-A9 and Cortex-A15 targets.
* Wed Dec  3 2014 dmitry_r@opensuse.org
- Update to version 0.2.13
  * Add SYMBOLPREFIX and SYMBOLSUFFIX makefile options
    for adding a prefix or suffix to all exported symbol names
    in the shared library.
  * Remove openblas-0.1.0-soname.patch
  * Add openblas-soname.patch
  * Rebase openblas-noexecstack.patch
  x86/x86-64:
  * Add generic kernel files for x86-64. make TARGET=GENERIC
  * Fix a bug of sgemm kernel on Intel Sandy Bridge.
  * Fix c_check bug on some amd64 systems.
  ARM:
  * Support APM's X-Gene 1 AArch64 processors.
  * Optimize trmm and sgemm.
* Fri Oct 17 2014 dmitry_r@opensuse.org
- Update to version 0.2.12
  * Added CBLAS interface for ?omatcopy and ?imatcopy.
  * Enable ?gemm3m functions.
  * Added benchmark for ?gemm3m.
  * Optimized multithreading lower limits.
  * Disabled SYMM3M and HEMM3M functions because of segment violations.
  x86/x86-64:
  * Improved axpy and symv performance on AMD Bulldozer.
  * Improved gemv performance on modern Intel and AMD CPUs.
* Mon Aug 18 2014 dmitry_r@opensuse.org
- Update to version 0.2.11
  * Added some benchmark codes.
  x86/x86-64:
  * Improved s/c/zgemm performance for Intel Haswell.
  * Improved s/d/c/zgemv performance.
  * Support the big numa machine.(EXPERIMENT)
  ARM:
  * Fix detection when cpuinfo uses "Processor".
* Thu Jul 17 2014 dmitry_r@opensuse.org
- Update to version 0.2.10
  * Added BLAS extensions as following.
    s/d/c/zaxpby, s/d/c/zimatcopy, s/d/c/zomatcopy.
  * Added OPENBLAS_CORETYPE environment for dynamic_arch. (a86d34)
  * Support outputing the CPU corename on runtime.(#407)
  * Patched LAPACK to fix bug 114, 117, 118.
  (http://www.netlib.org/lapack/bug_list.html)
  * Disabled ?gemm3m for a work-around fix. (#400)
  * Fixed lots of bugs for optimized kernels on sandybridge,Haswell,
    bulldozer, and piledriver.
  * Remove obsolete openblas-0.2.9-gcc-warnings.patch
* Tue Jun 10 2014 dmitry_r@opensuse.org
- Update to version 0.2.9
  * Update LAPACK to 3.5.0 version
  * Fixed compatiable issues with Clang and Pathscale compilers.
  * Added OPENBLAS_VERBOSE environment variable.(#338)
  * Make OpenBLAS thread-pool resilient to fork via pthread_atfork.
    (#294)
  * Rewrote rotmg
  * Fixed sdsdot bug.
  * Improved the result for LAPACK testing. (#372)
  x86/x86-64:
  * Optimization on Intel Haswell.
  * Enable optimization kernels on AMD Bulldozer and Piledriver.
  * Detect Intel Haswell for new Macbook.
  * To improve LAPACK testing, we fallback some kernels. (#372)
    https://github.com/xianyi/OpenBLAS/wiki/Fixed-optimized-kernels-To-do-List
  ARM:
  * Support ARMv6 and ARMv7 ISA.
  * Optimization on ARM Cortex-A9.
- Update patches:
  * openblas-0.2.8-libs.patch
  * openblas-0.2.8-noexecstack.patch
  to
  * openblas-libs.patch
  * openblas-noexecstack.patch
- Fix gcc warnings (#385)
  * openblas-0.2.9-gcc-warnings.patch
* Sat Apr 12 2014 dmitry_r@opensuse.org
- Remove files with problematic licenses
* Fri Apr  4 2014 dmitry_r@opensuse.org
- Update to version 0.2.8
  * Add executable stack markings.
  * Respect user's LDFLAGS
  * Rollback bulldozer and piledriver kernels to barcelona kernels
  * update openblas-0.2.6-libs.patch
  * update c_xerbla_no-void-return.patch
  * update openblas-0.2.7-noexecstack.patch
* Fri Jul 26 2013 scorot@free.fr
- version 0.2.7
  * Support LSB (Linux Standard Base) 4.1.
    e.g. make CC=lsbcc
  * Include LAPACK 3.4.2 source codes to the repo.
    Avoid downloading at compile time.
  * Add NO_PARALLEL_MAKE flag to disable parallel make.
  * Create openblas_get_parallel to retrieve information which
    parallelization model is used by OpenBLAS. (Thank
    grisuthedragon)
  * Detect LLVM/Clang compiler.
  * A walk round for dtrti_U single thread bug. Replace it with
    LAPACK codes. (#191)
  * Optimize c/zgemm, trsm, dgemv_n, ddot, daxpy, dcopy on
    AMD Bulldozer. (Thank Werner Saar)
  * Add Intel Haswell support (using Sandybridge optimizations).
    (Thank Dan Luu)
  * Add AMD Piledriver support (using Bulldozer optimizations).
  * Fix the computational error in zgemm avx kernel on
    Sandybridge. (#237)
  * Fix the overflow bug in gemv.
  * Fix the overflow bug in multi-threaded BLAS3, getrf when
    NUM_THREADS is very large.(#214, #221, #246).
- rebase patch noexecstack.patch
- remove lapack source tarball since lapack sources are included
  in openblas sources
- increase NUM_THREAD from 32 to 64
* Sat Mar  2 2013 scorot@free.fr
- version 0.2.6
  * Improved OpenMP performance slightly. (d744c9)
  * Improved cblas.h compatibility with Intel MKL.(#185)
  * Fixed the overflowing bug in single thread cholesky
    factorization.
  * Fixed the overflowing buffer bug of multithreading hbmv and
    sbmv.(#174)
  * Added AMD Bulldozer x86-64 S/DGEMM AVX kernels. (Thank
    Werner Saar) We will tune the performance in future.
  * Auto-detect Intel Xeon E7540.
  * Fixed the overflowing buffer bug of gemv. (#173)
  * Fixed the bug of s/cdot about invalid reading NAN on
    x86_64. (#189)
- rebase patch0 openblas-0.2.6-libs.patch
* Sun Feb 17 2013 jengelh@inai.de
- Remove redundant cleaning commands
- Do not create .so.0.2.5. SO versions are not package release
  numbers.
* Mon Jan 21 2013 scorot@free.fr
- use Requires(post) and Requires(preun) instead of PreReq
- add patch markups in spec file
* Tue Jan 15 2013 scorot@free.fr
- add update-alternatives support to allow easy switching between
  the different blas and lapack implementations
* Fri Nov 30 2012 scorot@free.fr
- version 0.2.5
  * Export LAPACK 3.4.2 symbols in shared library. (#147)
  * Restore the original CPU affinity when calling
  openblas_set_num_threads(1) (#153)
  * Fixed a SEGFAULT bug in dgemv_t when m is very large.(#154)
* Mon Oct  8 2012 scorot@free.fr
- version 0.2.4
  * Upgraded LAPACK to 3.4.2 version. (#145)
  * f77blas.h:compatibility for compilers without C99 complex
  number support. (#141)
  * Added NO_AVX flag. Check OS supporting AVX on runtime. (#139)
* Mon Aug 20 2012 scorot@free.fr
- version 0.2.3
  * Fixed LAPACK unstable bug about ?laswp. (#130)
  * Fixed the shared library bug about unloading the library on
  Linux (#132).
* Sun Jul  8 2012 scorot@free.fr
- version 0.2.2
  * Support Intel Sandy Bridge 22nm desktop/mobile CPU
* Mon Jul  2 2012 scorot@free.fr
- version 0.2.1
  * Fixed the SEGFAULT bug about hyper-theading
  * Support AMD Bulldozer by using GotoBLAS2 AMD Barcelona codes
  * Removed the limitation (64) of numbers of CPU cores.
  Now, it supports 256 cores at max.
  * Supported clang compiler.
  * Fixed some build bugs on FreeBSD
  * Optimized Level-3 BLAS on Intel Sandy Bridge x86-64 by AVX
  instructions.
  * Support AMD Bobcat by using GotoBLAS2 AMD Barcelona codes.
- update patch3
* Wed May  2 2012 scorot@free.fr
- update patch0
* Wed May  2 2012 scorot@free.fr
- again fix remaining library file name error in spec file
* Wed May  2 2012 scorot@free.fr
- fix wrong library file name version
* Wed May  2 2012 scorot@free.fr
- Update to version 0.1.1
  * Upgraded LAPACK to 3.4.1 version. (Thank Zaheer Chothia)
  * Supported LAPACKE, a C interface to LAPACKE. (Thank Zaheer Chothia)
  * Fixed the build bug (MD5 and download) on Mac OSX.
  * Auto download CUnit 2.1.2-2 from SF.net with UTEST_CHECK=1.
  x86/x86_64:
  * Auto-detect Intel Sandy Bridge Core i7-3xxx & Xeon E7 Westmere-EX.
  * Test alpha=Nan in dscale.
  * Fixed a SEGFAULT bug in samax on x86 windows.
* Wed Apr 25 2012 scorot@free.fr
- version 0.1.0
- update openblas-0.1.0-soname.patch
- add openblas-0.1.0-noexecstack.patch
- spec file cleanup
* Mon Mar 12 2012 scorot@free.fr
- version 0.1alpha2.5
