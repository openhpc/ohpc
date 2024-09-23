#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%{!?configure_options: %global configure_options %{nil}}
%bcond_without cma
%bcond_with    cuda
%bcond_with    gdrcopy
%bcond_without ib
%bcond_with    knem
%bcond_without rdmacm
%bcond_with    rocm
%bcond_with    ugni
%bcond_with    xpmem
%bcond_with    java

%include %{_sourcedir}/OHPC_macros
%undefine _annotated_build

# Base package name
%define pname ucx

Name:    ucx%{PROJ_DELIM}
Version: 1.17.0
Release: 1%{?dist}
Summary: UCX is a communication library implementing high-performance messaging
Group:   %{PROJ_NAME}/mpi-families
License: BSD
URL:     http://www.openucx.org
Source0: https://github.com/openucx/%{pname}/releases/download/v%{version}/%{pname}-%{version}.tar.gz

# UCX currently supports only the following architectures
ExclusiveArch: aarch64 ppc64le x86_64

BuildRequires: automake autoconf libtool gcc-c++ make
%if "%{_vendor}" == "suse"
BuildRequires: libnuma-devel
%else
BuildRequires: numactl-devel
%endif
%if %{with cma}
BuildRequires: glibc-devel >= 2.15
%endif
%if %{with gdrcopy}
BuildRequires: gdrcopy
%endif
%if %{with ib}
BuildRequires: libibverbs-devel
%endif
%if %{with knem}
BuildRequires: knem
%endif
%if %{with rdmacm}
BuildRequires: librdmacm-devel
%endif
%if %{with rocm}
BuildRequires: hsa-rocr-dev
%endif
%if %{with xpmem}
BuildRequires: xpmem-devel
%endif
%if %{with java}
BuildRequires: maven
%endif

%description
UCX stands for Unified Communication X. UCX provides an optimized communication
layer for Message Passing (MPI), PGAS/OpenSHMEM libraries and RPC/data-centric
applications. UCX utilizes high-speed networks, such as RDMA (InfiniBand, RoCE,
etc), Cray Gemini or Aries, for inter-node communication. If no such network is
available, TCP is used instead. UCX supports efficient transfer of data in
either main memory (RAM) or GPU memory (through CUDA and ROCm libraries).
In addition, UCX provides efficient intra-node communication, by leveraging the
following shared memory mechanisms: posix, sysv, cma, knem, and xpmem.
This package was built from '' branch, commit c30b7da.

%if "%{_vendor}" == "suse"
%debug_package
%endif

# Default library install path
%define install_path %{OHPC_MPI_STACKS}/%{name}/%version

%prep
%setup -q -n ucx-%{version}

%build
%define _with_arg()   %{expand:%%{?with_%{1}:--with-%{2}}%%{!?with_%{1}:--without-%{2}}}
%define _enable_arg() %{expand:%%{?with_%{1}:--enable-%{2}}%%{!?with_%{1}:--disable-%{2}}}
./configure --prefix=%{install_path} \
	   --disable-optimizations \
           --disable-logging \
           --disable-debug \
           --disable-assertions \
           --disable-params-check \
	   --libdir=%{install_path}/lib \
           %_enable_arg cma cma \
           %_with_arg cuda cuda \
           %_with_arg gdrcopy gdrcopy \
           %_with_arg ib verbs \
           %_with_arg knem knem \
           %_with_arg rdmacm rdmacm \
           %_with_arg rocm rocm \
           %_with_arg xpmem xpmem \
           %_with_arg ugni ugni \
           %_with_arg java java \
           %{?configure_options}

make %{?_smp_mflags} V=1

%install
make DESTDIR=%{buildroot} install
rm -f %{buildroot}%{install_path}/lib/*.la
rm -f %{buildroot}%{install_path}/lib/*.a
rm -f %{buildroot}%{install_path}/lib/ucx/*.la
rm -f %{buildroot}%{install_path}/lib/ucx/lib*.so
rm -f %{buildroot}%{install_path}/lib/ucx/lib*.a

# OpenHPC module file
%{__mkdir_p} %{buildroot}/%{OHPC_MODULES}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{PNAME} library."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname}"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL: %{url}"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib
prepend-path    PKG_CONFIG_PATH     %{install_path}/lib/pkgconfig

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_BIN        %{install_path}/bin
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include
setenv          %{PNAME}_WARN_UNUSED_ENV_VARS N
EOF

%{__cat} << EOF > %{buildroot}/%{OHPC_MODULES}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%files
%doc README AUTHORS NEWS
%{OHPC_MODULES}
%license LICENSE
%{OHPC_MPI_STACKS}/%{name}
%exclude %{install_path}/lib/ucx/libuct_cma.so.*
%exclude %{install_path}/lib/ucx/libuct_rdmacm.so.*
%exclude %{install_path}/lib/ucx/libuct_ib.so.*

%if %{with cma}
%package -n ucx-cma%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX CMA support
Group: System Environment/Libraries

%description -n ucx-cma%{PROJ_DELIM}
Provides CMA (Linux cross-memory-attach) transport for UCX. It utilizes the
system calls process_vm_readv/writev() for one-shot memory copy from another
process.

%files -n ucx-cma%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_cma.so.*
%endif

%if %{with cuda}
%package -n ucx-cuda%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX CUDA support
Group: System Environment/Libraries

%description -n ucx-cuda%{PROJ_DELIM}
Provide CUDA (NVIDIA GPU) support for UCX. Enables passing GPU memory pointers
to UCX communication routines, and transports taking advantage of GPU-Direct
technology for direct data transfer between GPU and RDMA devices.

%files cuda
%{install_path}/lib/ucx/libucx_perftest_cuda.so.*
%{install_path}/lib/ucx/libucm_cuda.so.*
%{install_path}/lib/ucx/libuct_cuda.so.*
%endif

%if %{with gdrcopy}
%package -n ucx-gdrcopy%{PROJ_DELIM}
Requires: %{name}-cuda%{?_isa} = %{version}-%{release}
Summary: UCX GDRCopy support
Group: System Environment/Libraries

%description -n ucx-gdrcopy%{PROJ_DELIM}
Provide GDRCopy support for UCX. GDRCopy is a low-latency GPU memory copy
library, built on top of the NVIDIA GPUDirect RDMA technology.

%files gdrcopy
%{install_path}/lib/ucx/libuct_cuda_gdrcopy.so.*
%endif

%if %{with ib}
%package -n ucx-ib%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX RDMA support
Group: System Environment/Libraries

%description -n ucx-ib%{PROJ_DELIM}
Provides support for IBTA-compliant transports for UCX. This includes RoCE,
InfiniBand, OmniPath, and any other transport supported by IB Verbs API.
Typically these transports provide RDMA support, which enables a fast and
hardware-offloaded data transfer.

%files -n ucx-ib%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_ib.so.*
%endif

%if %{with knem}
%package -n ucx-knem%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX KNEM transport support
Group: System Environment/Libraries

%description -n ucx-knem%{PROJ_DELIM}
Provides KNEM (fast inter-process copy) transport for UCX. KNEM is a Linux
kernel module that enables high-performance intra-node MPI communication
for large messages.

%files -n ucx-knem%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_knem.so.*
%endif

%if %{with rdmacm}
%package -n ucx-rdmacm%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX RDMA connection manager support
Group: System Environment/Libraries

%description -n ucx-rdmacm%{PROJ_DELIM}
Provides RDMA connection-manager support to UCX, which enables client/server
based connection establishment for RDMA-capable transports.

%files -n ucx-rdmacm%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_rdmacm.so.*
%endif

%if %{with rocm}
%package -n ucx-rocm%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX ROCm GPU support
Group: System Environment/Libraries

%description -n ucx-rocm%{PROJ_DELIM}
Provides Radeon Open Compute (ROCm) Runtime support for UCX.

%files -n ucx-rocm%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_rocm.so.*
%{install_path}/lib/ucx/libucm_rocm.so.*

%if %{with gdrcopy}
%package -n ucx-rocmgdr%{PROJ_DELIM}
Requires: %{name}-rocm%{?_isa} = %{version}-%{release}
Summary: UCX GDRCopy support for ROCM
Group: System Environment/Libraries

%description -n ucx-rocmgdr%{PROJ_DELIM}
Provide GDRCopy support for UCX ROCM. GDRCopy is a low-latency GPU memory copy
library, built on top of the NVIDIA GPUDirect RDMA technology.

%files -n udx-rocmgdr%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_rocm_gdr.so.*
%endif
%endif

%if %{with ugni}
%package -n ucx-ugni%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX Gemini/Aries transport support.
Group: System Environment/Libraries

%description -n ucx-ugni%{PROJ_DELIM}
Provides Gemini/Aries transport for UCX.

%files -n ucx-ugni%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_ugni.so.*
%endif

%if %{with xpmem}
%package -n ucx-xpmem%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX XPMEM transport support.
Group: System Environment/Libraries

%description -n ucx-xpmem%{PROJ_DELIM}
Provides XPMEM transport for UCX. XPMEM is a Linux kernel module that enables a
process to map the memory of another process into its virtual address space.

%files -n ucx-xpmem%{PROJ_DELIM}
%{install_path}/lib/ucx/libuct_xpmem.so.*
%endif

%if %{with java}
%package -n ucx-java%{PROJ_DELIM}
Requires: %{name}%{?_isa} = %{version}-%{release}
Summary: UCX Java bindings
Group: System Environment/Libraries

%description -n ucx-java%{PROJ_DELIM}
Provides java bindings for UCX.

%files -n ucx-java%{PROJ_DELIM}
%{_libdir}/jucx-*.jar
%endif
