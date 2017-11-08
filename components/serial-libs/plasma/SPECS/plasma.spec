#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

# plasma - Parallel Linear Algebra Software for Multicore Architectures

%define ohpc_compiler_dependent 1
%include %{_sourcedir}/OHPC_macros

# Build requires
BuildRequires: python
%if "%{compiler_family}" != "intel"
BuildRequires: openblas-%{compiler_family}%{PROJ_DELIM}
Requires:      openblas-%{compiler_family}%{PROJ_DELIM}
%endif

# Base package name
%define pname plasma
%define PNAME %(echo %{pname} | tr [a-z] [A-Z])

Name:	%{pname}-%{compiler_family}%{PROJ_DELIM}
Version: 2.8.0
Release: 1%{?dist}
Summary: Parallel Linear Algebra Software for Multicore Architectures
License: BSD-3-Clause
Group:     %{PROJ_NAME}/serial-libs
URL: https://bitbucket.org/icl/%{pname}		
Source0: http://icl.cs.utk.edu/projectsfiles/%{pname}/pubs/%{pname}_%{version}.tar.gz
Source1: http://icl.cs.utk.edu/projectsfiles/%{pname}/pubs/%{pname}-installer_%{version}.tar.gz
Source2: http://www.netlib.org/lapack/lapack-3.7.0.tgz
Source3: %{pname}-rpmlintrc
Source4: OHPC_macros
Patch1:  plasma-lapack_version.patch
Requires: lmod%{PROJ_DELIM} >= 7.6.1

BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root
DocDir:    %{OHPC_PUB}/doc/contrib

#!BuildIgnore: post-build-checks 
# Disable debug packages
%define debug_package %{nil}
# Default library install path
%define install_path %{OHPC_LIBS}/%{compiler_family}/%{pname}/%version

%description
PLASMA is a software package for solving problems in dense linear algebra using
multicore processors and Xeon Phi coprocessors. PLASMA provides implementations
of state-of-the-art algorithms using cutting-edge task scheduling
techniques. PLASMA currently offers a collection of routines for solving linear
systems of equations, least squares problems, eigenvalue problems, and singular
value problems.

%prep
%setup -q -a 1 -n %{pname}_%{version}
%patch1 -p 0

%build
mkdir -p build/download

cp %{SOURCE0} build/download
cp %{SOURCE2} build/download

# OpenHPC compiler designation
%ohpc_setup_compiler

%if "%{compiler_family}" != "intel"
module load openblas
%endif

export SHARED_OPT=-shared

%if %{compiler_family} == gnu7
export PIC_OPT=-fPIC
export SONAME_OPT="-Wl,-soname"
%endif

%if %{compiler_family} == intel
export PIC_OPT=-fpic
export SONAME_OPT="-Xlinker -soname"
%endif

plasma-installer_%{version}/setup.py              \
    --cc=${CC}                                    \
    --fc=${FC}                                    \
    --notesting                                   \
%if %{compiler_family} == gnu7
    --cflags="${RPM_OPT_FLAGS} ${PIC_OPT} -I${OPENBLAS_INC}" \
    --fflags="${RPM_OPT_FLAGS} ${PIC_OPT} -I${OPENBLAS_INC}" \
    --blaslib="-L${OPENBLAS_LIB} -lopenblas"      \
    --cblaslib="-L${OPENBLAS_LIB} -lopenblas"     \
%endif
%if %{compiler_family} == intel
    --cflags="${RPM_OPT_FLAGS} ${PIC_OPT}" \
    --fflags="${RPM_OPT_FLAGS} ${PIC_OPT}" \
    --blaslib="-L/intel/mkl/lib/em64t -lmkl_intel_lp64 -lmkl_sequential -lmkl_core" \
    --cblaslib="-L/intel/mkl/lib/em64t -lmkl_intel_lp64 -lmkl_sequential -lmkl_core" \
    --lapacklib="-L/intel/mkl/lib/em64t -lmkl_intel_lp64 -lmkl_sequential -lmkl_core" \
%endif
    --downlapc

#
#Create shared libraries
#
pushd install/lib  2>&1 > /dev/null
find . -name "*.a"|while read static_lib
do                                 
    bname=`basename ${static_lib}`
    libname=`basename ${static_lib} .a`
    mkdir -p tmpdir
    pushd tmpdir  2>&1 > /dev/null
    ar xv ../${bname}
    ${CC} ${SHARED_OPT} ${SONAME_OPT}=${libname}.so.0 -o ../${libname}.so *.o
    popd  2>&1 > /dev/null
    rm -fr tmpdir
done
popd 2>&1 > /dev/null

#
#Remove static libraries
#
rm -f install/lib/*.a

%install

mkdir -p %{buildroot}%{install_path}

# OpenHPC module file
%{__mkdir} -p %{buildroot}%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{OHPC_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} compiler"
puts stderr "toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} compiler"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL %{url}"

set     version			    %{version}


# Require openblas for gnu compiler families
if { ![is-loaded intel] } {
    depends-on openblas
}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib

setenv          %{PNAME}_DIR        %{install_path}
setenv          %{PNAME}_LIB        %{install_path}/lib
setenv          %{PNAME}_INC        %{install_path}/include

EOF

pushd install 2>&1 > /dev/null

#
# Fix .pc files
#
find . -name "*.pc"|while read file
do                                 
    echo "Fix ${file} up"
    mv ${file} ${file}.tmp
    cat ${file}.tmp | \
        sed -e "s@${RPM_BUILD_DIR}/%{pname}_%{version}/install@%{install_path}@g" \
        > ${file}
    diff -u  ${file}.tmp  ${file} || :
    rm -f ${file}.tmp
done

popd 2>&1 > /dev/null

cp -a install/* %{buildroot}%{install_path}

#
#make links
#
pushd %{buildroot}%{install_path}/lib 2>&1 > /dev/null
/sbin/ldconfig -N .
popd 2>&1 > /dev/null

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root,-)
%{OHPC_PUB}
%doc LICENSE README ReleaseNotes docs/pdf/*.pdf

%changelog
