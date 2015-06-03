#-------------------------------------------------------------------------------
# Copyright (c) 2015, Intel Corporation
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Intel Corporation nor the names of its contributors
#       may be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#-------------------------------------------------------------------------------

%include %{_sourcedir}/FSP_macros

%define pname gnu-compilers
%{!?PROJ_DELIM:%define PROJ_DELIM %{nil}}

# Define subcomponent versions required for build

%define gmp_version 6.0.0
%define mpc_version 1.0.3
%define mpfr_version 3.1.2


Summary:   The GNU C Compiler and Support Files
Name:      %{pname}%{PROJ_DELIM}
Version:   4.9.2
Release:   1
License:   GPL-3.0+
Group:     fsp/compiler-families
URL:       http://gcc.gnu.org/
Source0:   gcc-%{version}.tar.bz2
Source1:   gmp-%{gmp_version}a.tar.bz2
Source2:   mpc-%{mpc_version}.tar.gz
Source3:   mpfr-%{mpfr_version}.tar.bz2
Source4:   FSP_macros
BuildRoot: %{_tmppath}/%{pname}-%{version}-%{release}-root

%define debug_package %{nil}

BuildRequires:  bison
BuildRequires:  flex
BuildRequires:  gettext-devel
BuildRequires:  perl
BuildRequires:  gcc-c++
%if 0%{?suse_version} > 1220
BuildRequires:  makeinfo
%else
BuildRequires:  texinfo
%endif
BuildRequires:  zlib-devel
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
%endif


%define install_path %{FSP_COMPILERS}/gcc/%{version}

%description

Core package for the GNU Compiler Collection, including the C language
frontend.

%prep
%setup -n gcc-%{version}
%setup -n gcc-%{version} -T -D -a 1
%setup -n gcc-%{version} -T -D -a 2
%setup -n gcc-%{version} -T -D -a 3

ln -s gmp-%{gmp_version} gmp
ln -s mpc-%{mpc_version} mpc
ln -s mpfr-%{mpfr_version} mpfr

%build

mkdir obj
cd obj
../configure --disable-multilib --enable-languages="c,c++,fortran"  --prefix=%{install_path}

%install
cd obj
make %{?_smp_mflags} 
make %{?_smp_mflags} DESTDIR=$RPM_BUILD_ROOT install

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/include
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/lib
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/install-tools
%fdupes -s $RPM_BUILD_ROOT/%{install_path}/share
%endif

# FSP module file
mkdir -p %{buildroot}/%{FSP_MODULES}/gnu
%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/gnu/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the GNU compiler collection"
puts stderr " "
puts stderr "See the man pages for gcc, g++, and gfortran for detailed information"
puts stderr "on available compiler options and command-line syntax."
puts stderr " "

puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: GNU Compiler Collection"
module-whatis "Version: %{version}"
module-whatis "Category: compiler, runtime support"
module-whatis "Description: GNU Compiler Family (C/C++/Fortran for x86_64)"
module-whatis "URL: http://gcc.gnu.org/"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib64
prepend-path    MODULEPATH          %{FSP_MODULEDEPS}/gnu

family "compiler"
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULES}/gnu/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%post
/sbin/ldconfig

%postun
/sbin/ldconfig


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{FSP_HOME}

%changelog
* Tue Aug  5 2014  <karl.w.schulz@intel.com> - 
- Initial build.

