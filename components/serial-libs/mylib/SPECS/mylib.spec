

#-fsp-header-comp-begin----------------------------------------------

# FSP convention: the default assumes the gnu compiler family;
# however, this can be overridden by specifing the compiler_family
# variable via rpmbuild or other mechanisms.

%{!?compiler_family: %define compiler_family gnu}

# Compiler dependencies
BuildRequires: lmod
%if %{compiler_family} == gnu
BuildRequires: FSP-gnu-compilers
Requires:      FSP-gnu-compilers
%endif
%if %{compiler_family} == intel
BuildRequires: gcc-c++ FSP-intel-compilers
Requires:      gcc-c++ FSP-intel-compilers 
%endif
%if 0%{FSP_BUILD}
BuildRequires: intel_licenses
%endif

#-fsp-header-comp-end------------------------------------------------

# Base package name
%define pname mylib

Summary:   Demo example
Name:      %{pname}-%{compiler_family}
Version:   2.0
Release:   1
License:   GPL-3.0+
Group:     Development/Languages/C and C++
URL:       http://random.org
Source0:   %{pname}-%{version}.tar.gz
Source1:   FSP_setup_compiler
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%define debug_package %{nil}

# Default library install path
%define install_path %{FSP_LIBS}/%{compiler_family}/%{name}/%version

%description
Just an example to play with.

%prep

%setup -q -n %{pname}-%{version}

%build

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

./configure --prefix=%{install_path}

%install

# FSP compiler designation
export FSP_COMPILER_FAMILY=%{compiler_family}
. %{_sourcedir}/FSP_setup_compiler

make DESTDIR=$RPM_BUILD_ROOT install

# FSP module file
%{__mkdir} -p %{buildroot}%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}
%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/%{version}
#%Module1.0#####################################################################

proc ModulesHelp { } {

puts stderr " "
puts stderr "This module loads the %{pname} library built with the %{compiler_family} toolchain."
puts stderr "\nVersion %{version}\n"

}
module-whatis "Name: %{pname} built with %{compiler_family} toolchain"
module-whatis "Version: %{version}"
module-whatis "Category: runtime library"
module-whatis "Description: %{summary}"
module-whatis "URL: http://random.org"

set     version			    %{version}

prepend-path    PATH                %{install_path}/bin
prepend-path    MANPATH             %{install_path}/share/man
prepend-path    INCLUDE             %{install_path}/include
prepend-path	LD_LIBRARY_PATH	    %{install_path}/lib64
EOF

%{__cat} << EOF > %{buildroot}/%{FSP_MODULEDEPS}/%{compiler_family}/%{pname}/.version.%{version}
#%Module1.0#####################################################################
##
## version file for %{pname}-%{version}
##
set     ModulesVersion      "%{version}"
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{FSP_HOME}


%changelog
* Tue Aug  5 2014  <karl.w.schulz@intel.com> - 
- Initial build.

