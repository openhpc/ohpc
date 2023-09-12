help([[
This module loads the %{pname} library.
]])

whatis("Name: %{pname}")
whatis("Version: %{version}")

local version = "%{version}"

prepend_path("MANPATH",      "%{install_path}/share/man")
prepend_path("INCLUDE",      "%{install_path}/include")
prepend_path("LIBRARY_PATH", "%{install_path}/lib")

setenv("%{PNAME}_DIR", "%{install_path}")
setenv("%{PNAME}_LIB", "%{install_path}/lib")
setenv("%{PNAME}_INC", "%{install_path}/include")

family("PMIx")
