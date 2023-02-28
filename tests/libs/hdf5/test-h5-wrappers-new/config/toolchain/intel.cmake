# Uncomment the following to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "intel")

if(USE_SANITIZER)
  set(CMAKE_C_COMPILER icl)
  set(CMAKE_CXX_COMPILER icl++)
  set(CMAKE_Fortran_COMPILER ifort)
  set(INTEL_CLANG ON)
else ()
  set(CMAKE_C_COMPILER icc)
  set(CMAKE_CXX_COMPILER icpc)
  set(CMAKE_Fortran_COMPILER ifort)
endif ()

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
