# Uncomment the following to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "intel")

if(USE_SANITIZER)
  set(CMAKE_C_COMPILER icx-cl)
  set(CMAKE_CXX_COMPILER icx-cl)
  set(CMAKE_Fortran_COMPILER ifx)
  set(INTEL_CLANG ON)
else ()
  set(CMAKE_C_COMPILER icx)
  set(CMAKE_CXX_COMPILER icpx)
  set(CMAKE_Fortran_COMPILER ifx)
endif ()

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
