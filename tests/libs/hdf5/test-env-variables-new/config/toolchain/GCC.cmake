# Uncomment the following line and the correct system name to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "GCC")

set(CMAKE_C_COMPILER cc)
set(CMAKE_CXX_COMPILER c++)
set(CMAKE_Fortran_COMPILER gfortran)

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
