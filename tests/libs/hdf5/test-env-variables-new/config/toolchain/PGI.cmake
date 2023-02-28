# Uncomment the following to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "PGI")

set(CMAKE_C_COMPILER pgcc)
set(CMAKE_CXX_COMPILER pgc++)
set(CMAKE_Fortran_COMPILER pgf90)

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
