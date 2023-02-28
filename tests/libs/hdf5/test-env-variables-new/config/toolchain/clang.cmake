# Uncomment the following to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "clang")

if(WIN32)
  set(CMAKE_C_COMPILER clang-cl)
  set(CMAKE_CXX_COMPILER clang-cl)
else()
  set(CMAKE_C_COMPILER clang)
  set(CMAKE_CXX_COMPILER clang++)
endif()
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
