cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples C CXX)

#-----------------------------------------------------------------------------
# Instructions for use : Normal Build
#
# For standard build of HDF5 examples.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Allow Visual Studio solution directories
#-----------------------------------------------------------------------------
# Provide a way for Visual Studio Express users to turn OFF the new FOLDER
# organization feature. Default to ON for non-Express users. Express users must
# explicitly turn off this option to build HDF5 in the Express IDE...
#
option (HDF5_USE_FOLDERS "Enable folder grouping of projects in IDEs." ON)
MARK_AS_ADVANCED (HDF5_USE_FOLDERS)
if (HDF5_USE_FOLDERS)
  set_property (GLOBAL PROPERTY USE_FOLDERS ON)
endif (HDF5_USE_FOLDERS)

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (HDF_RESOURCES_EXT_DIR  ${PROJECT_SOURCE_DIR}/config/cmake_ext_mod)
set (HDF_RESOURCES_DIR      ${PROJECT_SOURCE_DIR}/config/cmake)
set (HDF5EX_C_SRC_DIR          ${PROJECT_SOURCE_DIR}/C)
set (HDF5EX_F90_SRC_DIR        ${PROJECT_SOURCE_DIR}/FORTRAN)

#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------
include (${HDF_RESOURCES_EXT_DIR}/HDFMacros.cmake)
include (${HDF_RESOURCES_DIR}/HDF5Macros.cmake)

#-----------------------------------------------------------------------------
# Setup output Directories
#-----------------------------------------------------------------------------
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY
    ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Executables."
)
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY
    ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Libraries"
)
set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY
    ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all static libraries."
)
set (CMAKE_Fortran_MODULE_DIRECTORY
    ${PROJECT_BINARY_DIR}/bin/fortran CACHE PATH "Single Directory for all fortran modules."
)

#-----------------------------------------------------------------------------
# Option to Build Static executables
#-----------------------------------------------------------------------------
option (BUILD_STATIC_EXECS "Build Static Executabless" OFF)
if (BUILD_STATIC_EXECS)
  if (NOT WIN32)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -static")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static")
  endif (NOT WIN32)
endif (BUILD_STATIC_EXECS)

#-----------------------------------------------------------------------------
# When building utility executables that generate other (source) files :
# we make use of the following variables defined in the root CMakeLists.
# Certain systems may add /Debug or /Release to output paths
# and we need to call the executable from inside the CMake configuration
#-----------------------------------------------------------------------------
set (EXE_EXT "")
if (WIN32)
  set (EXE_EXT ".exe")
  if (NOT CYGWIN)
    ADD_DEFINITIONS (-D_BIND_TO_CURRENT_VCLIBS_VERSION=1)
    ADD_DEFINITIONS (-D_CRT_SECURE_NO_WARNINGS)
    ADD_DEFINITIONS (-D_CONSOLE)
  endif (NOT CYGWIN)
endif (WIN32)

if (MSVC)
  set (CMAKE_MFC_FLAG 0)
endif (MSVC)

set (MAKE_SYSTEM)
if (CMAKE_BUILD_TOOL MATCHES "make")
  set (MAKE_SYSTEM 1)
endif (CMAKE_BUILD_TOOL MATCHES "make")

set (CFG_INIT "/${CMAKE_CFG_INTDIR}")
if (MAKE_SYSTEM)
  set (CFG_INIT "")
endif (MAKE_SYSTEM)

#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCC)
  set (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS} -std=c99 -fomit-frame-pointer -finline-functions -fno-common")
endif (CMAKE_COMPILER_IS_GNUCC)
if (CMAKE_COMPILER_IS_GNUCXX)
  set (CMAKE_CXX_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_CXX_FLAGS} -fomit-frame-pointer -finline-functions -fno-common")
endif (CMAKE_COMPILER_IS_GNUCXX)

#-----------------------------------------------------------------------------
# HDF5 support
#-----------------------------------------------------------------------------
set (CMAKE_MODULE_PATH ${HDF_RESOURCES_DIR} ${CMAKE_MODULE_PATH})
set (CMAKE_MODULE_PATH ${HDF_RESOURCES_EXT_DIR} ${CMAKE_MODULE_PATH})

if (NOT USE_SHARED_LIBS)
  set (HDF5_USE_STATIC_LIBRARIES 1)
endif (NOT USE_SHARED_LIBS)

ADD_DEFINITIONS (-DH5_USE_18_API)

if (NOT HDF5_HDF5_HEADER)
  FIND_PACKAGE (HDF5 NAMES ${HDF5_PACKAGE_NAME})
  if (HDF5_FOUND)
    set (HDF5_HAVE_H5PUBCONF_H 1)
    set (HDF5_HAVE_HDF5 1)
    set (HDF5_HDF5_HEADER "h5pubconf.h")
    set (HDF5_INCLUDE_DIR_GEN ${HDF5_INCLUDE_DIR})
    message (STATUS "HDF5 found: INC=${HDF5_INCLUDE_DIR} TOOLS=${HDF5_TOOLS_DIR}")
    if ((NOT USE_SHARED_LIBS AND HDF5_BUILD_SHARED_LIBS) OR (USE_SHARED_LIBS AND NOT HDF5_BUILD_SHARED_LIBS))
      message (STATUS "HDF5 build type does match requested type")
    endif ((NOT USE_SHARED_LIBS AND HDF5_BUILD_SHARED_LIBS) OR (USE_SHARED_LIBS AND NOT HDF5_BUILD_SHARED_LIBS))
    if (HDF5_BUILD_SHARED_LIBS)
      ADD_DEFINITIONS (-DH5_BUILT_AS_DYNAMIC_LIB)
    else (HDF5_BUILD_SHARED_LIBS)
      ADD_DEFINITIONS (-DH5_BUILT_AS_STATIC_LIB)
    endif (HDF5_BUILD_SHARED_LIBS)
  else (HDF5_FOUND)
    message (FATAL_ERROR " HDF5 is Required for HDF5 Examples")
  endif (HDF5_FOUND)
else (NOT HDF5_HDF5_HEADER)
  # This project is being called from within another and HDF5 is already configured
  set (HDF5_HAVE_H5PUBCONF_H 1)
  set (HDF5_HAVE_HDF5 1)
endif (NOT HDF5_HDF5_HEADER)
add_executable (h5dump IMPORTED)
if (USE_SHARED_LIBS AND WIN32)
  set_property (TARGET h5dump PROPERTY IMPORTED_LOCATION "${HDF5_TOOLS_DIR}/h5dumpdll")
else (USE_SHARED_LIBS AND WIN32)
  set_property (TARGET h5dump PROPERTY IMPORTED_LOCATION "${HDF5_TOOLS_DIR}/h5dump")
endif (USE_SHARED_LIBS AND WIN32)
set (HDF5_DUMP_EXECUTABLE $<TARGET_FILE:h5dump>)
set (LINK_LIBS ${LINK_LIBS} ${HDF5_LIBRARIES})
INCLUDE_DIRECTORIES (${HDF5_INCLUDE_DIR})
if (HDF_BUILD_FORTRAN)
  INCLUDE_DIRECTORIES (${HDF5_INCLUDE_DIR_FORTRAN})
endif (HDF_BUILD_FORTRAN)

#-----------------------------------------------------------------------------
# Option to use Shared/Static libs, default is static
#-----------------------------------------------------------------------------
set (LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  set (LIB_TYPE SHARED)
else (BUILD_SHARED_LIBS)
  if (NOT WIN32)
    # should this be a user setting : Everyone uses it anyway ?
    ADD_DEFINITIONS (-DPIC)
  endif (NOT WIN32)
endif (BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
option (HDF_DISABLE_COMPILER_WARNINGS "Disable compiler warnings" OFF)
if (HDF_DISABLE_COMPILER_WARNINGS)
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF_WARNINGS_BLOCKED 1)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /w")
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w")
  endif (MSVC)
  if (WIN32 AND NOT CYGWIN)
    ADD_DEFINITIONS (-D_CRT_SECURE_NO_WARNINGS)
  endif (WIN32 AND NOT CYGWIN)
  # Borland uses -w- to suppress warnings.
  if (BORLAND)
    set (HDF_WARNINGS_BLOCKED 1)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-")
  endif (BORLAND)

  # Most compilers use -w to suppress warnings.
  if (NOT HDF_WARNINGS_BLOCKED)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
  endif (NOT HDF_WARNINGS_BLOCKED)
endif (HDF_DISABLE_COMPILER_WARNINGS)

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCC)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fmessage-length=0")
endif (CMAKE_COMPILER_IS_GNUCC)
if (CMAKE_COMPILER_IS_GNUCXX)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0")
endif (CMAKE_COMPILER_IS_GNUCXX)

#-----------------------------------------------------------------------------
# All examples need the main include directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR})

#-----------------------------------------------------------------------------
# Option to Enable MPI Parallel
#-----------------------------------------------------------------------------
option (HDF_ENABLE_PARALLEL "Enable parallel build (requires MPI)" OFF)
if (HDF_ENABLE_PARALLEL)
  include (FindMPI)
  INCLUDE_DIRECTORIES (${MPI_INCLUDE_PATH})
  if (MPI_FOUND)
    set (H5_HAVE_PARALLEL 1)
    # MPI checks, only do these if MPI_FOUND is true, otherwise they always fail
    # and once set, they are cached as false and not regenerated
    set (CMAKE_REQUIRED_LIBRARIES "${MPI_LIBRARY};${MPI_EXTRA_LIBRARY}" )
    CHECK_FUNCTION_EXISTS (MPI_File_get_size H5_HAVE_MPI_GET_SIZE) 
    # Used by Fortran + MPI
    CHECK_SYMBOL_EXISTS (MPI_Comm_c2f "${MPI_INCLUDE_PATH}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Comm)
    CHECK_SYMBOL_EXISTS (MPI_Info_c2f "${MPI_INCLUDE_PATH}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Info)
  endif (MPI_FOUND)
endif (HDF_ENABLE_PARALLEL)

# Parallel IO usage requires MPI to be Linked and Included
if (H5_HAVE_PARALLEL)
  set (LINK_LIBS ${LINK_LIBS} ${MPI_LIBRARY})
  if (MPI_LINK_FLAGS)
    set (CMAKE_EXE_LINKER_FLAGS ${MPI_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS})
  endif (MPI_LINK_FLAGS)
  if (MPI_EXTRA_LIBRARY)
    set (LINK_LIBS ${LINK_LIBS} ${MPI_EXTRA_LIBRARY})
  endif (MPI_EXTRA_LIBRARY)
endif (H5_HAVE_PARALLEL)

#-----------------------------------------------------------------------------
# Option to use threadsafe
# Note: Currently CMake only allows configuring of threadsafe on WINDOWS.
#-----------------------------------------------------------------------------
if (WIN32 AND NOT CYGWIN)
  option (HDF_ENABLE_THREADSAFE "Enable Threadsafety" OFF)
  if (HDF_ENABLE_THREADSAFE)
    # check for unsupported options
    if (HDF_ENABLE_PARALLEL)
      message (FATAL " **** Parallel and Threadsafe options are mutually exclusive **** ")
    endif (HDF_ENABLE_PARALLEL)
    set (H5_HAVE_WIN_THREADS 1)
    set (H5_HAVE_THREADSAFE 1)
  endif (HDF_ENABLE_THREADSAFE)
endif (WIN32 AND NOT CYGWIN)

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
option (BUILD_TESTING "Build HDF5 Example Testing" OFF)
if (BUILD_TESTING)
  set (DART_TESTING_TIMEOUT 1200
      CACHE INTEGER
      "Timeout in seconds for each test (default 1200=20minutes)"
  )
  enable_testing ()
  include (CTest)
  include (${PROJECT_SOURCE_DIR}/CTestConfig.cmake)
  configure_file (${HDF_RESOURCES_EXT_DIR}/CTestCustom.cmake ${PROJECT_BINARY_DIR}/CTestCustom.ctest @ONLY)
endif (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Build examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/C ${PROJECT_BINARY_DIR}/C)

#-----------------------------------------------------------------------------
# Option to build Fortran examples
# Make sure this appears before the CONFIGURE_FILE step
# so that fortran name mangling is detected before writing H5pubconf.h
#-----------------------------------------------------------------------------
# Set default name mangling : overridden by Fortran detection in fortran dir
set (H5_FC_FUNC  "H5_FC_FUNC(name,NAME) name ## _")
set (H5_FC_FUNC_ "H5_FC_FUNC_(name,NAME) name ## _")
if (EXISTS "${PROJECT_SOURCE_DIR}/FORTRAN" AND IS_DIRECTORY "${PROJECT_SOURCE_DIR}/FORTRAN")
  option (HDF_BUILD_FORTRAN "Build FORTRAN support" OFF)
  if (HDF_BUILD_FORTRAN AND HDF5_BUILD_FORTRAN)
    include (${HDF_RESOURCES_EXT_DIR}/HDFUseFortran.cmake)
    add_subdirectory (${PROJECT_SOURCE_DIR}/FORTRAN ${PROJECT_BINARY_DIR}/FORTRAN)
  endif (HDF_BUILD_FORTRAN AND HDF5_BUILD_FORTRAN)
endif (EXISTS "${PROJECT_SOURCE_DIR}/FORTRAN" AND IS_DIRECTORY "${PROJECT_SOURCE_DIR}/FORTRAN")

#-----------------------------------------------------------------------------
# Source Packaging Option
#-----------------------------------------------------------------------------
option (PACK_EXAMPLE_SOURCE "Package HDF5 Example Source File" OFF)
if (PACK_EXAMPLE_SOURCE)
#  set (CPACK_SOURCE_INSTALLED_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}") 
  set (CPACK_SOURCE_IGNORE_FILES "${PROJECT_BINARY_DIR}" "/\\.svn/" )
  INCLUDE(CPack)
endif (PACK_EXAMPLE_SOURCE)
