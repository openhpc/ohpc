cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples_C)

#-----------------------------------------------------------------------------
# Shared/Static Libs
#-----------------------------------------------------------------------------
if (USE_SHARED_LIBS)
  set (C_BUILT_AS_DYNAMIC_LIB 1)
else (USE_SHARED_LIBS)
  set (C_BUILT_AS_STATIC_LIB 1)
endif (USE_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Build the C Examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/H5D ${PROJECT_BINARY_DIR}/H5D)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5G ${PROJECT_BINARY_DIR}/H5G)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5T ${PROJECT_BINARY_DIR}/H5T)
#add_subdirectory (${PROJECT_SOURCE_DIR}/Performance ${PROJECT_BINARY_DIR}/Performance)
