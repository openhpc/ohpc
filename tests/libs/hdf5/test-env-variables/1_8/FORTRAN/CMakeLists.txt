cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# Build the Fortran Examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/H5D ${PROJECT_BINARY_DIR}/H5D)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5G ${PROJECT_BINARY_DIR}/H5G)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5T ${PROJECT_BINARY_DIR}/H5T)
