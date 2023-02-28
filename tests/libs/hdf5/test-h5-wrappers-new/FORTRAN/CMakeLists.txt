cmake_minimum_required (VERSION 3.12)
PROJECT (HDF5Examples_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# Build the Fortran Examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/H5D)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5G)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5T)
