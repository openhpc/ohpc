include_directories(${PROJECT_SOURCE_DIR}/src/public)
include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/test/C/flexpath_tests/include)
link_directories(${PROJECT_SOURCE_DIR}/src)
link_directories(${PROJECT_BINARY_DIR}/src)
link_directories(${MXML_INCLUDE_DIR}/../lib)


add_executable(arrays_writer arrays_write.c)
target_link_libraries(arrays_writer adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES}) 

add_executable(arrays_reader arrays_read.c)
target_link_libraries(arrays_reader adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES})

file(COPY arrays.xml DESTINATION ${PROJECT_BINARY_DIR}/tests/C/flexpath_tests/global_range_select)
