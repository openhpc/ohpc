include_directories(${PROJECT_SOURCE_DIR}/src/public)
include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/tests/C/flexpath_tests/include)
link_directories(${PROJECT_SOURCE_DIR}/src)
link_directories(${PROJECT_BINARY_DIR}/src)
link_directories(${MXML_INCLUDE_DIR}/../lib)


add_executable(writer_global_noxml writer.c ../common/utils.c)
target_link_libraries(writer_global_noxml adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES}) 

add_executable(reader_global_noxml reader.c ../common/utils.c)
target_link_libraries(reader_global_noxml adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES})

file(COPY readme.txt DESTINATION ${PROJECT_BINARY_DIR}/tests/C/flexpath_tests/1D_arr_global_noxml)
