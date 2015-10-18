include_directories(${PROJECT_SOURCE_DIR}/src/public)
include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/tests/C/flexpath_tests/include)
link_directories(${PROJECT_SOURCE_DIR}/src)
link_directories(${PROJECT_BINARY_DIR}/src)
link_directories(${MXML_INCLUDE_DIR}/../lib)


add_executable(writer_scalar writer.c ../common/utils.c)
target_link_libraries(writer_scalar adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES}) 

add_executable(reader_scalar reader.c ../common/utils.c)
target_link_libraries(reader_scalar adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES})

file(COPY test_config_flex.xml test_config_mpi.xml readme.txt DESTINATION ${PROJECT_BINARY_DIR}/tests/C/flexpath_tests/maya_append)
