include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/src/public)
include_directories(${PROJECT_SOURCE_DIR}/tests/C/flexpath_tests/include)
link_directories(${PROJECT_BINARY_DIR}/src)

add_executable(writer_global writer.c ../common/utils.c)
target_link_libraries(writer_global adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES}) 
#add_custom_target(
#    writer ALL
#    COMMAND $(CC) writer.c ../common/utils.c -o writer -I${PROJECT_SOURCE_DIR}/src/public -I${CMAKE_CURRENT_SOURCE_DIR}/../include -ladios -L${PROJECT_BINARY_DIR}/src ${ADIOSLIB_LDADD}
#    DEPENDS writer.c ../common/utils.c
#    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})


add_executable(reader_global reader.c ../common/utils.c)
target_link_libraries(reader_global adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES})
#add_custom_target(
#    reader ALL
#    COMMAND $(CC) reader.c ../common/utils.c -o reader -I${PROJECT_SOURCE_DIR}/src/public -I${CMAKE_CURRENT_SOURCE_DIR}/../include -ladios -L${PROJECT_BINARY_DIR}/src ${ADIOSLIB_LDADD}
#    DEPENDS reader.c ../common/utils.c
#    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

file(COPY test_config_flex.xml readme.txt DESTINATION ${PROJECT_BINARY_DIR}/tests/C/flexpath_tests/1D_arr_global)
