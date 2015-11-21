include_directories(${PROJECT_SOURCE_DIR}/src/public)
include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../common)
link_directories(${PROJECT_BINARY_DIR}/src)
link_directories(${CMAKE_CURRENT_BINARY_DIR}../common)
link_directories(${MXML_INCLUDE_DIR}/../lib)


add_executable(adios_read_block adios_read_block.c)
target_link_libraries(adios_read_block adios ${ADIOSLIB_LDADD})
set_target_properties(adios_read_block PROPERTIES COMPILE_FLAGS "${ALACRITY_CPPFLAGS}")

add_executable(adios_read_ext_test adios_read_ext_test.c)
target_link_libraries(adios_read_ext_test adios ${ADIOSLIB_LDADD}) 
set_target_properties(adios_read_ext_test PROPERTIES COMPILE_FLAGS "${ALACRITY_CPPFLAGS}")

add_executable(adios_build_alac_index adios_build_alac_index.c)
target_link_libraries(adios_build_alac_index adios ${ADIOSLIB_LDADD})
set_target_properties(adios_build_alac_index PROPERTIES COMPILE_FLAGS "${ALACRITY_CPPFLAGS}")

add_executable(adios_alac_query adios_alac_query.c)
target_link_libraries(adios_alac_query parse_test_query_xml adios ${ADIOSLIB_LDADD})
set_target_properties(adios_alac_query PROPERTIES COMPILE_FLAGS "${ALACRITY_CPPFLAGS}")

add_executable(adios_show_bp_file adios_show_bp_file.c)
target_link_libraries(adios_show_bp_file adios ${ADIOSLIB_LDADD})

file(COPY xml
     DESTINATION ${PROJECT_BINARY_DIR}/tests/C/query/alacrity)
