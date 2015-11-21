include_directories(${PROJECT_SOURCE_DIR}/src/public)
include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../common)
link_directories(${PROJECT_BINARY_DIR}/src)
link_directories(${CMAKE_CURRENT_BINARY_DIR}../common)
link_directories(${MXML_INCLUDE_DIR}/../lib)
include_directories(${MXML_INCLUDE_DIR})

add_executable(fastbit_tests fastbit_tests.c)
target_link_libraries(fastbit_tests adiosread_nompi adios_nompi ${ADIOSREADLIB_LDADD} ${MXML_LIBS})
set_target_properties(fastbit_tests PROPERTIES COMPILE_FLAGS "${MACRODEFFLAG}_NOMPI")

file(COPY query.xml
     DESTINATION ${PROJECT_BINARY_DIR}/tests/C/query/fastbit)
