include_directories(${PROJECT_BINARY_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/tests/Fortran)
link_directories(${PROJECT_BINARY_DIR}/tests/Fortran)

add_executable(adios_test_f adios_test_f.F90)
target_link_libraries(adios_test_f adiosf ${ADIOSLIB_LDADD})
set_target_properties(adios_test_f PROPERTIES COMPILE_FLAGS "${ADIOSLIB_CPPFLAGS} ${ADIOSLIB_CFLAGS}")

file(COPY config_fortran.xml DESTINATION ${PROJECT_BINARY_DIR}/tests/Fortran)
