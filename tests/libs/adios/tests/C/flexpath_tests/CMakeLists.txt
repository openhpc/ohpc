add_subdirectory(1D_arr_global)
add_subdirectory(1D_arr_global_noxml)
add_subdirectory(global_range_select)
add_subdirectory(maya_noxml)
add_subdirectory(maya_append)
add_subdirectory(scalar)

file(COPY README DESTINATION ${PROJECT_BINARY_DIR}/tests/C/flexpath_tests/)
