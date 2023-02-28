cmake_minimum_required (VERSION 3.12)
PROJECT (HDF5Examples_C_PERFORM)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (C_sourcefiles.cmake)

foreach (example ${examples})
  get_filename_component (example_name ${example} NAME_WE)
  add_executable (${EXAMPLE_VARNAME}_${example_name} ${PROJECT_SOURCE_DIR}/${example})
  target_compile_options(${EXAMPLE_VARNAME}_${example_name}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_${example_name} ${LINK_LIBS})
endforeach ()

if (BUILD_TESTING)
  macro (ADD_H5_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.dmp.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.dmp"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname})
    endif ()
  endmacro ()

  foreach (example ${examples})
    get_filename_component (example_name ${example} NAME_WE)
    set (testdest "${PROJECT_BINARY_DIR}/${example_name}.test")
    #message (STATUS " Copying ${example_name}.test")
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_${example_name}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example_name}.test ${testdest}
    )
    ADD_H5_TEST (${example_name})
  endforeach ()
endif ()
