cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples_C_PERFORM)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
    h5slabread
    h5slabwrite
    h5efc
)

foreach (example ${examples})
  add_executable (${example} ${PROJECT_SOURCE_DIR}/${example}.c)
  TARGET_NAMING (${example} ${LIB_TYPE})
  target_link_libraries (${example} ${LINK_LIBS})
endforeach (example ${examples})

if (BUILD_TESTING)
  macro (ADD_H5_TEST testname)
    ADD_TEST (
        NAME ${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.dmp.out
            ${testname}.dmp.out.err
            ${testname}.h5
    )
    ADD_TEST (
        NAME ${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${testname} PROPERTIES DEPENDS ${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      ADD_TEST (
          NAME H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.dmp.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.dmp"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${testname} PROPERTIES DEPENDS ${testname})
    endif (HDF5_BUILD_TOOLS)
  endmacro (ADD_H5_TEST)

  foreach (example ${examples})
    set (testdest "${PROJECT_BINARY_DIR}/${example}.test")
    #message (STATUS " Copying ${example}.test")
    add_custom_command (
        TARGET     ${example}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example}.test ${testdest}
    )
    ADD_H5_TEST (${example})
  endforeach (example ${examples})
endif (BUILD_TESTING)
  