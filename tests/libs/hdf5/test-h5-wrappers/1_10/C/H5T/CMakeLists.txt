cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples_C_H5T)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
    h5ex_t_array
    h5ex_t_arrayatt
    h5ex_t_bit
    h5ex_t_bitatt
    h5ex_t_cmpd
    h5ex_t_cmpdatt
    h5ex_t_enum
    h5ex_t_enumatt
    h5ex_t_float
    h5ex_t_floatatt
    h5ex_t_int
    h5ex_t_intatt
    h5ex_t_objref
    h5ex_t_objrefatt
    h5ex_t_opaque
    h5ex_t_opaqueatt
    h5ex_t_regref
    h5ex_t_regrefatt
    h5ex_t_string
    h5ex_t_stringatt
    h5ex_t_vlen
    h5ex_t_vlenatt
    h5ex_t_vlstring
    h5ex_t_vlstringatt
    h5ex_t_cpxcmpd
    h5ex_t_cpxcmpdatt
    h5ex_t_commit
    h5ex_t_convert
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
            ${testname}.ddl.out
            ${testname}.ddl.out.err
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
              -D "TEST_OUTPUT=${testname}.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${testname} PROPERTIES DEPENDS ${testname})
    endif (HDF5_BUILD_TOOLS)
  endmacro (ADD_H5_TEST)
  
  macro (ADD_H5_CMP_TEST testname)
    ADD_TEST (
        NAME ${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.h5
    )
    ADD_TEST (
        NAME ${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${testname}>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${testname} PROPERTIES DEPENDS ${testname}-clearall)
  endmacro (ADD_H5_CMP_TEST)

  foreach (example ${examples})
    if (${example} STREQUAL "h5ex_t_cpxcmpd" OR ${example} STREQUAL "h5ex_t_cpxcmpdatt")  
      set (testdest "${PROJECT_BINARY_DIR}/${example}")
      #message (STATUS " Copying ${example}.test")
      add_custom_command (
          TARGET     ${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${testdest}.tst
      )
      if (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
        add_custom_command (
            TARGET     ${example}
            POST_BUILD
            COMMAND    ${CMAKE_COMMAND}
            ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.ddl ${testdest}.ddl
        )
      endif (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
      ADD_H5_TEST (${example} -n)
    elseif (${example} STREQUAL "h5ex_t_convert")  
      add_custom_command (
          TARGET     ${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${PROJECT_BINARY_DIR}/${example}.tst
      )
      ADD_H5_CMP_TEST (${example})
    else (${example} STREQUAL "h5ex_t_cpxcmpd" OR ${example} STREQUAL "h5ex_t_cpxcmpdatt")  
      set (testdest "${PROJECT_BINARY_DIR}/${example}")
      #message (STATUS " Copying ${example}.test")
      add_custom_command (
          TARGET     ${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${testdest}.tst
      )
      if (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
        add_custom_command (
            TARGET     ${example}
            POST_BUILD
            COMMAND    ${CMAKE_COMMAND}
            ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.ddl ${testdest}.ddl
        )
      endif (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
      ADD_H5_TEST (${example})
    endif (${example} STREQUAL "h5ex_t_cpxcmpd" OR ${example} STREQUAL "h5ex_t_cpxcmpdatt")  
  endforeach (example ${examples})
endif (BUILD_TESTING)
  