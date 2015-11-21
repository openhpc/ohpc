cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5Examples_C_H5D)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
    h5ex_d_alloc
    h5ex_d_checksum
    h5ex_d_chunk
    h5ex_d_compact
    h5ex_d_extern
    h5ex_d_fillval
    h5ex_d_gzip
    h5ex_d_hyper
    h5ex_d_nbit
    h5ex_d_rdwr
    h5ex_d_soint
    h5ex_d_szip
    h5ex_d_transform
    h5ex_d_unlimmod
    h5ex_d_shuffle
    h5ex_d_sofloat
    h5ex_d_unlimadd
    h5ex_d_unlimgzip
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

  foreach (example ${examples})
    set (testdest "${PROJECT_BINARY_DIR}/${example}")
    #message (STATUS " Copying ${example}.test")
    add_custom_command (
        TARGET     ${example}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${testdest}.tst
    )
    if (HDF5_BUILD_TOOLS)
      add_custom_command (
          TARGET     ${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.ddl ${testdest}.ddl
      )
    endif (HDF5_BUILD_TOOLS)
    
    if (${example} STREQUAL "h5ex_d_transform")  
      ADD_H5_TEST (${example} -n)
    else (${example} STREQUAL "h5ex_d_transform")  
      ADD_H5_TEST (${example})
    endif (${example} STREQUAL "h5ex_d_transform")  
  endforeach (example ${examples})
endif (BUILD_TESTING)
  