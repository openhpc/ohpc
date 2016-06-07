cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples_C_H5G)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
    h5ex_g_compact
    h5ex_g_corder
    h5ex_g_create
    h5ex_g_phase
    h5ex_g_iterate
    h5ex_g_traverse
    h5ex_g_intermediate
    h5ex_g_visit
)

foreach (example ${examples})
  add_executable (${example} ${PROJECT_SOURCE_DIR}/${example}.c)
  TARGET_NAMING (${example} ${LIB_TYPE})
  target_link_libraries (${example} ${LINK_LIBS})
endforeach (example ${examples})

if (BUILD_TESTING)
  macro (ADD_DUMP_TEST testname)
    ADD_TEST (
        NAME ${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.h5
    )
    ADD_TEST ( NAME ${testname} COMMAND $<TARGET_FILE:${testname}>)
    set_tests_properties (${testname} PROPERTIES DEPENDS ${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      ADD_TEST (
          NAME H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${testname} PROPERTIES DEPENDS ${testname})
    endif (HDF5_BUILD_TOOLS)
  endmacro (ADD_DUMP_TEST)

  macro (ADD_H5_DUMP_TEST testname)
    ADD_TEST (
        NAME ${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.ddl.out
            ${testname}.ddl.out.err
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
  endmacro (ADD_H5_DUMP_TEST)
  
  macro (ADD_H5_DUMP2_TEST testname)
    ADD_TEST (
        NAME ${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}1.ddl.out
            ${testname}1.ddl.out.err
            ${testname}1.h5
            ${testname}2.ddl.out
            ${testname}2.ddl.out.err
            ${testname}2.h5
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
          NAME H5DUMP-${testname}1
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${testname}1.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}1.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}1.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${testname}1 PROPERTIES DEPENDS ${testname})
      ADD_TEST (
          NAME H5DUMP-${testname}2
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${testname}2.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}2.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}2.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${testname}2 PROPERTIES DEPENDS H5DUMP-${testname}1)
    endif (HDF5_BUILD_TOOLS)
  endmacro (ADD_H5_DUMP2_TEST)
  
  macro (ADD_H5_CMP_TEST testname)
    ADD_TEST (
        NAME ${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.tmp
            ${testname}.out.err
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
  endmacro (ADD_H5_CMP_TEST testname)

  #message (STATUS " Copying h5ex_g_compact.test")
  add_custom_command (
      TARGET     h5ex_g_compact
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5ex_g_compact.tst ${PROJECT_BINARY_DIR}/h5ex_g_compact.tst
  )
    
  if (HDF5_BUILD_TOOLS)
    add_custom_command (
        TARGET     h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5ex_g_compact1.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact1.ddl
    )
    add_custom_command (
        TARGET     h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5ex_g_compact2.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact2.ddl
    )
    add_custom_command (
        TARGET     h5ex_g_create
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5ex_g_create.ddl ${PROJECT_BINARY_DIR}/h5ex_g_create.ddl
    )
    set (exrefs
        h5ex_g_corder
        h5ex_g_phase
        h5ex_g_iterate
        h5ex_g_traverse
        h5ex_g_intermediate
        h5ex_g_visit
    )
    foreach (example ${exrefs})
      add_custom_command (
          TARGET     ${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${PROJECT_BINARY_DIR}/${example}.tst
      )
    endforeach (example ${exrefs})
      
    set (exfiles
        h5ex_g_iterate
        h5ex_g_traverse
        h5ex_g_visit
    )
    foreach (example ${exfiles})
      add_custom_command (
          TARGET     ${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example}.h5 ${PROJECT_BINARY_DIR}/${example}.h5
      )
    endforeach (example ${exfiles})

  endif (HDF5_BUILD_TOOLS)

  ADD_H5_DUMP2_TEST (h5ex_g_compact)
  ADD_DUMP_TEST (h5ex_g_create)
  ADD_H5_CMP_TEST (h5ex_g_corder)
  ADD_H5_CMP_TEST (h5ex_g_phase)
  ADD_H5_CMP_TEST (h5ex_g_iterate)
  ADD_H5_CMP_TEST (h5ex_g_traverse)
  ADD_H5_CMP_TEST (h5ex_g_intermediate)
  ADD_H5_CMP_TEST (h5ex_g_visit)

endif (BUILD_TESTING)
  