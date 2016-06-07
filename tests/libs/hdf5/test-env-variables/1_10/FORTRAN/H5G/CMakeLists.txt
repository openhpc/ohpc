cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples_FORTRAN_H5G C CXX Fortran)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
#set (f03examples
#    h5ex_g_iterate_F03
#    h5ex_g_traverse_F03
#    h5ex_g_visit_F03
#)

set (examples
    h5ex_g_compact
    h5ex_g_corder
    h5ex_g_create
    h5ex_g_phase
)

#if (HDF_ENABLE_F2003)
#  foreach (example ${f03examples})
#    add_executable (f90_${example} ${PROJECT_SOURCE_DIR}/${example}.f90)
#    TARGET_NAMING (f90_${example} ${LIB_TYPE})
#    TARGET_FORTRAN_WIN_PROPERTIES (f90_${example} "")
#    if (WIN32 AND NOT CYGWIN)
#      set_property (TARGET f90_${example} 
#          APPEND PROPERTY COMPILE_DEFINITIONS 
#            HDF5F90_WINDOWS
#      )
#    endif (WIN32 AND NOT CYGWIN)
#    target_link_libraries (f90_${example} ${LINK_LIBS})
#    set_target_properties (f90_${example} PROPERTIES LINKER_LANGUAGE Fortran)
#  endforeach (example ${f03examples})
#endif (HDF_ENABLE_F2003)

foreach (example ${examples})
  add_executable (f90_${example} ${PROJECT_SOURCE_DIR}/${example}.f90)
  TARGET_NAMING (f90_${example} ${LIB_TYPE})
  TARGET_FORTRAN_WIN_PROPERTIES (f90_${example} "")
  if (WIN32 AND NOT CYGWIN)
    set_property (TARGET f90_${example} 
        APPEND PROPERTY COMPILE_DEFINITIONS 
          HDF5F90_WINDOWS
    )
  endif (WIN32 AND NOT CYGWIN)
  target_link_libraries (f90_${example} ${LINK_LIBS})
  set_target_properties (f90_${example} PROPERTIES LINKER_LANGUAGE Fortran)
endforeach (example ${examples})

if (BUILD_TESTING)
  macro (ADD_DUMP_TEST testname)
    ADD_TEST (
        NAME f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.h5
    )
    ADD_TEST ( NAME f90_${testname} COMMAND $<TARGET_FILE:f90_${testname}>)
    set_tests_properties (f90_${testname} PROPERTIES DEPENDS f90_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      ADD_TEST (
          NAME H5DUMP-f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-f90_${testname} PROPERTIES DEPENDS f90_${testname})
    endif (HDF5_BUILD_TOOLS)
  endmacro (ADD_DUMP_TEST)

  macro (ADD_H5_DUMP_TEST testname)
    ADD_TEST (
        NAME f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.ddl.out
            ${testname}.ddl.out.err
    )
    ADD_TEST (
        NAME f90_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:f90_${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (f90_${testname} PROPERTIES DEPENDS f90_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      ADD_TEST (
          NAME H5DUMP-f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-f90_${testname} PROPERTIES DEPENDS f90_${testname})
    endif (HDF5_BUILD_TOOLS)
  endmacro (ADD_H5_DUMP_TEST)
  
  macro (ADD_H5_DUMP2_TEST testname)
    ADD_TEST (
        NAME f90_${testname}-clearall
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
    if (${ARGN} STREQUAL "NULL")
      ADD_TEST (NAME f90_${testname} COMMAND $<TARGET_FILE:f90_${testname}>)
    else (${ARGN} STREQUAL "NULL")
      ADD_TEST (
          NAME f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:f90_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_REFERENCE=${testname}.tst"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
    endif (${ARGN} STREQUAL "NULL")
    set_tests_properties (f90_${testname} PROPERTIES DEPENDS f90_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      ADD_TEST (
          NAME H5DUMP-f90_${testname}1
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${testname}1.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}1.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}1.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-f90_${testname}1 PROPERTIES DEPENDS f90_${testname})
      ADD_TEST (
          NAME H5DUMP-f90_${testname}2
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${testname}2.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}2.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}2.ddl"
              -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-f90_${testname}2 PROPERTIES DEPENDS H5DUMP-f90_${testname}1)
    endif (HDF5_BUILD_TOOLS)
  endmacro (ADD_H5_DUMP2_TEST)
  
  macro (ADD_H5_CMP_TEST testname)
    ADD_TEST (
        NAME f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
    )
    ADD_TEST (
        NAME f90_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:f90_${testname}>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -P "${HDF5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (f90_${testname} PROPERTIES DEPENDS f90_${testname}-clearall)
  endmacro (ADD_H5_CMP_TEST testname)
    
  if (HDF5_BUILD_TOOLS)
    add_custom_command (
        TARGET     f90_h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5ex_g_compact1.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact1.ddl
    )
    add_custom_command (
        TARGET     f90_h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5ex_g_compact2.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact2.ddl
    )
    add_custom_command (
        TARGET     f90_h5ex_g_create
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5ex_g_create.ddl ${PROJECT_BINARY_DIR}/h5ex_g_create.ddl
    )

  endif (HDF5_BUILD_TOOLS)
  
  set (exrefs
      h5ex_g_corder
      h5ex_g_phase
  )
  foreach (example ${exrefs})
    add_custom_command (
        TARGET     f90_${example}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${PROJECT_BINARY_DIR}/${example}.tst
    )
  endforeach (example ${exrefs})
      
#  if (HDF_ENABLE_F2003)
#    set (exfiles
#        h5ex_g_iterate
#        h5ex_g_traverse
#        h5ex_g_visit
#    )
#    foreach (example ${exfiles})
#      add_custom_command (
#          TARGET     f90_${example}_F03
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}_F03.tst ${PROJECT_BINARY_DIR}/${example}_F03.tst
#      )
#      add_custom_command (
#          TARGET     f90_${example}_F03
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example}.h5 ${PROJECT_BINARY_DIR}/${example}.h5
#      )
#    endforeach (example ${exfiles})
#  endif (HDF_ENABLE_F2003)

  ADD_H5_DUMP2_TEST (h5ex_g_compact NULL)
  ADD_DUMP_TEST (h5ex_g_create)
  ADD_H5_CMP_TEST (h5ex_g_corder)
  ADD_H5_CMP_TEST (h5ex_g_phase)
#  if (HDF_ENABLE_F2003)
#    ADD_H5_CMP_TEST (h5ex_g_iterate_F03)
#    ADD_H5_CMP_TEST (h5ex_g_traverse_F03)
#    ADD_H5_CMP_TEST (h5ex_g_visit_F03)
#  endif (HDF_ENABLE_F2003)

endif (BUILD_TESTING)
  