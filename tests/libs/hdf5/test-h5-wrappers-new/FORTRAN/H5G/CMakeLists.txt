cmake_minimum_required (VERSION 3.12)
PROJECT (HDF5Examples_FORTRAN_H5G C CXX Fortran)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (Fortran_sourcefiles.cmake)

#if (HDF_ENABLE_F2003)
#  foreach (example ${f03examples})
#    get_filename_component (example_name ${example} NAME_WE)
#    add_executable (f90_${example_name} ${PROJECT_SOURCE_DIR}/${example})
#    target_compile_options(${EXAMPLE_VARNAME}_f90_${example_name}
#        PRIVATE
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
#    )
#    if (H5_HAVE_PARALLEL)
#      target_include_directories (f90_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
#    endif ()
#    target_link_libraries (f90_${example_name} ${LINK_Fortran_LIBS})
#    set_target_properties (f90_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
#  endforeach (example ${f03examples})
#endif ()

foreach (example ${examples})
  get_filename_component (example_name ${example} NAME_WE)
  add_executable (${EXAMPLE_VARNAME}_f90_${example_name} ${PROJECT_SOURCE_DIR}/${example})
  target_compile_options(${EXAMPLE_VARNAME}_f90_${example_name}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_f90_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${LINK_Fortran_LIBS})
  set_target_properties (${EXAMPLE_VARNAME}_f90_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
endforeach ()

if (BUILD_TESTING)
  macro (ADD_DUMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_SKIP_COMPARE=TRUE"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_DUMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
    )
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_DUMP2_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}1.h5
            ${testname}2.h5
    )
    if (${ARGN} STREQUAL "NULL")
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_REFERENCE=${testname}.tst"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}1
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${testname}1.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}1.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}1.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}1 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}2
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${testname}2.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}2.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}2.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}2 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}1)
    endif ()
  endmacro ()

  macro (ADD_H5_CMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
    )
  endmacro ()

  if (HDF5_BUILD_TOOLS)
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/h5ex_g_compact1.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact1.ddl
    )
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/h5ex_g_compact2.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact2.ddl
    )
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_h5ex_g_create
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/h5ex_g_create.ddl ${PROJECT_BINARY_DIR}/h5ex_g_create.ddl
    )

  endif ()

  set (exrefs
      h5ex_g_corder
      h5ex_g_phase
  )
  foreach (example ${exrefs})
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_${example}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example}.tst ${PROJECT_BINARY_DIR}/${example}.tst
    )
  endforeach ()

#  if (HDF_ENABLE_F2003)
#    set (exfiles
#        h5ex_g_iterate
#        h5ex_g_traverse
#        h5ex_g_visit
#    )
#    foreach (example ${exfiles})
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example}_F03
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example}_F03.tst ${PROJECT_BINARY_DIR}/${example}_F03.tst
#      )
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example}_F03
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example}.h5 ${PROJECT_BINARY_DIR}/${example}.h5
#      )
#    endforeach ()
#  endif ()

  ADD_H5_DUMP2_TEST (h5ex_g_compact NULL)
  ADD_DUMP_TEST (h5ex_g_create)
  ADD_H5_CMP_TEST (h5ex_g_corder)
  ADD_H5_CMP_TEST (h5ex_g_phase)
#  if (HDF_ENABLE_F2003)
#    ADD_H5_CMP_TEST (h5ex_g_iterate_F03)
#    ADD_H5_CMP_TEST (h5ex_g_traverse_F03)
#    ADD_H5_CMP_TEST (h5ex_g_visit_F03)
#  endif ()

endif ()
