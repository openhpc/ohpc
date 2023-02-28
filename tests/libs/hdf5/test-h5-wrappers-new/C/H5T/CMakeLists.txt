cmake_minimum_required (VERSION 3.12)
PROJECT (HDF5Examples_C_H5T)

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
              -D "TEST_OUTPUT=${testname}.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_CMP_TEST testname)
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
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
  endmacro ()

  foreach (example ${examples})
    get_filename_component (example_name ${example} NAME_WE)
    if (${example_name} STREQUAL "h5ex_t_cpxcmpd" OR ${example_name} STREQUAL "h5ex_t_cpxcmpdatt")
      set (testdest "${PROJECT_BINARY_DIR}/${example_name}")
      #message (STATUS " Copying ${example_name}.test")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}.tst ${testdest}.tst
      )
      if (HDF5_BUILD_TOOLS AND NOT ${example_name} STREQUAL "h5ex_t_convert")
        add_custom_command (
            TARGET     ${EXAMPLE_VARNAME}_${example_name}
            POST_BUILD
            COMMAND    ${CMAKE_COMMAND}
            ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}.ddl ${testdest}.ddl
        )
      endif ()
      ADD_H5_TEST (${example_name} -n)
    elseif (${example_name} STREQUAL "h5ex_t_convert")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
      )
      ADD_H5_CMP_TEST (${example_name})
    else ()
      set (testdest "${PROJECT_BINARY_DIR}/${example_name}")
      #message (STATUS " Copying ${example_name}.test")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}.tst ${testdest}.tst
      )
      if (HDF5_BUILD_TOOLS AND NOT ${example_name} STREQUAL "h5ex_t_convert")
        if (${${EXAMPLE_VARNAME}_USE_16_API} OR ${${EXAMPLE_VARNAME}_USE_18_API} OR ${${EXAMPLE_VARNAME}_USE_110_API})
          if ((HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.12") AND ((${example_name} STREQUAL "h5ex_t_objref" OR ${example_name} STREQUAL "h5ex_t_objrefatt") OR (${example_name} STREQUAL "h5ex_t_regref" OR ${example_name} STREQUAL "h5ex_t_regrefatt")))
            add_custom_command (
                TARGET     ${EXAMPLE_VARNAME}_${example_name}
                POST_BUILD
                COMMAND    ${CMAKE_COMMAND}
                ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/112/${example_name}.ddl ${testdest}.ddl
            )
          else ()
            add_custom_command (
                TARGET     ${EXAMPLE_VARNAME}_${example_name}
                POST_BUILD
                COMMAND    ${CMAKE_COMMAND}
                ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}.ddl ${testdest}.ddl
            )
          endif ()
        else ()
          if ((${example_name} STREQUAL "h5ex_t_objref" OR ${example_name} STREQUAL "h5ex_t_objrefatt") OR (${example_name} STREQUAL "h5ex_t_regref" OR ${example_name} STREQUAL "h5ex_t_regrefatt"))
            if (HDF5_VERSION_MAJOR VERSION_EQUAL "1.8" AND HDF5_VERSION_STRING VERSION_LESS_EQUAL "1.8.22")
              add_custom_command (
                  TARGET     ${EXAMPLE_VARNAME}_${example_name}
                  POST_BUILD
                  COMMAND    ${CMAKE_COMMAND}
                  ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}22.ddl ${testdest}.ddl
              )
            else ()
              add_custom_command (
                  TARGET     ${EXAMPLE_VARNAME}_${example_name}
                  POST_BUILD
                  COMMAND    ${CMAKE_COMMAND}
                  ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}.ddl ${testdest}.ddl
              )
            endif ()
          else ()
            add_custom_command (
                TARGET     ${EXAMPLE_VARNAME}_${example_name}
                POST_BUILD
                COMMAND    ${CMAKE_COMMAND}
                ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${example_name}.ddl ${testdest}.ddl
            )
          endif ()
        endif ()
      endif ()
      ADD_H5_TEST (${example_name})
    endif ()
  endforeach ()
endif ()
