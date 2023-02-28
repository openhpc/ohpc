cmake_minimum_required (VERSION 3.12)
PROJECT (H5Parallel_C)

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
  target_include_directories (${EXAMPLE_VARNAME}_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
  target_link_libraries (${EXAMPLE_VARNAME}_${example_name} ${LINK_LIBS})
endforeach ()

if (BUILD_TESTING)
  macro (ADD_GREP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    if (last_test)
      set_tests_properties (${EXAMPLE_VARNAME}_${testname}-clearall PROPERTIES DEPENDS ${last_test})
    endif ()
    add_test (NAME ${EXAMPLE_VARNAME}_${testname} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=${MPIEXEC_EXECUTABLE};${MPIEXEC_NUMPROC_FLAG};${NUMPROCS};${MPIEXEC_PREFLAGS};$<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>;${MPIEXEC_POSTFLAGS}"
        -D "TEST_ARGS:STRING="
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=${testname}.out"
        -D "TEST_REFERENCE:STRING=PHDF5 example finished with no errors"
        -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
        -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/grepTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
    set (last_test "${EXAMPLE_VARNAME}_${testname}")
  endmacro ()

  # Ensure that 24 is a multiple of the number of processes.
  # The number 24 corresponds to SPACE1_DIM1 and SPACE1_DIM2 defined in ph5example.c
  math(EXPR NUMPROCS "24 / ((24 + ${MPIEXEC_MAX_NUMPROCS} - 1) / ${MPIEXEC_MAX_NUMPROCS})")

  foreach (example ${examples})
    get_filename_component (example_name ${example} NAME_WE)
    ADD_GREP_TEST (${example_name})
  endforeach ()

endif ()
