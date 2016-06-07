cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5Examples_FORTRAN_H5T C CXX Fortran)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (f03examples
    h5ex_t_array_F03
    h5ex_t_arrayatt_F03
    h5ex_t_bit_F03
    h5ex_t_bitatt_F03
    h5ex_t_cmpd_F03
    h5ex_t_cmpdatt_F03
    h5ex_t_enum_F03
    h5ex_t_enumatt_F03
    h5ex_t_float_F03
    h5ex_t_floatatt_F03
    h5ex_t_int_F03
    h5ex_t_intatt_F03
    h5ex_t_objref_F03
    h5ex_t_objrefatt_F03
    h5ex_t_opaque_F03
    h5ex_t_opaqueatt_F03
    h5ex_t_regref_F03
    h5ex_t_regrefatt_F03
    h5ex_t_string_F03
    h5ex_t_stringC_F03
    h5ex_t_stringCatt_F03
    h5ex_t_vlen_F03
    h5ex_t_vlenatt_F03
#    h5ex_t_vlstring_F03
#    h5ex_t_vlstringatt_F03
#    h5ex_t_cpxcmpd_F03
#    h5ex_t_cpxcmpdatt_F03
#    h5ex_t_commit_F03
#    h5ex_t_convert_F03
)
set (examples
    h5ex_t_vlstring
#    h5ex_t_vlstringatt
#    h5ex_t_cpxcmpd
#    h5ex_t_cpxcmpdatt
#    h5ex_t_commit
#    h5ex_t_convert
)

if (HDF_ENABLE_F2003)
  foreach (example ${f03examples})
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
  endforeach (example ${f03examples})
endif (HDF_ENABLE_F2003)

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
  macro (ADD_H5_TEST testname)
    ADD_TEST (
        NAME f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.ddl.out
            ${testname}.ddl.out.err
            ${testname}.h5
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
  endmacro (ADD_H5_TEST)
  
  macro (ADD_H5_CMP_TEST testname)
    ADD_TEST (
        NAME f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            ${testname}.out
            ${testname}.out.err
            ${testname}.h5
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
  endmacro (ADD_H5_CMP_TEST)

  macro (TEST_EXAMPLE example)
    if (${example} STREQUAL "h5ex_t_cpxcmpd" OR ${example} STREQUAL "h5ex_t_cpxcmpdatt")  
      set (testdest "${PROJECT_BINARY_DIR}/${example}")
      #message (STATUS " Copying ${example}.test")
      add_custom_command (
          TARGET     f90_${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${testdest}.tst
      )
      if (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
        add_custom_command (
            TARGET     f90_${example}
            POST_BUILD
            COMMAND    ${CMAKE_COMMAND}
            ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.ddl ${testdest}.ddl
        )
      endif (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
      ADD_H5_TEST (${example} -n)
    elseif (${example} STREQUAL "h5ex_t_convert")  
      add_custom_command (
          TARGET     f90_${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${PROJECT_BINARY_DIR}/${example}.tst
      )
      ADD_H5_CMP_TEST (${example})
    else (${example} STREQUAL "h5ex_t_cpxcmpd" OR ${example} STREQUAL "h5ex_t_cpxcmpdatt")  
      set (testdest "${PROJECT_BINARY_DIR}/${example}")
      #message (STATUS " Copying ${example}.test")
      add_custom_command (
          TARGET     f90_${example}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.tst ${testdest}.tst
      )
      if (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
        add_custom_command (
            TARGET     f90_${example}
            POST_BUILD
            COMMAND    ${CMAKE_COMMAND}
            ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/${example}.ddl ${testdest}.ddl
        )
      endif (HDF5_BUILD_TOOLS AND NOT ${example} STREQUAL "h5ex_t_convert")
      ADD_H5_TEST (${example})
    endif (${example} STREQUAL "h5ex_t_cpxcmpd" OR ${example} STREQUAL "h5ex_t_cpxcmpdatt")  
  endmacro (TEST_EXAMPLE)

  if (HDF_ENABLE_F2003)
    foreach (example ${f03examples})
      TEST_EXAMPLE (${example})
    endforeach (example ${f03examples})
  endif (HDF_ENABLE_F2003)
  
  foreach (example ${examples})
    TEST_EXAMPLE (${example})
  endforeach (example ${examples})
  
endif (BUILD_TESTING)
  