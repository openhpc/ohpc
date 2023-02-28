cmake_minimum_required (VERSION 3.12)
project (HDF5Filters_C)

set (dyn_examples)

option (ENABLE_BLOSC "Enable Library Building for blosc plugin" ON)
if (ENABLE_BLOSC)
  if (WIN32)
    if (NOT CMAKE_C_COMPILER_ID MATCHES "[Cc]lang" AND MSVC_VERSION GREATER 1600)
      set (BLOSC_AVAILABLE 1)
      set (dyn_examples ${dyn_examples} h5ex_d_blosc)
    else ()
      set (BLOSC_AVAILABLE 0)
    endif ()
  elseif (APPLE)
    if (NOT CMAKE_C_COMPILER_ID STREQUAL "Intel")
      set (BLOSC_AVAILABLE 1)
      set (dyn_examples ${dyn_examples} h5ex_d_blosc)
    endif ()
  else ()
    set (BLOSC_AVAILABLE 0)
  endif ()
else ()
  set (BLOSC_AVAILABLE 0)
endif ()

option (ENABLE_BSHUF "Enable Library Building for bshuf plugin" ON)
if (ENABLE_BSHUF)
  if (NOT CMAKE_C_COMPILER_ID STREQUAL "Intel")
    set (BSHUF_AVAILABLE 1)
    set (dyn_examples ${dyn_examples} h5ex_d_bshuf)
  else ()
    set (BSHUF_AVAILABLE 0)
  endif ()
else ()
  set (BSHUF_AVAILABLE 0)
endif ()

option (ENABLE_BZIP2 "Enable Library Building for bzip2 plugin" ON)
if (ENABLE_BZIP2)
  set (BZIP2_AVAILABLE 1)
  set (dyn_examples ${dyn_examples} h5ex_d_bzip2)
else ()
  set (BZIP2_AVAILABLE 0)
endif ()

option (ENABLE_FPZIP "Enable Library Building for fpzip plugin" OFF)
if (ENABLE_FPZIP)
  set (FPZIP_AVAILABLE 1)
  set (dyn_examples ${dyn_examples} h5ex_d_fpzip)
else ()
  set (FPZIP_AVAILABLE 0)
endif ()
option (ENABLE_JPEG "Enable Library Building for jpeg plugin" ON)
if (ENABLE_JPEG)
  if (NOT WIN32)
    set (JPEG_AVAILABLE 1)
    set (dyn_examples ${dyn_examples} h5ex_d_jpeg)
  else ()
    set (JPEG_AVAILABLE 0)
  endif ()
else ()
  set (JPEG_AVAILABLE 0)
endif ()

option (ENABLE_LZ4 "Enable Library Building for lz4 plugin" ON)
if (ENABLE_MAFISC)
  set (LZ4_AVAILABLE 1)
  set (dyn_examples ${dyn_examples} h5ex_d_lz4)
else ()
  set (LZ4_AVAILABLE 0)
endif ()

option (ENABLE_LZF "Enable Library Building for lzf plugin" ON)
if (ENABLE_LZF)
  set (LZF_AVAILABLE 1)
  set (dyn_examples ${dyn_examples} h5ex_d_lzf)
else ()
  set (LZF_AVAILABLE 0)
endif ()

option (ENABLE_MAFISC "Enable Library Building for mafisc plugin" OFF)
if (ENABLE_MAFISC)
  set (MAFISC_AVAILABLE 1)
  set (dyn_examples ${dyn_examples} h5ex_d_mafisc)
else ()
  set (MAFISC_AVAILABLE 0)
endif ()
option (ENABLE_SZ "Enable Library Building for sz plugin" OFF)
if (ENABLE_SZ)
  if (WIN32 AND MSVC_VERSION GREATER 1900)
    if (CMAKE_C_COMPILER_ID MATCHES "[Cc]lang" OR CMAKE_C_COMPILER_ID STREQUAL "Intel")
      set (SZ_AVAILABLE 1)
      set (dyn_examples ${dyn_examples} h5ex_d_sz)
    else ()
      set (SZ_AVAILABLE 0)
    endif ()
  elseif (NOT WIN32)
    if(CMAKE_C_COMPILER_ID MATCHES "[Cc]lang" AND NOT CMAKE_C_COMPILER_ID MATCHES "Apple[Cc]lang")
      set (SZ_AVAILABLE 1)
      set (dyn_examples ${dyn_examples} h5ex_d_sz)
    else ()
      set (SZ_AVAILABLE 0)
    endif ()
  else ()
    set (SZ_AVAILABLE 0)
  endif ()
else ()
  set (SZ_AVAILABLE 0)
endif ()

option (ENABLE_ZFP "Enable Library Building for zfp plugin" OFF)
if (ENABLE_ZFP)
  set (ZFP_AVAILABLE 1)
  set (dyn_examples ${dyn_examples} h5ex_d_zfp)
else ()
  set (ZFP_AVAILABLE 0)
endif ()

option (ENABLE_ZSTD "Enable Library Building for zstd plugin" ON)
if (ENABLE_ZSTD)
  if (WIN32)
    if (CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
      set (ZSTD_AVAILABLE 1)
      set (dyn_examples ${dyn_examples} h5ex_d_zstd)
    else ()
      set (ZSTD_AVAILABLE 0)
    endif ()
  elseif (APPLE)
    if (NOT CMAKE_C_COMPILER_ID STREQUAL "Intel")
      set (ZSTD_AVAILABLE 1)
      set (dyn_examples ${dyn_examples} h5ex_d_zstd)
    else ()
      set (ZSTD_AVAILABLE 0)
    endif ()
  else ()
    set (ZSTD_AVAILABLE 1)
    set (dyn_examples ${dyn_examples} h5ex_d_zstd)
  endif ()
else ()
  set (ZSTD_AVAILABLE 0)
endif ()

#run-time loadable library examples
foreach (example ${dyn_examples})
  add_executable (${EXAMPLE_VARNAME}_${example} ${PROJECT_SOURCE_DIR}/${example}.c)
  TARGET_C_PROPERTIES (${EXAMPLE_VARNAME}_${example} ${LIB_TYPE} " " " ")
  target_compile_options(${EXAMPLE_VARNAME}_${example}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_${example} PUBLIC ${MPI_C_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_${example} PRIVATE ${LINK_LIBS})
  if (NOT WIN32)
    target_link_libraries (${EXAMPLE_VARNAME}_${example} PRIVATE dl)
  endif ()
endforeach ()

if (BUILD_TESTING)
  macro (ADD_H5_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.out
            ${testname}.out.err
            ${testname}.ddl.out
            ${testname}.ddl.out.err
            ${testname}.h5
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (${EXAMPLE_VARNAME}_${testname}-clearall PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "${EXAMPLE_VARNAME}_${testname}-clearall")
    if (WIN32)
      set(TESTLIBDIR "${HDF5_TOOLS_DIR}")
    else ()
      set(TESTLIBDIR "${HDF5_LIBRARY_PATH}")
    endif ()
    if (DISABLE_H5PL_ENCODER)
      add_test (
          NAME ${EXAMPLE_VARNAME}_${testname}-ERR
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=1"
              -D "TEST_MASK_ERROR=true"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_REFERENCE=${testname}.tst"
              -D "TEST_SKIP_COMPARE=1"
              -D "TEST_ERRREF=1"
              -D "GREP_ERRREF=Filter present but encoding disabled"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_${testname}-ERR PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
      add_test (
          NAME ${EXAMPLE_VARNAME}_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${testname}.h5" "${PROJECT_BINARY_DIR}/${testname}.h5"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-ERR)
    else ()
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
    endif ()
    set (last_test "${EXAMPLE_VARNAME}_${testname}")
    if (HDF5_BUILD_TOOLS)
      if (NOT "${ARGN}" STREQUAL "FILTERALL")
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}
            COMMAND "${CMAKE_COMMAND}"
               -D "TEST_PROGRAM=${HDF5_DUMP_EXECUTABLE}"
                -D "TEST_ARGS:STRING=--enable-error-stack;-p;${testname}.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${testname}.ddl.out"
                -D "TEST_FILTER=PARAMS { ([0-9]) [-]?[0-9]+ ([0-9] [0-9] [0-9] [0-9] [0-9] [0-9]) }\n"
                -D "TEST_FILTER_REPLACE=PARAMS { \\1 XXXX \\2 }\n"
                -D "TEST_EXPECT=0"
                -D "TEST_REFERENCE=${testname}.ddl"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=${HDF5_DUMP_EXECUTABLE}"
                -D "TEST_ARGS:STRING=--enable-error-stack;-p;${testname}.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${testname}.ddl.out"
                -D "TEST_FILTER:STRING=PARAMS {[ -0-9]*}"
                -D "TEST_FILTER_REPLACE:STRING=PARAMS { XXXX }"
                -D "TEST_EXPECT=0"
                -D "TEST_REFERENCE=${testname}.ddl"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname})
      set (last_test "${EXAMPLE_VARNAME}_H5DUMP-${testname}")
    endif ()
  endmacro ()

  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  foreach (h5_file ${dyn_examples})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${h5_file}.tst" "${PROJECT_BINARY_DIR}/${h5_file}.tst" "example_files")
    if (WIN32 AND MSVC_VERSION LESS 1900 AND ${h5_file} MATCHES "h5ex_d_zfp")
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/h5ex_d_zfp.wddl" "${PROJECT_BINARY_DIR}/h5ex_d_zfp.ddl" "example_files")
    else ()
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${h5_file}.ddl" "${PROJECT_BINARY_DIR}/${h5_file}.ddl" "example_files")
    endif()
  endforeach ()
  foreach (h5_file ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${H5_LIBVER_DIR}/${h5_file}" "${PROJECT_BINARY_DIR}/${h5_file}" "example_files")
  endforeach ()
  add_custom_target(${EXAMPLE_VARNAME}_example_files ALL COMMENT "Copying files needed by example tests" DEPENDS ${example_files_list})

  foreach (h5_file ${dyn_examples})
    if (${h5_file} MATCHES "h5ex_d_zfp")
      ## special filter
      ADD_H5_TEST (h5ex_d_zfp FILTERALL)
    else ()
      ADD_H5_TEST (${h5_file})
    endif ()
  endforeach ()

endif ()
