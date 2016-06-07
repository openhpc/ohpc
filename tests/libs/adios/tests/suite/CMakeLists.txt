add_subdirectory(programs)

file(COPY HOWTO-Add-new-test HOWTO-Run cleanup.sh test.sh DESTINATION ${PROJECT_BINARY_DIR}/tests/suite)

if(EXISTS ${PROJECT_BINARY_DIR}/tests/suite/reference)
  file(COPY reference/ DESTINATION ${PROJECT_BINARY_DIR}/tests/suite/reference)
else()
  file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/tests/suite/reference)
  file(COPY reference/ DESTINATION ${PROJECT_BINARY_DIR}/tests/suite/reference)
endif()

if(EXISTS ${PROJECT_BINARY_DIR}/tests/suite/tests)
  file(COPY tests/ DESTINATION ${PROJECT_BINARY_DIR}/tests/suite/tests)
else()
  file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/tests/suite/tests)
  file(COPY tests/ DESTINATION ${PROJECT_BINARY_DIR}/tests/suite/tests)
endif()

