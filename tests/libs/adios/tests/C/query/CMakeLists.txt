add_subdirectory(common)

if(HAVE_ALACRITY)
  add_subdirectory(alacrity)
endif()

if(HAVE_FASTBIT)
  add_subdirectory(fastbit)
endif()

file(COPY query-xmls
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
