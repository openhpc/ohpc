include_directories(${PROJECT_SOURCE_DIR}/tests/suite)
include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/src/public)
include_directories(${PROJECT_SOURCE_DIR}/tests/suite/programs)
include_directories(${PROJECT_BINARY_DIR}/tests/suite/programs)
link_directories(${PROJECT_BINARY_DIR}/tests/suite/programs)

set (WRITE_PROGS1 local_array_time 
  adios_amr_write 
  adios_amr_write_2vars
  write_read 
  write_alternate 
  two_groups
  many_vars 
  selections 
  path_test 
  reuse_dim 
  adios_transforms_read_write
  connect_to_space_subset
  big_file
  set_path
  set_path_var
  steps_write
  blocks
  build_standard_dataset)

set(WRITE_PROGS2 adios_staged_read
                 adios_staged_read_v2 
                 adios_staged_read_2vars
                 steps_read_file
                 steps_read_stream
                 transforms_writeblock_read)

set(WRITE_PROGS ${WRITE_PROGS1}
                ${WRITE_PROGS2}
                copy_subvolume
                transforms_specparse
                hashtest
                group_free_test)

if(BUILD_WRITE)
  foreach (PROG ${WRITE_PROGS} )
    add_executable(${PROG} ${PROG}.c)
    if(MPI_COMPILE_FLAGS)
      set_target_properties(${PROG} PROPERTIES COMPILE_FLAGS "${MPI_C_COMPILE_FLAGS}")
    endif()
    if(MPI_LINK_FLAGS)
      set_target_properties(${PROG} PROPERTIES LINK_FLAGS "${MPI_C_LINK_FLAGS}")
    endif()
  endforeach()

  foreach (PROG ${WRITE_PROGS1} )
    target_link_libraries(${PROG} adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES})
  endforeach()  

  foreach (PROG ${WRITE_PROGS2} )
    target_link_libraries(${PROG} adiosread ${ADIOSREADLIB_LDADD} ${MPI_C_LIBRARIES})
  endforeach()

  target_link_libraries(copy_subvolume adiosread_nompi ${ADIOSREADLIB_SEQ_LDADD})
  target_link_libraries(transforms_specparse adios_nompi ${ADIOSLIB_SEQ_LDADD})
#  target_link_libraries(hashtest ${PROJECT_BINARY_DIR}/src/libadios_a-qhashtbl.o)
  target_link_libraries(hashtest adios ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES})
  target_link_libraries(group_free_test adios_nompi ${ADIOSLIB_SEQ_LDADD})
endif(BUILD_WRITE)

if(BUILD_FORTRAN)
  if(BUILD_WRITE)
    add_executable(posix_method posix_method.F90 gwrite_posix_method.fh)
    target_link_libraries(posix_method adiosf ${ADIOSLIB_LDADD} ${MPI_C_LIBRARIES})
    include_directories(${PROJECT_BINARY_DIR}/src)
    add_custom_command(
	OUTPUT gwrite_posix_method.fh
	COMMAND ${PROJECT_SOURCE_DIR}/utils/gpp/gpp.py ${PROJECT_SOURCE_DIR}/tests/suite/programs/posix_method.xml
	DEPENDS posix_method.xml
	)
  endif(BUILD_WRITE)
endif(BUILD_FORTRAN)

file(COPY adios_amr_write.xml adios_amr_write_2vars.xml posix_method.xml
          local_array_time.xml write_alternate.xml write_read.xml transforms.xml
          path_test.xml adios_transforms.xml set_path.xml set_path_var.xml
          two_groups.xml
     DESTINATION ${PROJECT_BINARY_DIR}/tests/suite/programs)
