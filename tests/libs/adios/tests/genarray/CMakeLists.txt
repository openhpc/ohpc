include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(${PROJECT_BINARY_DIR}/src)
include_directories(${PROJECT_SOURCE_DIR}/tests/genarray)
include_directories(${PROJECT_BINARY_DIR}/tests/genarray)
link_directories(${PROJECT_BINARY_DIR}/tests/genarray)

add_executable(genarray genarray.F90 gwrite_genarray.fh)
target_link_libraries(genarray adiosf ${ADIOSLIB_LDADD})
set_target_properties(genarray PROPERTIES COMPILE_FLAGS "${ADIOSLIB_CPPFLAGS} ${ADIOSLIB_CFLAGS}")

add_executable(genarray2D genarray2D.F90)
target_link_libraries(genarray2D adiosf ${ADIOSLIB_LDADD})
set_target_properties(genarray2D PROPERTIES COMPILE_FLAGS "${ADIOSLIB_CPPFLAGS} ${ADIOSLIB_CFLAGS}")

add_executable(copyarray copyarray.F90 gread_genarray.fh)
target_link_libraries(copyarray adiosf ${ADIOSLIB_LDADD})
set_target_properties(copyarray PROPERTIES COMPILE_FLAGS "${ADIOSLIB_CPPFLAGS} ${ADIOSLIB_CFLAGS}")

add_executable(copyarray2D copyarray2D.F90)
target_link_libraries(copyarray2D adiosf ${ADIOSLIB_LDADD})
set_target_properties(copyarray2D PROPERTIES COMPILE_FLAGS "${ADIOSLIB_CPPFLAGS} ${ADIOSLIB_CFLAGS}")

add_custom_command(
	OUTPUT gwrite_genarray.fh
	COMMAND rm -f gwrite_genarray.fh gread_genarray.fh 
	COMMAND ${PROJECT_SOURCE_DIR}/utils/gpp/gpp.py ${PROJECT_SOURCE_DIR}/tests/genarray/genarray3d.xml
	DEPENDS genarray3d.xml
	)

add_custom_command(
        OUTPUT gread_genarray.fh 
	COMMAND rm -f gwrite_genarray.fh gread_genarray.fh 
	COMMAND ${PROJECT_SOURCE_DIR}/utils/gpp/gpp.py ${PROJECT_SOURCE_DIR}/tests/genarray/genarray3d.xml
	DEPENDS genarray3d.xml
	)

file(COPY genarray.xml genarray3d.xml job.ewok job2D.ewok DESTINATION ${PROJECT_BINARY_DIR}/tests/genarray) 
