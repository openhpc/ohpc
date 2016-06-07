
AM_CPPFLAGS = '-D H5_USE_18_API'

TESTS              = extend_ds
check_PROGRAMS     = extend_ds
extend_ds_SOURCES  = extend_ds.cpp

TESTS             += chunks
check_PROGRAMS    += chunks
chunks_SOURCES     = chunks.cpp

TESTS             += create
check_PROGRAMS    += create
create_SOURCES     = create.cpp

TESTS             += h5group
check_PROGRAMS    += h5group
h5group_SOURCES    = h5group.cpp

TESTS             += writedata
check_PROGRAMS    += writedata
writedata_SOURCES  = writedata.cpp

TESTS             += readdata
check_PROGRAMS    += readdata
readdata_SOURCES  = readdata.cpp

CLEANFILES        = *.h5
