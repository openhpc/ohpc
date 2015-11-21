# Add your codes that you want to build at 'make check'
# Do not build Fortran codes if Fortran is not supported.
# Do not build a code using the write API if only the read API is available
# Steps:
# 1. add code name below to check_readonly_C/check_C/check_readonly_Fortran/check_Fortran targets
# 2. add code name to TESTS if it is a sequential test 
# 3. define build rules for your code
# 4. add files to CLEANFILES that should be deleted at 'make clean'
# 5. add to EXTRA_DIST any non-source files that should go with the distribution
check_readonly_C=
if BUILD_WRITE
check_C=local_array_time \
        adios_amr_write \
        adios_amr_write_2vars \
        adios_staged_read \
        adios_staged_read_v2 \
        adios_staged_read_2vars \
        write_read \
        write_alternate \
        two_groups \
        many_vars \
        selections \
        path_test \
        reuse_dim \
        adios_transforms_read_write \
        connect_to_space_subset \
	big_file \
	set_path \
	set_path_var \
	steps_write \
	steps_read_file \
	steps_read_stream \
	blocks \
	build_standard_dataset \
	transforms_writeblock_read

test_C=copy_subvolume transforms_specparse group_free_test

endif

if BUILD_FORTRAN
check_readonly_Fortran=
if BUILD_WRITE
check_Fortran=posix_method
endif
endif

check_PROGRAMS=$(check_C) $(check_Fortran) $(check_readonly_C) $(check_readonly_Fortran) $(test_C)

# Sequential tests to be executed by "make check" can be defined right here in TESTS
# Parallel tests should be defined in ../tests/*.sh and executed by ../test.sh
TESTS=$(test_C)

AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_builddir)/src -I$(top_srcdir)/src/public

all-local:
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/*.xml $(builddir)

#############################
# Build rules for each test #
#############################

# posix_method
posix_method_SOURCES=posix_method.F90
posix_method_LDADD = $(top_builddir)/src/libadiosf.a $(ADIOSLIB_LDADD)
posix_method_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
posix_method.o: gwrite_posix_method.fh 
gwrite_posix_method.fh: posix_method.xml
	$(ADIOS_BIN)/gpp.py $(srcdir)/posix_method.xml

local_array_time_SOURCES=local_array_time.c
local_array_time_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
local_array_time_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)

adios_amr_write_SOURCES=adios_amr_write.c
adios_amr_write_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
adios_amr_write_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
adios_amr_write.o: adios_amr_write.c

adios_amr_write_2vars_SOURCES=adios_amr_write_2vars.c
adios_amr_write_2vars_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
adios_amr_write_2vars_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
adios_amr_write_2vars.o: adios_amr_write_2vars.c

adios_staged_read_SOURCES=adios_staged_read.c
adios_staged_read_LDADD = $(top_builddir)/src/libadiosread.a $(ADIOSREADLIB_LDADD)
adios_staged_read_LDFLAGS = $(AM_LDFLAGS) $(ADIOSREADLIB_LDFLAGS)
adios_staged_read.o: adios_staged_read.c

adios_staged_read_v2_SOURCES=adios_staged_read_v2.c
adios_staged_read_v2_LDADD = $(top_builddir)/src/libadiosread.a $(ADIOSREADLIB_LDADD)
adios_staged_read_v2_LDFLAGS = $(AM_LDFLAGS) $(ADIOSREADLIB_LDFLAGS)
adios_staged_read_v2.o: adios_staged_read_v2.c

adios_staged_read_2vars_SOURCES=adios_staged_read_2vars.c
adios_staged_read_2vars_LDADD = $(top_builddir)/src/libadiosread.a $(ADIOSREADLIB_LDADD)
adios_staged_read_2vars_LDFLAGS = $(AM_LDFLAGS) $(ADIOSREADLIB_LDFLAGS)
adios_staged_read_2vars.o: adios_staged_read_2vars.c

adios_transforms_read_write_SOURCES=adios_transforms_read_write.c
adios_transforms_read_write_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
adios_transforms_read_write_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
adios_transforms_read_write.o: adios_transforms_read_write.c 

write_read_SOURCES=write_read.c
write_read_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
write_read_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
write_read.o: write_read.c

write_alternate_SOURCES=write_alternate.c
write_alternate_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
write_alternate_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
write_alternate.o: write_alternate.c

set_path_SOURCES=set_path.c
set_path_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
set_path_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
set_path.o: set_path.c

set_path_var_SOURCES=set_path_var.c
set_path_var_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
set_path_var_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
set_path_var.o: set_path_var.c

two_groups_SOURCES=two_groups.c
two_groups_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
two_groups_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
two_groups.o: two_groups.c

many_vars_SOURCES=many_vars.c
many_vars_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
many_vars_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
many_vars.o: many_vars.c

selections_SOURCES=selections.c
selections_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
selections_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
selections.o: selections.c

path_test_SOURCES=path_test.c
path_test_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
path_test_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
path_test.o: path_test.c

reuse_dim_SOURCES=reuse_dim.c
reuse_dim_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
reuse_dim_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
reuse_dim.o: reuse_dim.c

connect_to_space_subset_SOURCES=connect_to_space_subset.c
connect_to_space_subset_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
connect_to_space_subset_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
connect_to_space_subset.o: connect_to_space_subset.c

big_file_SOURCES=big_file.c
big_file_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
big_file_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
big_file.o: big_file.c

steps_write_SOURCES=steps_write.c
steps_write_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
steps_write_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
steps_write.o: steps_write.c

steps_read_file_SOURCES=steps_read_file.c
steps_read_file_LDADD = $(top_builddir)/src/libadiosread.a $(ADIOSREADLIB_LDADD)
steps_read_file_LDFLAGS = $(AM_LDFLAGS) $(ADIOSREADLIB_LDFLAGS)
steps_read_file.o: steps_read_file.c

steps_read_stream_SOURCES=steps_read_stream.c
steps_read_stream_LDADD = $(top_builddir)/src/libadiosread.a $(ADIOSREADLIB_LDADD)
steps_read_stream_LDFLAGS = $(AM_LDFLAGS) $(ADIOSREADLIB_LDFLAGS)
steps_read_stream.o: steps_read_stream.c

blocks_SOURCES=blocks.c
blocks_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
blocks_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
blocks.o: blocks.c

build_standard_dataset_SOURCES = build_standard_dataset.c 
build_standard_dataset_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD) 
build_standard_dataset_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)

transforms_writeblock_read_SOURCES = transforms_writeblock_read.c 
transforms_writeblock_read_LDADD = $(top_builddir)/src/libadiosread.a $(ADIOSREADLIB_LDADD) 
transforms_writeblock_read_LDFLAGS = $(AM_LDFLAGS) $(ADIOSREADLIB_LDFLAGS)

#transforms_SOURCES=transforms.c
#transforms_CPPFLAGS = -DADIOS_USE_READ_API_1
#transforms_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
#transforms_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_LDFLAGS)
#transforms.o: transforms.c

copy_subvolume_SOURCES=copy_subvolume.c
copy_subvolume_LDADD = $(top_builddir)/src/libadiosread_nompi.a $(ADIOSREADLIB_SEQ_LDADD)
copy_subvolume_LDFLAGS = $(AM_LDFLAGS) $(ADIOSREADLIB_SEQ_LDFLAGS)
copy_subvolume_CPPFLAGS = -I$(top_srcdir)/src $(ADIOSREADLIB_SEQ_CPPFLAGS)
#copy_subvolume.o: copy_subvolume.c

transforms_specparse_SOURCES=transforms_specparse.c
transforms_specparse_LDADD = $(top_builddir)/src/libadios_nompi.a $(ADIOSLIB_SEQ_LDADD)
transforms_specparse_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_SEQ_LDFLAGS)
transforms_specparse_CPPFLAGS = -I$(top_srcdir)/src $(ADIOSLIB_SEQ_CPPFLAGS)
#transforms_specparse.o: transforms_specparse.c







group_free_test_SOURCES=group_free_test.c
group_free_test_LDADD = $(top_builddir)/src/libadios_nompi.a $(ADIOSLIB_SEQ_LDADD)
group_free_test_LDFLAGS = $(AM_LDFLAGS) $(ADIOSLIB_SEQ_LDFLAGS)
group_free_test_CPPFLAGS = -I$(top_srcdir)/src $(ADIOSLIB_SEQ_CPPFLAGS)
group_free_test.o: group_free_test.c

EXTRA_DIST = adios_amr_write.xml adios_amr_write_2vars.xml \
             posix_method.xml local_array_time.xml  \
             write_alternate.xml write_read.xml transforms.xml \
             path_test.xml adios_transforms.xml \
	     two_groups.xml set_path.xml set_path_var.xml

CLEANFILES = gwrite_posix_method.fh gread_posix_method.fh 

CC=$(MPICC)
FC=$(MPIFC)

FCLINK = \
	$(LIBTOOL) --mode=link --tag F77 $(FC) \
	$(AM_FCFLAGS) $(FCFLAGS) $(LDFLAGS) -o $@
