AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_srcdir)/src -I$(top_srcdir)/src/public -I$(srcdir)/../include

AUTOMAKE_OPTIONS = no-dependencies

noinst_PROGRAMS = writer_maya reader_maya

writer_maya_SOURCES = writer.c ../common/utils.c
writer_maya_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
writer_maya_LDFLAGS = $(ADIOSLIB_LDFLAGS)
reader_maya_SOURCES = reader.c ../common/utils.c
reader_maya_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
reader_maya_LDFLAGS = $(ADIOSLIB_LDFLAGS)

CLEANFILES = *.bp
CC=$(MPICC)

EXTRA_DIST = cfg.h readme.txt

