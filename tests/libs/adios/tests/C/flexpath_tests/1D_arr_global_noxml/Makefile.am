AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_srcdir)/src -I$(top_srcdir)/src/public -I$(srcdir)/../include

AUTOMAKE_OPTIONS = no-dependencies

all-local:
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/readme.txt $(builddir)

noinst_PROGRAMS = writer_global_noxml reader_global_noxml

writer_global_noxml_SOURCES = writer.c ../common/utils.c
writer_global_noxml_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
writer_global_noxml_LDFLAGS = $(ADIOSLIB_LDFLAGS)
reader_global_noxml_SOURCES = reader.c ../common/utils.c
reader_global_noxml_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
reader_global_noxml_LDFLAGS = $(ADIOSLIB_LDFLAGS)

CLEANFILES = *.bp
CC=$(MPICC)

EXTRA_DIST = cfg.h readme.txt

