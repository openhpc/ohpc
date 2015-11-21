AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_srcdir)/src -I$(top_srcdir)/src/public -I$(srcdir)/../include

AUTOMAKE_OPTIONS = no-dependencies

all-local:
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/*.xml $(builddir)
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/readme.txt $(builddir)

noinst_PROGRAMS = writer_scalar reader_scalar

writer_scalar_SOURCES = writer.c ../common/utils.c
writer_scalar_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
writer_scalar_LDFLAGS = $(ADIOSLIB_LDFLAGS)
reader_scalar_SOURCES = reader.c ../common/utils.c
reader_scalar_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
reader_scalar_LDFLAGS = $(ADIOSLIB_LDFLAGS)

CLEANFILES = *.bp
CC=$(MPICC)
EXTRA_DIST = test_config_flex.xml test_config_mpi.xml cfg.h readme.txt
