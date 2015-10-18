AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_srcdir)/src -I$(top_srcdir)/src/public

SUBDIRS=flexpath_tests fgr_tests query

AUTOMAKE_OPTIONS = no-dependencies

all-local:
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/*.xml $(builddir)

noinst_PROGRAMS=adios_test_c

adios_test_c_SOURCES = adios_test_c.c
adios_test_c_CPPFLAGS = $(AM_CPPFLAGS) $(ADIOSLIB_CPPFLAGS)
adios_test_c_CFLAGS = $(ADIOSLIB_CFLAGS)
adios_test_c_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
adios_test_c_LDFLAGS = $(ADIOSLIB_LDFLAGS) 

CLEANFILES = restart.bp
CC=$(MPICC)

EXTRA_DIST = config_c.xml
