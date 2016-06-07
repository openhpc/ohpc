AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_builddir)/src

AUTOMAKE_OPTIONS = no-dependencies

all-local:
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/*.xml $(builddir)

noinst_PROGRAMS=adios_test_f

#adios_test_f_SOURCES = adios_test_f.F90 adios_module.F90
adios_test_f_SOURCES = adios_test_f.F90
adios_test_f_CPPFLAGS = $(AM_CPPFLAGS) $(ADIOSLIB_CPPFLAGS)
adios_test_f_CFLAGS = $(ADIOSLIB_CFLAGS)
adios_test_f_LDFLAGS = $(ADIOSLIB_LDFLAGS)
adios_test_f_LDADD = $(top_builddir)/src/libadiosf.a $(ADIOSLIB_LDADD)

FC=$(MPIFC)

FCLINK = \
	$(LIBTOOL) --mode=link --tag F77 $(FC) \
	$(AM_FCFLAGS) $(FCFLAGS) $(LDFLAGS) -o $@

EXTRA_DIST = config_fortran.xml
