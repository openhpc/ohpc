INCLUDES = $(all_includes)
#
# ${ADIOSREADLIB_CPPFLAGS} has all the flags like -DFASTBIT -DALACRITY
# as well as related includes, so it is needed here
# 
INCLUDES += -I$(top_srcdir)/src/ -I$(top_srcdir)/src/public/ $(MXML_CPPFLAGS)

AUTOMAKE_OPTIONS = no-dependencies

noinst_PROGRAMS = 

# Only compile these test programs if we have FastBit, since they rely on FastBit
if HAVE_FASTBIT
CC=$(MPICC)

noinst_PROGRAMS += fastbit_tests

fastbit_tests_SOURCES = fastbit_tests.c
fastbit_tests_CPPFLAGS = ${MACRODEFFLAG}_NOMPI 
fastbit_tests_LDADD = $(top_builddir)/src/libadiosread_nompi.a $(top_builddir)/src/libadios_nompi.a $(ADIOSREADLIB_LDADD) ${MXML_LIBS}
fastbit_tests_LDFLAGS = $(ADIOSREADLIB_LDFLAGS) ${MXML_LDFLAGS}

endif # HAVE_FASTBIT
CC=$(MPICC)