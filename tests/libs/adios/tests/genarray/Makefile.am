AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_builddir)/src

AUTOMAKE_OPTIONS = no-dependencies

noinst_PROGRAMS=genarray genarray2D copyarray copyarray2D

genarray_SOURCES = genarray.F90
genarray_CPPFLAGS = $(AM_CPPFLAGS) $(ADIOSLIB_CPPFLAGS)
genarray_CFLAGS = $(ADIOSLIB_CFLAGS)
genarray_LDADD = $(top_builddir)/src/libadiosf.a $(ADIOSLIB_LDADD)
genarray_LDFLAGS = $(ADIOSLIB_LDFLAGS)

genarray2D_SOURCES = genarray2D.F90
genarray2D_CPPFLAGS = $(AM_CPPFLAGS) $(ADIOSLIB_CPPFLAGS)
genarray2D_CFLAGS = $(ADIOSLIB_CFLAGS)
genarray2D_LDADD = $(top_builddir)/src/libadiosf.a $(ADIOSLIB_LDADD)
genarray2D_LDFLAGS = $(ADIOSLIB_LDFLAGS)

copyarray_SOURCES = copyarray.F90
copyarray_CPPFLAGS = $(AM_CPPFLAGS) $(ADIOSLIB_CPPFLAGS)
copyarray_CFLAGS = $(ADIOSLIB_CFLAGS)
copyarray_LDADD = $(top_builddir)/src/libadiosf.a $(ADIOSLIB_LDADD)
copyarray_LDFLAGS = $(ADIOSLIB_LDFLAGS)

copyarray2D_SOURCES = copyarray2D.F90
copyarray2D_CPPFLAGS = $(AM_CPPFLAGS) $(ADIOSLIB_CPPFLAGS)
copyarray2D_CFLAGS = $(ADIOSLIB_CFLAGS)
copyarray2D_LDADD = $(top_builddir)/src/libadiosf.a $(ADIOSLIB_LDADD)
copyarray2D_LDFLAGS = $(ADIOSLIB_LDFLAGS)

FC=$(MPIFC)

genarray.F90: gwrite_genarray.fh
copyarray.F90: gwrite_genarray.fh gread_genarray.fh

gwrite_genarray.fh: genarray3d.xml
	rm -f gwrite_genarray.fh gread_genarray.fh
	$(ADIOS_BIN)/gpp.py $(srcdir)/genarray3d.xml
gread_genarray.fh: genarray3d.xml
	rm -f gwrite_genarray.fh gread_genarray.fh
	$(ADIOS_BIN)/gpp.py $(srcdir)/genarray3d.xml

FCLINK = \
	$(LIBTOOL) --mode=link --tag F77 $(FC) \
	$(AM_FCFLAGS) $(FCFLAGS) $(LDFLAGS) -o $@

all-local:
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/genarray3d.xml $(srcdir)/genarray.xml $(builddir)

CLEANFILES = genarray_comm.mod genarray2d_comm.mod gwrite_genarray.fh gread_genarray.fh

EXTRA_DIST = genarray.xml genarray3d.xml job.ewok job2D.ewok
