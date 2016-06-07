AM_CPPFLAGS = $(all_includes)
AM_CPPFLAGS += -I$(top_srcdir)/src -I$(top_srcdir)/src/public

AUTOMAKE_OPTIONS = no-dependencies

noinst_PROGRAMS=posix_fgr

posix_fgr_SOURCES = posix_fgr.c
posix_fgr_CPPFLAGS = $(ADIOSLIB_CPPFLAGS)
posix_fgr_CFLAGS = $(ADIOSLIB_CFLAGS)
posix_fgr_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD)
posix_fgr_LDFLAGS = $(ADIOSLIB_LDFLAGS) 

CLEANFILES = posix_fgr.bp
CC=$(MPICC)

