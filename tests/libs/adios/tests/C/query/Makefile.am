SUBDIRS = common

if HAVE_ALACRITY
SUBDIRS += alacrity
endif

if HAVE_FASTBIT
SUBDIRS += fastbit
endif

#if HAVE_ALACRITY && HAVE_FASTBIT
#SUBDIRS=alacrity fastbit
#endif

all-local:
	test "$(srcdir)" = "$(builddir)" || cp -r $(srcdir)/query-xmls $(builddir)

EXTRA_DIST = query-xmls

