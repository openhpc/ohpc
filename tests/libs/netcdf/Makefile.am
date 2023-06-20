AUTOMAKE_OPTIONS = foreign no-dependencies
AM_MAKEFLAGS     = --no-print-directory
if PARALLEL_ENABLED
PARALLEL_SUBDIR  = tests-parallel
endif
SUBDIRS          = tests $(PARALLEL_SUBDIR)
