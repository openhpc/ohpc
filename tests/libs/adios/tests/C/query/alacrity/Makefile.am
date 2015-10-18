INCLUDES = $(all_includes)
INCLUDES += $(ADIOSLIB_CPPFLAGS)
INCLUDES += -I$(top_srcdir)/src -I$(top_srcdir)/src/public -I$(srcdir)/../common/

AUTOMAKE_OPTIONS = no-dependencies
if HAVE_ALACRITY

noinst_PROGRAMS = adios_read_ext_test adios_build_alac_index adios_alac_query adios_show_bp_file adios_read_block
#alac_internal_test 

#alac_internal_test_SOURCES = test_query_alac_internal.c 
#alac_internal_test_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD) 
#alac_internal_test_LDFLAGS = $(ADIOSLIB_LDFLAGS)
#alac_internal_test_CPPFLAGS = $(ALACRITY_CPPFLAGS) -I$(top_srcdir)/src/core



adios_read_block_SOURCES = adios_read_block.c
adios_read_block_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD) 
adios_read_block_LDFLAGS = $(ADIOSLIB_LDFLAGS)
adios_read_block_CPPFLAGS = $(ALACRITY_CPPFLAGS)



adios_read_ext_test_SOURCES = adios_read_ext_test.c
adios_read_ext_test_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD) 
adios_read_ext_test_LDFLAGS = $(ADIOSLIB_LDFLAGS)
adios_read_ext_test_CPPFLAGS = $(ALACRITY_CPPFLAGS)


adios_build_alac_index_SOURCES = adios_build_alac_index.c
adios_build_alac_index_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD) 
adios_build_alac_index_LDFLAGS = $(ADIOSLIB_LDFLAGS)
adios_build_alac_index_CPPFLAGS = $(ALACRITY_CPPFLAGS)

adios_alac_query_SOURCES = adios_alac_query.c
adios_alac_query_LDADD = $(top_builddir)/tests/C/query/common/libparse_test_query_xml.a $(top_builddir)/src/libadios.a $(ADIOSLIB_LDADD) 
adios_alac_query_LDFLAGS = $(ADIOSLIB_LDFLAGS)
adios_alac_query_CPPFLAGS = $(ALACRITY_CPPFLAGS)

adios_show_bp_file_SOURCES = adios_show_bp_file.c
adios_show_bp_file_LDADD = $(top_builddir)/src/libadios.a $(ADIOSLIB_LDFLAGS) 

adios_show_bp_file_LDADD += $(ADIOSLIB_LDADD)
  

endif

CLEANFILES = *.bp
CC=$(MPICC)
#EXTRA_DIST = test_config_flex.xml 
