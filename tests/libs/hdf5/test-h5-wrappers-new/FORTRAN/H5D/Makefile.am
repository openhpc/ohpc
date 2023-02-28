#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
noinst_PROGRAMS = \
  h5ex_d_alloc \
  h5ex_d_checksum \
  h5ex_d_chunk \
  h5ex_d_compact \
  h5ex_d_extern  \
  h5ex_d_fillval \
  h5ex_d_gzip \
  h5ex_d_hyper \
  h5ex_d_nbit \
  h5ex_d_rdwr \
  h5ex_d_soint \
  h5ex_d_szip \
  h5ex_d_transform \
  h5ex_d_unlimmod

h5ex_d_alloc_SOURCES     = h5ex_d_alloc.F90
h5ex_d_checksum_SOURCES  = h5ex_d_checksum.F90
h5ex_d_chunk_SOURCES     = h5ex_d_chunk.F90 
h5ex_d_compact_SOURCES   = h5ex_d_compact.F90
h5ex_d_extern_SOURCES    = h5ex_d_extern.F90   
h5ex_d_fillval_SOURCES   = h5ex_d_fillval.F90
h5ex_d_gzip_SOURCES      = h5ex_d_gzip.F90
h5ex_d_hyper_SOURCES     = h5ex_d_hyper.F90
h5ex_d_nbit_SOURCES      = h5ex_d_nbit.F90
h5ex_d_rdwr_SOURCES      = h5ex_d_rdwr.F90
h5ex_d_soint_SOURCES     = h5ex_d_soint.F90
h5ex_d_szip_SOURCES      = h5ex_d_szip.F90
h5ex_d_transform_SOURCES = h5ex_d_transform.F90
h5ex_d_unlimmod_SOURCES  = h5ex_d_unlimmod.F90

if FORTRAN_2003_CONDITIONAL_F
  noinst_PROGRAMS +=  h5ex_d_rdwr_kind
  h5ex_d_rdwr_kind_SOURCES = h5ex_d_rdwr_kind.F90
endif


EXTRA_DIST = testfiles/h5ex_d_rdwr.tst testfiles/h5ex_d_hyper.tst testfiles/h5ex_d_chunk.tst \
testfiles/h5ex_d_gzip.tst testfiles/h5ex_d_extern.tst testfiles/h5ex_d_compact.tst \
testfiles/h5ex_d_unlimadd.tst \
testfiles/h5ex_d_checksum.tst testfiles/h5ex_d_transform.tst \
testfiles/h5ex_d_fillval.tst testfiles/h5ex_d_alloc.tst \
testfiles/h5ex_d_rdwr.ddl testfiles/h5ex_d_hyper.ddl testfiles/h5ex_d_chunk.ddl \
testfiles/h5ex_d_gzip.ddl testfiles/h5ex_d_extern.ddl testfiles/h5ex_d_compact.ddl \
testfiles/h5ex_d_unlimadd.ddl \
testfiles/h5ex_d_checksum.ddl testfiles/h5ex_d_transform.ddl \
testfiles/h5ex_d_soint.tst testfiles/h5ex_d_soint.dll \
testfiles/h5ex_d_szip.tst testfiles/h5ex_d_szip.dll \
testfiles/h5ex_d_nbit.tst testfiles/h5ex_d_nbit.dll \
testfiles/h5ex_d_unlimmod.tst testfiles/h5ex_d_unlimmod.dll \
testfiles/h5ex_d_fillval.ddl testfiles/h5ex_d_alloc.ddl \
test.sh

if FORTRAN_2003_CONDITIONAL_F
  EXTRA_DIST += testfiles/h5ex_d_rdwr_kind.tst testfiles/h5ex_d_rdwr_kind.dll
endif

TESTS = test.sh

CLEANFILES = Makefile test.sh maketestref.sh
