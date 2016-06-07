#! /bin/sh
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

case $CC in
*/*)    H5DUMP=`echo $CC | sed -e 's/\/[^/]*$/\/h5dump/'`;
        test -x $H5DUMP || H5DUMP=h5dump;;
*)      H5DUMP=h5dump;;
esac

exout() {
    $*
}

dumpout() {
    $H5DUMP $*
}

./h5ex_g_create
dumpout h5ex_g_create.h5 >testfiles/h5ex_g_create.ddl
rm -f h5ex_g_create.h5

exout ./h5ex_g_iterate >testfiles/h5ex_g_iterate.tst

exout ./h5ex_g_traverse >testfiles/h5ex_g_traverse.tst

exout ./h5ex_g_visit >testfiles/h5ex_g_visit.tst

exout ./h5ex_g_compact >testfiles/h5ex_g_compact.tst
dumpout h5ex_g_compact1.h5 >testfiles/h5ex_g_compact1.ddl
dumpout h5ex_g_compact2.h5 >testfiles/h5ex_g_compact2.ddl
rm -f h5ex_g_compact1.h5
rm -f h5ex_g_compact2.h5

exout ./h5ex_g_phase >testfiles/h5ex_g_phase.tst
rm -f h5ex_g_phase.h5

exout ./h5ex_g_corder >testfiles/h5ex_g_corder.tst
rm -f h5ex_g_corder.h5

exout ./h5ex_g_intermediate >testfiles/h5ex_g_intermediate.tst
rm -f h5ex_g_intermediate.h5
