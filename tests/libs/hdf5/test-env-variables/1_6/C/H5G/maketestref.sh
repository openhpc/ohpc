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
    echo '*******************************'
    echo '*  Output of example program  *'
    echo '*******************************'
    echo
    $*
}

dumpout() {
    echo '**********************'
    echo '*  Output of h5dump  *'
    echo '**********************'
    echo
    $H5DUMP $*
}

dumpout2() {
    echo
    echo
    echo '**********************'
    echo '*  Output of h5dump  *'
    echo '**********************'
    echo
    $H5DUMP $*
}

./h5ex_g_create
dumpout  h5ex_g_create.h5 >h5ex_g_create.test
rm -f h5ex_g_create.h5

exout ./h5ex_g_iterate >h5ex_g_iterate.test

exout ./h5ex_g_traverse >h5ex_g_traverse.test
