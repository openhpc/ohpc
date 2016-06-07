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


case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ;;
  *)       ECHO_N= ECHO_C='\c' ;;
esac
ECHO_N="echo $ECHO_N"


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


return_val=0


$ECHO_N "Testing 1_6/C/H5G/h5ex_g_create...$ECHO_C"
./h5ex_g_create
dumpout h5ex_g_create.h5 >tmp.test
rm -f h5ex_g_create.h5
cmp -s tmp.test $srcdir/h5ex_g_create.test
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`


$ECHO_N "Testing 1_6/C/H5G/h5ex_g_iterate...$ECHO_C"
if test -f "h5ex_g_iterate.h5"
then
    exout ./h5ex_g_iterate >tmp.test
else
    cp $srcdir/h5ex_g_iterate.h5 h5ex_g_iterate.h5
    exout ./h5ex_g_iterate >tmp.test
    rm  -f h5ex_g_iterate.h5
fi
cmp -s tmp.test $srcdir/h5ex_g_iterate.test
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`


$ECHO_N "Testing 1_6/C/H5G/h5ex_g_traverse...$ECHO_C"
if test -f h5ex_g_traverse.h5
then
    exout ./h5ex_g_traverse >tmp.test
else
    cp $srcdir/h5ex_g_traverse.h5 h5ex_g_traverse.h5
    exout ./h5ex_g_traverse >tmp.test
    rm  -f h5ex_g_traverse.h5
fi
cmp -s tmp.test $srcdir/h5ex_g_traverse.test
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`


rm -f tmp.test
echo "$return_val tests failed in 1_6/C/H5G/"
exit $return_val
