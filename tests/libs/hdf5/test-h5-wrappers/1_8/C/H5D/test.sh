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
    $*
}

dumpout() {
    $H5DUMP $*
}


topics="rdwr hyper chunk gzip szip nbit soint sofloat extern compact unlimadd \
unlimmod unlimgzip checksum shuffle fillval alloc"


return_val=0

if [ -z $srcdir ];then
    srcdir="./"
fi

for topic in $topics
do
    fname=h5ex_d_$topic
    $ECHO_N "Testing 1_8/C/H5D/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    status=$?
    if test $status -eq 1
    then
        echo "  Unsupported feature"
    else
        cmp -s tmp.test $srcdir/testfiles/$fname.tst
        status=$?
        if test $status -ne 0
        then
            echo "  FAILED!"
        else
          dumpout $fname.h5 >tmp.test
          rm -f $fname.h5
          cmp -s tmp.test $srcdir/testfiles/$fname.ddl
          status=$?
          if test $status -ne 0
          then
              echo "  FAILED!"
          else
              echo "  Passed"
          fi
        fi
        return_val=`expr $status + $return_val`
    fi
done


#######Non-standard tests#######


#Remove external data file from h5ex_d_extern
rm -f h5ex_d_extern.data


fname=h5ex_d_transform
$ECHO_N "Testing 1_8/C/H5D/$fname...$ECHO_C"
exout ./$fname >tmp.test
status=$?
if test $status -eq 1
then
    echo "  Unsupported feature"
else
    cmp -s tmp.test $srcdir/testfiles/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        dumpout -n $fname.h5 >tmp.test
        rm -f $fname.h5
        cmp -s tmp.test $srcdir/testfiles/$fname.ddl
        status=$?
        if test $status -ne 0
        then
            echo "  FAILED!"
        else
            echo "  Passed"
        fi
        return_val=`expr $status + $return_val`
    fi
    return_val=`expr $status + $return_val`
fi


rm -f tmp.test
echo "$return_val tests failed in 1_8/C/H5D/"
exit $return_val
