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


case $FC in
*/*)    H5DUMP=`echo $FC | sed -e 's/\/[^/]*$/\/h5dump/'`;
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

topics="vlstring"

if ($FC -showconfig 2>&1 | grep 'Fortran 2003 Compiler: yes') > /dev/null; then
  topics="$topics arrayatt_F03 array_F03 bitatt_F03 bit_F03 \
  cmpdatt_F03 cmpd_F03  enumatt_F03 \
  enum_F03 floatatt_F03 float_F03 intatt_F03 int_F03 \
  objrefatt_F03 objref_F03 opaqueatt_F03 opaque_F03 \
  regrefatt_F03 regref_F03 stringCatt_F03 stringC_F03 \
  string_F03 vlenatt_F03 vlen_F03"
fi

return_val=0

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Testing 1_8/FORTRAN/H5T/$fname...$ECHO_C"
    exout ./$fname >tmp.test
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
done


#######Non-standard tests#######

#fname=h5ex_t_cpxcmpd
#$ECHO_N "Testing 1_8/C/H5T/$fname...$ECHO_C"
#exout ./$fname >tmp.test
#cmp -s tmp.test $srcdir/testfiles/$fname.tst
#status=$?
#if test $status -ne 0
#then
#    echo "  FAILED!"
#else
#  dumpout -n $fname.h5 >tmp.test
#  rm -f $fname.h5
#  cmp -s tmp.test $srcdir/testfiles/$fname.ddl
#  status=$?
#  if test $status -ne 0
#  then
#      echo "  FAILED!"
#  else
#      echo "  Passed"
#  fi
#fi
#return_val=`expr $status + $return_val`


#fname=h5ex_t_cpxcmpdatt
#$ECHO_N "Testing 1_8/C/H5T/$fname...$ECHO_C"
#exout ./$fname >tmp.test
#cmp -s tmp.test $srcdir/testfiles/$fname.tst
#status=$?
#if test $status -ne 0
#then
#    echo "  FAILED!"
#else
#  dumpout -n $fname.h5 >tmp.test
#  rm -f $fname.h5
#  cmp -s tmp.test $srcdir/testfiles/$fname.ddl
#  status=$?
#  if test $status -ne 0
#  then
#      echo "  FAILED!"
#  else
#      echo "  Passed"
#  fi
#fi
#return_val=`expr $status + $return_val`


#fname=h5ex_t_convert
#$ECHO_N "Testing 1_8/C/H5T/$fname...$ECHO_C"
#exout ./$fname >tmp.test
#cmp -s tmp.test $srcdir/$fname.test
#status=$?
#if test $status -ne 0
#then
#    echo "  FAILED!"
#else
#    echo "  Passed"
#fi
#return_val=`expr $status + $return_val`


rm -f tmp.test
echo "$return_val tests failed in 1_8/FORTRAN/H5T/"
exit $return_val
