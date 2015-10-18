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

topics="int intatt float floatatt enum enumatt bit bitatt opaque opaqueatt \
array arrayatt vlen vlenatt string stringatt vlstring vlstringatt \
cmpd cmpdatt objref objrefatt regref regrefatt commit"

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Creating test reference file for 1.8/C/H5T/$fname...$ECHO_C"
    exout ./$fname >testfiles/$fname.tst
    dumpout $fname.h5 >testfiles/$fname.ddl
    rm -f $fname.h5
    echo "  Done."
done

#######Non-standard tests#######

fname=h5ex_t_cpxcmpd
$ECHO_N "Creating test reference file for 1.8/C/H5T/$fname...$ECHO_C"
exout ./$fname >testfiles/$fname.tst
dumpout -n $fname.h5 >testfiles/$fname.ddl
rm -f $fname.h5
echo "  Done."

fname=h5ex_t_cpxcmpdatt
$ECHO_N "Creating test reference file for 1.8/C/H5T/$fname...$ECHO_C"
exout ./$fname >testfiles/$fname.tst
dumpout -n $fname.h5 >testfiles/$fname.ddl
rm -f $fname.h5
echo "  Done."

fname=h5ex_t_convert
$ECHO_N "Creating test reference file for 1.8/C/H5T/$fname...$ECHO_C"
exout ./$fname >testfiles/$fname.tst
echo "  Done."
