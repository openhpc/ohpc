#!/bin/sh

PLASMA_DEFAULT_BEGIN_SIZE=1
PLASMA_DEFAULT_END_SIZE=10

if [ "x${PLASMA_BEGIN_SIZE}" != "x" ]; then
    BEGIN_SIZE=${PLASMA_BEGIN_SIZE}
else
    BEGIN_SIZE=${PLASMA_DEFAULT_BEGIN_SIZE}
fi

if [ "x${PLASMA_END_SIZE}" != "x" ]; then
    END_SIZE=${PLASMA_END_SIZE}
else
    END_SIZE=${PLASMA_DEFAULT_END_SIZE}
fi

echo " "
echo " "
echo "-----------------------------------------------------------------"
echo "PLASMA tests $compiler begin = ${BEGIN_SIZE} end = ${END_SIZE}"
echo "-----------------------------------------------------------------"

./ZHEEVDG_test.sh ${BEGIN_SIZE} ${END_SIZE}
echo " "
grep -e "ERROR" -e "FAILED"  -e "suspicious" -e "illegal" res
if [ $? -ne 1 ]; then
    exit 1
fi
exit 0

