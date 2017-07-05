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

rm -f res out
xlintsts  2>&1 >> out
xlintstd  2>&1 >> out
xlintstc  2>&1 >> out
xlintstz  2>&1 >> out
echo " "
grep -e "ERROR" -e "FAILED"  -e "suspicious" -e "illegal" out
if [ $? -ne 1 ]; then
    exit 1
fi
exit 0

