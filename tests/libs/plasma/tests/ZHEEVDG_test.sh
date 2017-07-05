#!/bin/bash

nbthreads=2
a=2

#make -C ../ testplasma >/dev/null

if [ "$a" -le "$#" ]
then

    rm -rf res
    touch res

    mode=0
    end_mode=6
    if [ "$2" -le "$1" ]
    then
        echo "Invalid parameters"

    else
        while [ $mode -le $end_mode ]
        do
            echo ""
            echo "TESTING MODE "$mode""
            echo ""

            i=$1
            end=$2

            while [ $i -le $end ]
            do
                echo -n "testing for entry "$i" mode "$mode""

                echo -n " simple "
                ./stesting $nbthreads 1 HEEVD $mode $i $i 2>&1 > out
                cat out | awk '/FAILED/ {print "Accuracy s entry '$i' mode '$mode'"}' >> res
                cat out | awk '/suspicious/ {print $0}' >> res
                cat out | awk '/illegal/ {print $0 " s entry '$i' mode '$mode'"}' >> res
                cat out | awk '/ERROR/ {print $0 " s entry '$i' mode '$mode'"}' >> res
                cat out | awk '/TESTING/ {print $0 " s entry '$i' mode '$mode'"}' >> res

                echo -n "double"
                ./dtesting $nbthreads 1 HEEVD $mode $i $i 2>&1 > out
                cat out | awk '/FAILED/ {print "Accuracy d entry '$i' mode '$mode'"}' >> res
                cat out | awk '/suspicious/ {print $0}' >> res
                cat out | awk '/illegal/ {print $0 " d entry '$i' mode '$mode'"}' >> res
                cat out | awk '/ERROR/ {print $0 " d entry '$i' mode '$mode'"}' >> res
                cat out | awk '/TESTING/ {print $0 " d entry '$i' mode '$mode'"}' >> res

                echo -n " simple_complex"
                ./ctesting $nbthreads 1 HEEVD $mode $i $i 2>&1 > out
                cat out | awk '/FAILED/ {print "Accuracy c entry '$i' mode '$mode'"}' >> res
                cat out | awk '/suspicious/ {print $0}' >> res
                cat out | awk '/illegal/ {print $0 " c entry '$i' mode '$mode'"}' >> res
                cat out | awk '/ERROR/ {print $0 " c entry '$i' mode '$mode'"}' >> res
                cat out | awk '/TESTING/ {print $0 " c entry '$i' mode '$mode'"}' >> res

                echo " double_complex"
                ./ztesting $nbthreads 1 HEEVD $mode $i $i 2>&1 > out
                cat out | awk '/FAILED/ {print "Accuracy z entry '$i' mode '$mode'"}' >> res
                cat out | awk '/suspicious/ {print $0}' >> res
                cat out | awk '/illegal/ {print $0 " z entry '$i' mode '$mode'"}' >> res
                cat out | awk '/ERROR/ {print $0 " z entry '$i' mode '$mode'"}' >> res
                cat out | awk '/TESTING/ {print $0 " z entry '$i' mode '$mode'"}' >> res

                i=`expr $i + 134`
            done

            mode=`expr $mode + 1`
        done

        echo ""
        echo "RESULTS"
        echo ""
        cat res
    fi

else
    echo "USAGE: ./ZHEEVD_test.sh begin_size end_size"
fi
