#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=5.1.0.056
release=vtune_amplifier_xe_2015_update2
relocate_ver=vtune_amplifier_xe_20$version

input_dir=../../../compiler-families/intel-compilers-fsp/input/update1/parallel_studio_xe_2016_beta

INSTALL=0
TARBALL=0
UNINSTALL=1
DEVEL=1

match_keys='intel-mpi'
skip_keys='i486.rpm$'
runtime_keys='intel-mpi-rt|intel-mpirt'

for rpm in `ls $input_dir/rpm/*.rpm`; do

    name=`basename $rpm`

    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
#        echo "  --> skipping potential install of $rpm"
        continue
    fi

    echo $rpm | egrep -q "$match_keys" 
    if [ $? -eq 0 ];then
        echo "detected MPI $rpm..."
    else
        continue
    fi

    echo $rpm | egrep -q $runtime_keys
    if [ $? -eq 0 ];then
        if [ $DEVEL -eq 0 ];then
            echo "  --> (runtime) $name found"
        else
            echo "  --> skipping potential install of (runtime) $name"
            continue
        fi
    else
        if [ $DEVEL -eq 0 ];then
            echo "  --> skipping potential install of (runtime) $name"
            continue
        else
            echo "  --> (development) $name found"
        fi
    fi

    if [ $INSTALL -eq 1 ];then
        echo "--> installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/=/opt/fsp/pub/compiler/intel $rpm
    fi

    if [ $UNINSTALL -eq 1 ];then
	localrpm=`basename --suffix=.rpm $rpm`
        echo "--> uninstalling $localrpm ...."
        rpm -e --nodeps $localrpm
    fi
done

if [ $TARBALL -eq 1 ];then
    if [ $DEVEL -eq 1 ];then
        echo "Building devel tarball...."
        tar cfz intel-impi-devel-fsp-$version.tar.gz /opt/fsp/pub/compiler/intel
    else
        echo "Building runtime tarball...."
        tar cfz intel-impi-fsp-$version.tar.gz /opt/fsp/pub/compiler/intel
    fi
fi


