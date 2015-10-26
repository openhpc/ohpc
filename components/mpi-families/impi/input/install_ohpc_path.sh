#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub
modscanner=../../../OHPC_mod_generator.sh

version=5.1.1.109
input_dir=l_mpi_p_5.1.1.109

INSTALL=1
TARBALL=0
POST_UNINSTALL=1
DEVEL=0

installed_RPMS=""

match_keys='intel-mpi'
skip_keys='i486.rpm$'
runtime_keys='intel-mpi-rt|intel-mpirt|pset'

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

    echo "--> installing $rpm...."
    if [ $INSTALL -eq 1 ];then
        rpm -ivh --nodeps --relocate /opt/intel/=${pubdir}/compiler/intel $rpm
        installed_RPMS="$name $installed_RPMS"
    fi

done

input=`find ${pubdir}/compiler/intel/ -name mpivars.sh`
$modscanner $input > modfile-$delim.input

if [ $TARBALL -eq 1 ];then
    if [ $DEVEL -eq 1 ];then
        echo "Building devel tarball...."
        tar cfz intel-impi-devel-${delim}-$version.tar.gz ${OHPC_PUB}/compiler/intel
    else
        echo "Building runtime tarball...."
        tar cfz intel-impi-${delim}-$version.tar.gz ${OHPC_PUB}/pub/compiler/intel
    fi
fi


if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi
