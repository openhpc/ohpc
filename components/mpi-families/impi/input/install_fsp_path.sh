#!/bin/bash

# Install from release rpms into standard OpenHPC path

version=5.1.1.109
input_dir=l_mpi_p_5.1.1.109

INSTALL=1
TARBALL=1
POST_UNINSTALL=1
DEVEL=1

installed_RPMS=""

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

    echo "--> installing $rpm...."
    if [ $INSTALL -eq 1 ];then
        rpm -ivh --nodeps --relocate /opt/intel/=/opt/fsp/pub/compiler/intel $rpm
        installed_RPMS="$name $installed_RPMS"
    fi

###     if [ $UNINSTALL -eq 1 ];then
### 	localrpm=`basename --suffix=.rpm $rpm`
###         echo "--> uninstalling $localrpm ...."
###         rpm -e --nodeps $localrpm
###     fi
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


if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi
