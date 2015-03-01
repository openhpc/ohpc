#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=2015.2.164
release=composer_xe_$version

skip_arch=i486.rpm
INSTALL=0
TAR=1

for rpm in `ls l_compxe_$version/rpm/*.rpm` ; do 
    echo "$rpm found"
    echo $rpm | grep -q "$skip_arch$" 
    if [ $? -eq 0 ];then
        echo "  --> skipping potential install of $rpm **"
        continue
    fi
    if [ $INSTALL -eq 1 ];then
        echo "--> installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/$release=/opt/fsp/pub/compiler/intel/$release $rpm
    fi
done

if [ $TAR -eq 1 ];then
    echo "creating tarball..."
    tar cfz intel-compilers-fsp-$version.tar.gz /opt/fsp/pub/compiler/intel/$release
fi


