#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=2015.2.164
release=composer_xe_$version

skip_arch=i486.rpm
#skip_keys='i486.rpm$|/intel-ipp|/intel-mkl-pgi'
skip_keys='i486.rpm$'
INSTALL=1
TAR=1
UNINSTALL=0

for rpm in `ls l_compxe_$version/rpm/*.rpm` ; do 
    echo "$rpm found"
    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
        echo "  --> skipping potential install of $rpm"
        continue
    fi
    if [ $INSTALL -eq 1 ];then
        echo "--> installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/$release=/opt/fsp/pub/compiler/intel/$release $rpm
    fi

    if [ $UNINSTALL -eq 1 ];then
	localrpm=`basename --suffix=.rpm $rpm`
        echo "--> uninstalling $localrpm ...."
        rpm -e --nodeps $localrpm
    fi
done

if [ $TAR -eq 1 ];then
    echo "creating tarball..."
    tar cfz intel-compilers-fsp-$version.tar.gz /opt/fsp/pub/compiler/intel/$release
fi


