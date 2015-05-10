#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=16.0.0-042
#release=composer_xe_$version
release_dir=parallel_studio_xe_2016_beta

skip_arch=i486.rpm
#skip_keys='i486.rpm$|/intel-ipp|/intel-mkl-pgi'
skip_keys='i486.rpm$'
INSTALL=0
TAR=0
UNINSTALL=1

for rpm in `ls $release_dir/rpm/*.rpm` ; do 
    echo "$rpm found"
    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
        echo "  --> skipping potential install of $rpm"
        continue
    fi
    if [ $INSTALL -eq 1 ];then
        echo "--> installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel=/opt/fsp/pub/compiler/intel $rpm
#        rpm -ivh --nodeps $rpm
    fi

    if [ $UNINSTALL -eq 1 ];then
	localrpm=`basename --suffix=.rpm $rpm`
        echo "--> uninstalling $localrpm ...."
        rpm -e --nodeps $localrpm
    fi
done

if [ $TAR -eq 1 ];then
    echo "creating tarball..."
    tar cfz intel-compilers-fsp-$version.tar.gz /opt/fsp/pub/compiler/intel
fi


