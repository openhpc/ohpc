#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=15.1.2.379161
release=inspector_xe_2015_update1
relocate_ver=inspector_xe_20$version

skip_arch=i486.rpm
INSTALL=1
TAR=1


for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm` ; do 
    echo $rpm | grep -q "$skip_arch$" 
    if [ $? -eq 0 ];then
        echo "    --> ** skipping $rpm for consideration"
        continue
    fi
    
    if [ $INSTALL -eq 1 ];then
        echo "installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=/opt/fsp/pub/inspector/$version $rpm
    else
        echo "--> would install $rpm...."
    fi
done


if [ $TAR -eq 1 ];then
    tar cfz intel-inspector-fsp-$version.tar.gz /opt/fsp/pub/inspector/$version
fi


