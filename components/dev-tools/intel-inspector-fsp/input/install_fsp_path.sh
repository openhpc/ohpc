#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=15.1.2.379161
release=inspector_xe_2015_update1
relocate_ver=inspector_xe_20$version

skip_arch=i486.rpm
INSTALL=0

if [ $INSTALL -eq 1 ];then
    for rpm in `ls $release/rpm/*.rpm`; do 
        echo $rpm | grep -q "$skip_arch$" 
        if [ $? -eq 0 ];then
            echo "  [ ** skipping install of $rpm **]"
            continue
        fi

        echo "installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=/opt/fsp/pub/inspector/$version $rpm
    done
fi

tar cfz intel-inspector-fsp-$version.tar.gz /opt/fsp/pub/inspector/$version


