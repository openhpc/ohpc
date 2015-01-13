#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=15.1.10.380555
release=advisor_xe_2015_update1
relocate_ver=advisor_xe_20$version

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
        
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=/opt/fsp/pub/advisor/$version $rpm
    done
fi

tar cfz intel-advisor-fsp-$version.tar.gz /opt/fsp/pub/advisor/$version


