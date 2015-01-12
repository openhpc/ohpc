#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=15.1.1.380310
release=vtune_amplifier_xe_2015_update1
relocate_ver=vtune_amplifier_xe_20$version

skip_arch=i486.rpm
INSTALL=0

if [ $INSTALL -eq 1 ];then
    for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm`; do 
        echo $rpm | grep -q "$skip_arch$" 
        if [ $? -eq 0 ];then
            echo "  [ ** skipping install of $rpm **]"
            continue
        fi
        echo "installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=/opt/fsp/pub/vtune_amplifier/$version $rpm
    done
fi

tar cfz intel-vtune-amplifier-fsp-$version.tar.gz /opt/fsp/pub/vtune_amplifier/$version


