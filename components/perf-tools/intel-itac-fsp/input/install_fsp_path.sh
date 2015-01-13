#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=9.0.2.045

INSTALL=1

if [ $INSTALL -eq 1 ];then
    for rpm in `ls l_itac_p_$version/rpm/*.rpm`; do 
        echo "installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/itac/$version=/opt/fsp/pub/itac/$version $rpm
    done
fi

tar cfz intel-itac-fsp-$version.tar.gz /opt/fsp/pub/itac/$version


