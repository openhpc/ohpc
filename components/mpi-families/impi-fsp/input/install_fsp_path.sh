#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=5.0.3.048
release=vtune_amplifier_xe_2015_update2
relocate_ver=vtune_amplifier_xe_20$version

INSTALL=0
TARBALL=1

for rpm in `ls l_mpi_p_$version/rpm/*.rpm`; do
    echo "detected $rpm..."
    if [ $INSTALL -eq 1 ];then
        echo "--> installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/impi/$version=/opt/fsp/pub/mpi/impi/$version $rpm
    fi
done

if [ $TARBALL -eq 1 ];then
    echo "Building tarball...."
    tar cfz intel-impi-fsp-$version.tar.gz /opt/fsp/pub/mpi/impi/$version
fi


