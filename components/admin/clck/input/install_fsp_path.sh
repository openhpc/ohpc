#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=2.2.1
#release=inspector_xe_2015_update1
#relocate_ver=inspector_xe_20$version
release_dir=l_clck_p_2.2.1.003

skip_arch=i486.rpm
INSTALL=1
TAR=1


for rpm in `ls $release_dir/rpm/*.rpm`; do 
    echo $rpm | grep -q "$skip_arch$" 
    if [ $? -eq 0 ];then
        echo "    --> ** skipping $rpm for consideration"
        continue
    fi
    
    if [ $INSTALL -eq 1 ];then
        echo "installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/clck/$version=/opt/fsp/admin/clck/$version $rpm
    else
        echo "--> would install $rpm...."
    fi
done


if [ $TAR -eq 1 ];then
    tar cfz intel-clck-fsp-$version.tar.gz /opt/fsp/admin/clck/$version
fi


