#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=3.0.0
#release=inspector_xe_2015_update1
#relocate_ver=inspector_xe_20$version
release_dir=l_clck_p_2.2.1.003

input_dir=../../../compiler-families/intel-compilers/input/update2/parallel_studio_xe_2016_beta
input_dir=l_clck_p_3.0.0.026

match_keys='clck_'
INSTALL=1
TARBALL=1
POST_UNINSTALL=1

installed_RPMS=""

# Note, CLCK now has an order dependency - common must be installed
# first or other %post install scriptlets will fail with relocation

#for rpm in `ls $release_dir/rpm/*.rpm`; do 

    if [ $INSTALL -eq 1 ];then
        echo "installing $match_keys*.rpm $rpm(s)..."
        rpm -ivh --nodeps --relocate /opt/intel/clck/$version=/opt/fsp/admin/clck/$version $input_dir/rpm/$match_keys*.rpm

        for rpm in `ls $input_dir/rpm/$match_keys*.rpm`; do
            name=`basename $rpm`
            installed_RPMS="$name $installed_RPMS"
        done
    fi

#done


if [ $TARBALL -eq 1 ];then
    tar cfz intel-clck-fsp-$version.tar.gz /opt/fsp/admin/clck/$version
fi

if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi


