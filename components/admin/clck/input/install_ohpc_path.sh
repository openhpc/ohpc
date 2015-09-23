#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
admindir=/opt/${delim}/admin

version=3.0.1
release_dir=l_clck_p_3.0.1.030

input_dir=l_clck_p_3.0.1.030

match_keys='clck_'    # 3.x
INSTALL=1
TARBALL=1
POST_UNINSTALL=1

installed_RPMS=""

# Note, CLCK now has an order dependency - common must be installed
# first or other %post install scriptlets will fail with relocation

#for rpm in `ls $release_dir/rpm/*.rpm`; do 

    if [ $INSTALL -eq 1 ];then
        echo "installing $match_keys*.rpm $rpm(s)..."
        rpm -ivh --nodeps --relocate /opt/intel/clck/$version=${admindir}/clck/$version $input_dir/rpm/$match_keys*.rpm

        for rpm in `ls $input_dir/rpm/$match_keys*.rpm`; do
            name=`basename $rpm`
            installed_RPMS="$name $installed_RPMS"
        done
    fi

#done


if [ $TARBALL -eq 1 ];then
    tar cfz intel-clck-${delim}-$version.tar.gz ${admindir}/clck/$version
fi

if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi


