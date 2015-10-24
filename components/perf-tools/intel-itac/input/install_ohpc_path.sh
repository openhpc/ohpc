#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub

version=9.1.1.017

input_dir=l_itac_p_9.1.1.017

skip_arch=i486.rpm
INSTALL=1
POST_UNINSTALL=1
TARBALL=1

match_keys='intel-itac|intel-ta|intel-tc'
skip_keys='i486.rpm$'

installed_RPMS=""

for rpm in `ls $input_dir/rpm/*.rpm`; do

    name=`basename $rpm`

    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
        continue
    fi

    echo $rpm | egrep -q "$match_keys" 
    if [ $? -eq 0 ];then
        echo "detected VTUNE $rpm..."
    else
        continue
    fi

    if [ $INSTALL -eq 1 ];then

        echo "--> installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/itac/$version=${pubdir}/itac/$version $rpm || exit 1
        installed_RPMS="$name $installed_RPMS"
    fi

done

if [ $TARBALL -eq 1 ];then
    tar cfz intel-itac-${delim}-$version.tar.gz ${pubdir}/itac/$version
fi

if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi


