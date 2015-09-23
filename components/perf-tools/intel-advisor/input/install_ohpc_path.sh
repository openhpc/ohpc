#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub

version=16.1.0.423501
release=advisor_xe_2016
relocate_ver=advisor_xe_20$version

input_dir=../../../compiler-families/intel-compilers/input/update2/parallel_studio_xe_2016_beta

match_keys='intel-advisor-xe'
skip_keys='i486.rpm$'
INSTALL=1
POST_UNINSTALL=1
TAR=1

installed_RPMS=""

for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm`; do 

#for rpm in `ls $input_dir/rpm/*.rpm`; do

    name=`basename $rpm`

    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
        continue
    fi

    echo $rpm | egrep -q "$match_keys" 
    if [ $? -eq 0 ];then
        echo "--> detected Intel Advisor rpm -> $name..."
    else
        continue
    fi
    
    if [ $INSTALL -eq 1 ];then
	echo "installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=${pubdir}/advisor/$version $rpm
        installed_RPMS="$name $installed_RPMS"
    fi
done

if [ $TAR -eq 1 ];then
    tar cfz intel-advisor-${delim}-$version.tar.gz ${pubdir}/advisor/$version
fi


if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi
