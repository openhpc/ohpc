#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub

release=vtune_amplifier_xe_2016  # 15.44
relocate_ver=vtune_amplifier_xe_20$version

input_dir=../../../compiler-families/intel-compilers/input/update2/parallel_studio_xe_2016_beta

skip_arch=i486.rpm
INSTALL=1
POST_UNINSTALL=1
TARBALL=1

match_keys='intel-vtune'
skip_keys='i486.rpm$|CLI_Install/rpm/intel-vtune-amplifier-xe-2016-common-pset-16.1-424694.noarch.rpm'
#skip_dup='CLI_Install/rpm/intel-vtune-amplifier-xe-2016-common-pset-16.1-424694.noarch.rpm'

installed_RPMS=""

for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm`; do 

#for rpm in `ls $input_dir/rpm/*.rpm`; do

    name=`basename $rpm`

    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
	echo "Skipping $rpm"
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
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=${pubdir}/vtune_amplifier/$version $rpm || exit 1
        installed_RPMS="$name $installed_RPMS"
    fi

done


if [ $TARBALL -eq 1 ];then
    tar cfz intel-vtune-amplifier-%{delim}-$version.tar.gz ${pubdir}/vtune_amplifier/$version
fi

if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi




