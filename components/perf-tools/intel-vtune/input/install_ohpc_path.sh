#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub
modscanner=../../../OHPC_mod_generator.sh

#version=16.1.1.434111 # 1.0
version=16.2.0.444464 # 1.1

release=vtune_amplifier_xe_2016_update2
relocate_ver=vtune_amplifier_xe_20$version

skip_arch=i486.rpm
INSTALL=1
POST_UNINSTALL=1
TARBALL=1

match_keys='intel-vtune'
skip_keys='i486.rpm$|-common-pset-'

installed_RPMS=""

for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm`; do 

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

# generate relevant module file input
$modscanner ${pubdir}/vtune_amplifier/$version/amplxe-vars.sh > modfile-$delim.input

if [ $TARBALL -eq 1 ];then
    tar cfz intel-vtune-amplifier-${delim}-$version.tar.gz ${pubdir}/vtune_amplifier/$version
fi

if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi




