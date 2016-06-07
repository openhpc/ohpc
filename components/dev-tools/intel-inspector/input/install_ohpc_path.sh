#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub
modscanner=../../../OHPC_mod_generator.sh

version=16.1.0.423441   # 1.0
version=16.1.2.450824   # 1.1

release=inspector_xe_2016_update2
relocate_ver=inspector_xe_20$version

match_keys=intel-inspector
skip_keys='i486.rpm$|-pset-'

INSTALL=1
POST_UNINSTALL=1
TARBALL=1

installed_RPMS=""

for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm` ; do 

    name=`basename $rpm`

    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
        continue
    fi

    echo $rpm | egrep -q "$match_keys" 
    if [ $? -eq 0 ];then
        echo "detected Inspector $rpm..."
    else
        continue
    fi

    if [ $INSTALL -eq 1 ];then
        echo "installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=${pubdir}/inspector/$version $rpm
        installed_RPMS="$name $installed_RPMS"
    fi
done

# generate relevant module file input
$modscanner ${pubdir}/inspector/$version/inspxe-vars.sh > modfile-$delim.input

if [ $TARBALL -eq 1 ];then
    tar cfz intel-inspector-${delim}-$version.tar.gz ${pubdir}/inspector/$version
fi

if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi


