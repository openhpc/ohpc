#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub
modscanner=../../../OHPC_mod_generator.sh

version=16.1.30.450722
release=advisor_xe_2016_update3
relocate_ver=advisor_xe_20$version

match_keys='intel-advisor-xe'
skip_keys='i486.rpm$|pset'
INSTALL=1
POST_UNINSTALL=1
TAR=1

installed_RPMS=""

for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm`; do 

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

# generate relevant module file input
$modscanner ${pubdir}/advisor/$version/advixe-vars.sh > modfile-$delim.input

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
