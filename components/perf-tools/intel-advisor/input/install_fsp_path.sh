#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=16.0.60.410057
release=advisor_xe_2016_beta_update2
relocate_ver=advisor_xe_20$version

input_dir=../../../compiler-families/intel-compilers/input/update1/parallel_studio_xe_2016_beta

match_keys='intel-advisor-xe'
skip_keys='i486.rpm$'
INSTALL=0
TAR=1

for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm`; do 

#for rpm in `ls $input_dir/rpm/*.rpm`; do

    name=`basename $rpm`

    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
#        echo "  --> skipping potential install of $rpm"
        continue
    fi

    echo $rpm | egrep -q "$match_keys" 
    if [ $? -eq 0 ];then
        echo "--> detected Intel Advisor rpm -> $name..."
    else
#        echo "skipping $rpm"
        continue
    fi
    
    if [ $INSTALL -eq 1 ];then
	echo "installing $rpm...."
#        rpm -ivh --nodeps --relocate /opt/intel/=/opt/fsp/pub/compiler/intel $rpm
        rpm -ivh --nodeps --relocate /opt/intel/$relocate_ver=/opt/fsp/pub/advisor/$version $rpm
    fi
done

if [ $TAR -eq 1 ];then
    tar cfz intel-advisor-fsp-$version.tar.gz /opt/fsp/pub/advisor/$version
#    tar cfz intel-advisor-fsp-$version.tar.gz /opt/fsp/pub/compiler/$version
fi


