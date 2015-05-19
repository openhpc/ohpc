#!/bin/bash

# FSP: install from release rpms into standard FSP path

version=16.0.0-056
#release=composer_xe_$version
release_dir=update1/parallel_studio_xe_2016_beta

#skip_keys='i486.rpm$|/intel-ipp|/intel-mkl-pgi'
skip_keys='i486.rpm$|intel-vtune-amplifier|intel-mpi|clck_|intel-advisor|intel-inspector|intel-itac|intel-tc-|intel-ta-'
INSTALL=1
TAR=1
UNINSTALL=0
DEVEL=1

# Devel RPMs

for rpm in `ls $release_dir/rpm/*.rpm` ; do 

    echo $rpm | egrep -q "$skip_keys" 
    if [ $? -eq 0 ];then
        echo "  --> skipping potential install of $rpm"
        continue
    fi

    echo $rpm | egrep -q '\-devel\-\S*.rpm'
    if [ $? -eq 0 ];then
        if [ $DEVEL -eq 1 ];then
            echo "  (devel) $rpm found"
        else
            echo "  --> skipping potential install of (devel) $rpm"
            continue
        fi
    else
        if [ $DEVEL -eq 0 ];then
            echo "  (non-devel) $rpm found"
        else
            echo "  --> skipping potential install of (non-devel) $rpm"
            continue
        fi
    fi


    if [ $INSTALL -eq 1 ];then
        echo "--> installing $rpm...."
        rpm -ivh --nodeps --relocate /opt/intel=/opt/fsp/pub/compiler/intel $rpm
    fi

    if [ $UNINSTALL -eq 1 ];then
	localrpm=`basename --suffix=.rpm $rpm`
        echo "--> uninstalling $localrpm ...."
        rpm -e --nodeps $localrpm
    fi
done

if [ $TAR -eq 1 ];then
    if [ $DEVEL -eq 1 ];then
        echo "creating devel tarball..."
        tar cfz intel-compilers-devel-fsp-$version.tar.gz /opt/fsp/pub/compiler/intel
    else
        echo "creating runtime tarball..."
        tar cfz intel-compilers-fsp-$version.tar.gz /opt/fsp/pub/compiler/intel
    fi

fi


