#!/bin/bash

# Install from release rpms into standard OpenHPC path

delim=ohpc
pubdir=/opt/${delim}/pub
modscanner=../../../OHPC_mod_generator.sh

version=16.0.0-109  # 1.0
version=16.2.181  # 1.1

release_dir=parallel_studio_xe_2016_composer_edition_update2

#skip_keys='i486.rpm$|/intel-ipp|/intel-mkl-pgi'
skip_keys='i486.rpm$|intel-vtune-amplifier|intel-mpi|clck_|intel-advisor|intel-inspector|intel-itac|intel-tc-|intel-ta-'
INSTALL=1
TAR=0
POST_UNINSTALL=0
DEVEL=0

installed_RPMS=""

# Devel RPMs

for rpm in `ls $release_dir/rpm/*.rpm` ; do 

    name=`basename $rpm`

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
        rpm -ivh --nodeps --relocate /opt/intel=${pubdir}/compiler/intel $rpm
        installed_RPMS="$name $installed_RPMS"
    fi

##     if [ $UNINSTALL -eq 1 ];then
## 	localrpm=`basename --suffix=.rpm $rpm`
##         echo "--> uninstalling $localrpm ...."
##         rpm -e --nodeps $localrpm
##     fi

done

# generate relevant module file input
$modscanner ${pubdir}/compiler/intel/compilers_and_libraries_20$version/linux/bin/compilervars.sh -arch intel64 -platform linux > modfile-$delim.input


if [ $TAR -eq 1 ];then
    if [ $DEVEL -eq 1 ];then
        echo "creating devel tarball..."
        tar cfz intel-compilers-devel-${delim}-$version.tar.gz ${pubdir}/compiler/intel
    else
        echo "creating runtime tarball..."
        tar cfz intel-compilers-${delim}-$version.tar.gz ${pubdir}/compiler/intel
    fi

fi

if [ $POST_UNINSTALL -eq 1 ];then
    echo " "
    for pkg in $installed_RPMS; do 
        localrpm=`basename --suffix=.rpm $pkg`
        echo "[post-install] removing $localrpm...."
        rpm -e --nodeps $localrpm
    done
fi
