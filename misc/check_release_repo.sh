#!/bin/bash

which repoquery >& /dev/null || { echo "repoquery must be installed locally"; exit 1; }

if [ "$#" -lt 1 ];then
    echo "usage: `basename $0` [version]"
    exit 1
fi

version=$1

minor_ver=`echo ${version} | cut -d '.' -f1,2`
micro_ver=`echo ${version} | cut -d '.' -f3`

distros="CentOS_7 SLE_12"

for baseos in $distros; do
    for arches in x86_64 aarch64; do
	arch="$arches,noarch"

	echo " "
	echo "--"
	echo "Checking BaseOS=$baseos, arch=$arch..."
	
	repobase="http://build.openhpc.community/OpenHPC:/${minor_ver}/${baseos}"
	repoupdate="http://build.openhpc.community/OpenHPC:/${minor_ver}:/Update${micro_ver}/${baseos}"

	repoquery -a --archlist=${arch} \
	    --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base \
	    --repofrompath="ohpc-update,${repoupdate}" --repoid=ohpc-update | sort > /tmp/packages.production

	let micro_ver_prev="$micro_ver-1"
	repoFactory="http://build.openhpc.community/OpenHPC:/${minor_ver}:/Update${micro_ver}:/Factory/${baseos}"
	repoPrevUpdate="http://build.openhpc.community/OpenHPC:/${minor_ver}:/Update${micro_ver_prev}/${baseos}"

	repoquery -a --archlist=${arch} \
	    --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base \
	    --repofrompath="ohpc-prev-update,${repoPrevUpdate}" --repoid=ohpc-prev-update \
	    --repofrompath="ohpc-update-factory,${repoFactory}" --repoid=ohpc-update-factory | sort > /tmp/packages.factory
	
	diff /tmp/packages.factory /tmp/packages.production
	
    done
done
