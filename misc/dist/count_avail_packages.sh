#!/bin/bash
#
#----------------------------------------------------------------------
# Simple utility to query OpenHPC repo to determine number of packages
# available.
#----------------------------------------------------------------------

if [ "$#" -lt 1 ];then
    echo "usage: `basename $0` [version]"
    exit 1
fi

which repoquery >& /dev/null || { echo "repoquery must be installed locally"; exit 1; }

version=$1
arches="x86_64 aarch64 noarch"
oses="CentOS_7 SLE_12"


minor_ver=`echo ${version} | cut -d '.' -f1,2`
micro_ver=`echo ${version} | cut -d '.' -f3`

if [[ -z $micro_ver ]];then
    micro_ver=0
fi


echo " "
echo "Querying available package counts: minor_ver=${minor_ver}, micro_ver=${micro_ver}"
echo " "


colon=""
factory=""
if [[ ${USE_FACTORY} -eq 1 ]]; then
    echo "[Querying from Factory repositories]"
    colon=":"
    factory="Factory/"
fi

for os in ${oses}; do
    repobase="http://build.openhpc.community/OpenHPC:/${minor_ver}${colon}/${factory}${os}"
    if [[ $micro_ver -gt 0 ]];then
	repoupdate="http://build.openhpc.community/OpenHPC:/${minor_ver}:/Update${micro_ver}${colon}/${factory}${os}"
    fi

    echo " "
    echo "repo base = ${repobase}"
    if [[ $micro_ver -gt 0 ]];then
	echo "repo update = ${repoupdate}"
    fi

    echo "--"
    echo "OS=${os}:"
    for arch in ${arches}; do
	echo -n "  ${arch}: "

	if [[ $micro_ver -eq 0 ]];then
	    repoquery --archlist=${arch} --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base '*' | wc -l
	else
	    repoquery --archlist=${arch} --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base '*' \
	        --repofrompath="ohpc-update,${repoupdate}" --repoid=ohpc-update '*' | wc -l
	fi
    done
done


