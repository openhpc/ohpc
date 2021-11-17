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
oses="EL_8 Leap_15"


major_ver=`echo ${version} | cut -d '.' -f1`
minor_ver=`echo ${version} | cut -d '.' -f1,2`
micro_ver=`echo ${version} | cut -d '.' -f3`
minor_dig=`echo ${version} | cut -d '.' -f2`

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

total=0

for os in ${oses}; do
    if [[ ${USE_FACTORY} -eq 1 ]]; then
	repobase="http://obs.openhpc.community:82/OpenHPC:/${minor_ver}${colon}/${factory}${os}"
    else
	repobase="http://repos.openhpc.community/.staging/OpenHPC/${major_ver}/${os}"
    fi

    if [[ $minor_dig -gt 0 ]];then
	if [[ ${USE_FACTORY} -eq 1 ]]; then
	    repoupdate="http://obs.openhpc.community:82/OpenHPC:/${minor_ver}:/Update${micro_ver}${colon}/${factory}${os}"
	else
	    repoupdate="http://repos.openhpc.community/.staging/OpenHPC/${major_ver}/update.${minor_ver}/${os}"
	fi
    fi

    echo " "
    echo "repo base = ${repobase}"
    if [[ $micro_dig -gt 0 ]];then
	echo "repo update = ${repoupdate}"
    fi

    echo "--"
    echo "OS=${os}:"
    for arch in ${arches}; do
	echo -n "  ${arch}: "

	if [[ $minor_dig -eq 0 ]];then
	    numrpms=`repoquery --archlist=${arch} --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base '*' | wc -l`
	    echo $numrpms
	    let "total=$total+$numrpms"
	else
	    numrpms=`repoquery --archlist=${arch} --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base '*' \
	        --repofrompath="ohpc-update,${repoupdate}" --repoid=ohpc-update '*' | wc -l`
	    echo $numrpms
	    let "total=$total+$numrpms"
	fi
    done
done


echo " "
echo "Total # of all RPMs = $total"
