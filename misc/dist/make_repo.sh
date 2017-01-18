#!/bin/bash


# determine standard repodir
repodir=""

if [ -s "/etc/os-release" ];then
    [[ `grep "CentOS"  /etc/os-release` ]] && repodir=/etc/yum.repos.d
    [[ `grep "Red Hat" /etc/os-release` ]] && repodir=/etc/yum.repos.d
    [[ `grep "SLES"    /etc/os-release` ]] && repodir=/etc/zypp/repos.d
else
    echo "Error: no /etc/os-release file found"
    exit 1
fi

if [ -z "${repodir}" ];then
    echo "Error: unknown or unsupported OS distro."
    echo "Please confirm local host matches an OpenHPC supported distro."
    exit 1
fi

if [ ! -d "${repodir}" ];then
    echo "Error: unable to find local repodir (expected ${repodir})"
    exit 1
fi

# sufficient credentials?
if [ "$EUID" -ne 0 ]; then
  echo "Error: Elevated credentials required to create files in ${repodir}"
  exit 1
fi

# setup local OpenHPC repo using downloaded repodata
localRepoDir=`dirname $0`
localRepoDir=$(dirname $(readlink -f $0) )
#dirname $(readlink -f relative/path/to/file)
localRepoFile=OpenHPC.local.repo

echo "--> Creating $localRepoFile file in $repodir"
echo "--> Local repodata stored in $localRepoDir"

pushd ${localRepoDir} >& /dev/null

if [ -s ${localRepoFile} ];then
    cp -p ${localRepoFile} $repodir
    # Update file path(s)
    sed -i "s!@PATH@!${localRepoDir}!" ${repodir}/${localRepoFile}
else
    echo "Error: expected ${localRepoFile} missing"
    exit 1
fi

popd >& /dev/null
