#!/bin/bash
#
# Simple developer convenience utility to generate release tarballs
# from local repo.

BRANCHNAME=`git rev-parse --abbrev-ref HEAD`
VERSION=`echo $BRANCHNAME | awk -F '_' '{print $2}'`

echo "Current branch name = $BRANCHNAME"
echo "FSP version         = $VERSION"

VERSION=15.16
TARBALL=docs-fsp-$VERSION.tar

if [ -e $TARBALL.gz ];then
    rm $TARBALL.gz
fi

echo " "
echo "Packaging docs distribution tarball for v$VERSION..."

git archive --format=tar --prefix=docs-fsp-$VERSION/ HEAD  > $TARBALL 


ABV_HASH=`git --no-pager log -1 HEAD --pretty=format:"%h%n"`

echo "Including git revision info (vc.tex)..."
echo "--> Abbrev. git hash = $ABV_HASH"

./common/vc

if [ ! -s vc.tex ];then 
    echo "Error: unable to generate vc.tex"
    exit 1
fi

cp vc.tex sles12/vanilla/
cp vc.tex centos7/vanilla/

tar rf $TARBALL --transform "flags=r;s|^|docs-fsp-$VERSION/sles12/vanilla/|" vc.tex
tar rf $TARBALL --transform "flags=r;s|^|docs-fsp-$VERSION/centos7/vanilla/|" vc.tex

gzip -n $TARBALL

if [ ! -s ./$TARBALL.gz ];then
   echo "Error generating tarball.."
   exit 1
fi

numfiles=`tar tvfz ./$TARBALL.gz | wc -l`
md5sum=`md5sum ./$TARBALL.gz | awk '{print $1}'`

echo " "
echo "--> $TARBALL generated from local git repo"
echo "--> numfiles included = $numfiles"
echo "--> md5sum            = $md5sum"


