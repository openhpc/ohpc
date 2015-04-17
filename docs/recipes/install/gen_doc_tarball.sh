#!/bin/bash
#
# Simple developer convenience utility to generate release tarballs
# from local repo.

BRANCHNAME=`git rev-parse --abbrev-ref HEAD`
VERSION=`echo $BRANCHNAME | awk -F '_' '{print $2}'`

echo "Current branch name = $BRANCHNAME"
echo "FSP version         = $VERSION"

VERSION=15.16
TARBALL=docs-$VERSION.tar.gz

if [ -e $TARBALL ];then
    rm $TARBALL
fi

echo " "
echo "Packaging docs distribution tarball for v$VERSION..."

git archive --format=tar --prefix=docs-$VERSION/ HEAD  | gzip -n > $TARBALL

if [ ! -s ./$TARBALL ];then
   echo "Error generating tarball.."
   exit 1
fi

numfiles=`tar tvfz ./$TARBALL | wc -l`
md5sum=`md5sum ./$TARBALL | awk '{print $1}'`

echo "--> $TARBALL generated from local git repo"
echo "--> numfiles included = $numfiles"
echo "--> md5sum            = $md5sum"


