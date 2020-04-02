#!/bin/bash

GIT_URL=https://github.com/openSUSE/python-rpm-macros.git
SPEC_FILE_NAME=python-rpm-macros.spec

PREV_VERSION=$(sed -rn 's/^Version:\s+(.*)$/\1/p' $SPEC_FILE_NAME)
rm python-rpm-macros-$PREV_VERSION.tar.bz2

tmpdir=tmp.$RANDOM

git clone --depth=1 $GIT_URL $tmpdir
cd $tmpdir
cp packaging/* ..

VERSION=$(git log -n 1 --date=format:%Y.%m.%d --format=format:%cd.%h)
git archive --format=tar --prefix=python-rpm-macros-$VERSION/ HEAD \
    | bzip2 -c > ../python-rpm-macros-$VERSION.tar.bz2

cd ..
rm -rf $tmpdir
sed -i -r 's/^(Version:\s+)(.*)$/\1'$VERSION'/' $SPEC_FILE_NAME

osc vc -m "version bump to $VERSION"
