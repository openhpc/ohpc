#!/bin/bash
# 
# Copyright (c) 2015-2016, Gregory M. Kurtzer. All rights reserved.
# 
# “Singularity” Copyright (c) 2016, The Regents of the University of California,
# through Lawrence Berkeley National Laboratory (subject to receipt of any
# required approvals from the U.S. Dept. of Energy).  All rights reserved.
# 
# This software is licensed under a customized 3-clause BSD license.  Please
# consult LICENSE file distributed with the sources of this project regarding
# your rights to use or distribute this software.
# 
# NOTICE.  This Software was developed under funding from the U.S. Department of
# Energy and the U.S. Government consequently retains certain rights. As such,
# the U.S. Government has been granted for itself and others acting on its
# behalf a paid-up, nonexclusive, irrevocable, worldwide license in the Software
# to reproduce, distribute copies to the public, prepare derivative works, and
# perform publicly and display publicly, and to permit other to do so. 
# 
# 

## Basic sanity
if [ -z "$SINGULARITY_libexecdir" ]; then
    echo "Could not identify the Singularity libexecdir."
    exit 1
fi

## Load functions
if [ -f "$SINGULARITY_libexecdir/singularity/functions" ]; then
    . "$SINGULARITY_libexecdir/singularity/functions"
else
    echo "Error loading functions: $SINGULARITY_libexecdir/singularity/functions"
    exit 1
fi

if [ -z "${SINGULARITY_ROOTFS:-}" ]; then
    message ERROR "Singularity root file system not defined\n"
    exit 1
fi

if [ -z "${SINGULARITY_BUILDDEF:-}" ]; then
    exit
fi


########## BEGIN BOOTSTRAP SCRIPT ##########

install -d -m 0755 "$SINGULARITY_ROOTFS/dev"

cp -a /dev/null         "$SINGULARITY_ROOTFS/dev/null"      2>/dev/null || > "$SINGULARITY_ROOTFS/dev/null"
cp -a /dev/zero         "$SINGULARITY_ROOTFS/dev/zero"      2>/dev/null || > "$SINGULARITY_ROOTFS/dev/zero"
cp -a /dev/random       "$SINGULARITY_ROOTFS/dev/random"    2>/dev/null || > "$SINGULARITY_ROOTFS/dev/random"
cp -a /dev/urandom      "$SINGULARITY_ROOTFS/dev/urandom"   2>/dev/null || > "$SINGULARITY_ROOTFS/dev/urandom"


# dnf should probably be preferred if it's present, at some point we will make
# a dnf specific bootstrap module.
if INSTALL_CMD=`singularity_which zypper`; then
    message 1 "Found zypper at: $INSTALL_CMD\n"
else
    message ERROR "zypper not in PATH!\n"
    ABORT 1
fi

OSVERSION=`singularity_key_get "OSVersion" "$SINGULARITY_BUILDDEF"`
if [ -z "${OSVERSION:-}" ]; then
    if [ -f "/etc/SuSE-release" ]; then
        OSVERSION=`rpm -qf --qf '%{VERSION}' /etc/SuSE-release`
    else
        OSVERSION=12.2
    fi
fi

MIRROR=`singularity_key_get "MirrorURL" "$SINGULARITY_BUILDDEF" | sed -r "s/%\{?OSVERSION\}?/$OSVERSION/gi"`
if [ -z "${MIRROR:-}" ]; then
    message ERROR "No 'MirrorURL' defined in bootstrap definition\n"
    ABORT 1
fi
MIRROR_UPDATES=`singularity_key_get "UpdateURL" "$SINGULARITY_BUILDDEF" | sed -r "s/%\{?OSVERSION\}?/$OSVERSION/gi"`
if [ ! -z "${MIRROR_UPDATES:-}" ]; then
    message 1 "'UpdateURL' defined in bootstrap definition\n"
fi

INSTALLPKGS=`singularity_keys_get "Include" "$SINGULARITY_BUILDDEF"`

REPO_COUNT=0
ZYPP_CONF="/etc/zypp/bootstrap-zypp.conf"
export ZYPP_CONF

# Create the main portion of yum config
mkdir -p "$SINGULARITY_ROOTFS"

ZYPP_CONF_DIRNAME=`dirname $ZYPP_CONF`
mkdir -m 0755 -p "$SINGULARITY_ROOTFS/$ZYPP_CONF_DIRNAME"
mkdir -m 0755 -p "$SINGULARITY_ROOTFS/$ZYPP_CONF_DIRNAME/repos.d"

cp /etc/zypp/zypp.conf $SINGULARITY_ROOTFS/$ZYPP_CONF
echo 'cachedir=/var/cache/zypp-bootstrap' >> "$SINGULARITY_ROOTFS/$ZYPP_CONF"
cp /etc/zypp/repos.d/* $SINGULARITY_ROOTFS/$ZYPP_CONF_DIRNAME/repos.d/.

if ! eval "$INSTALL_CMD -c $SINGULARITY_ROOTFS/$ZYPP_CONF --root $SINGULARITY_ROOTFS --gpg-auto-import-keys -n install --auto-agree-with-licenses sles-release coreutils $INSTALLPKGS"; then
    message ERROR "Bootstrap failed... exiting\n"
    ABORT 255
fi

if ! eval "rm -rf $SINGULARITY_ROOTFS/var/cache/zypp-bootstrap"; then
    message WARNING "Failed cleaning Bootstrap packages\n"
fi

# If we got here, exit...
exit 0
