#!/bin/bash

# Script to build updated Warewulf specfile templates from the latest source 

SOURCE="https://github.com/warewulf/warewulf3/archive/development.tar.gz"

# Option to replace everything in the source tree.
if [ "$1" == "overwrite" ]; then
	echo "Overwriting original specfiles"
	EXTENSION="spec"
else
	EXTENSION="spec.new"
fi

TARBALL=$(basename $SOURCE)
cd ..

TMPDIR=$(mktemp -d)
if [ $? -ne 0 ]; then
    echo "Cannot create temporary directory."
    exit 1
fi

MISCDIR=$(find ~+ -path "*/ohpc/misc/HEADER" -printf "%h\n" -quit 2>/dev/null)
if [ $? -ne 0 ]; then
    echo "Cannot find HEADER file."
    exit 1
fi

if ! [ -d $MISCDIR/../components/provisioning/warewulf-common ]; then
    echo "Cannot find Warewulf component directory."
    exit 1
else echo "Warewulf found at $MISCDIR/../components/provisioning/warewulf-common"
fi

# Download latest Warewulf tarball
wget --progress=dot:mega -O $TMPDIR/$TARBALL $SOURCE
if [ $? -ne 0 ]; then
    echo "Cannot download Warewulf source."
    exit 1
fi

EXTRACT=$(tar --no-recursion --exclude='*/*' -tzf $TMPDIR/$TARBALL | tr -d /)

# Extract it to a temp directory
mkdir -p $TMPDIR/$EXTRACT
tar --strip-components=1 -zxf $TMPDIR/$TARBALL -C $TMPDIR/$EXTRACT
if [ $? -ne 0 ]; then
    echo "Cannot extract Warewulf tarball."
    exit 1
fi

for WWMOD in cluster common ipmi provision vnfs; do
echo "Creating warewulf-$WWMOD.$EXTENSION"
MODDIR=$TMPDIR/$EXTRACT/$WWMOD
WWDIR=$MISCDIR/../components/provisioning/warewulf-$WWMOD
SPECFILE=$WWDIR/SPECS/warewulf-$WWMOD.$EXTENSION
cd $MODDIR

# Grab the package version from the autoconf file
WWVER=$(awk -F ',' '/^AC_INIT/{print $2}' $MODDIR/configure.ac | tr -d "[:blank:]\[\]")

# If overwrite is enabled, copy the source tarball
if [ "$1" == "overwrite" ]; then
     rm -f $WWDIR/SOURCES/$TARBALL
     cp $TMPDIR/$TARBALL $WWDIR/SOURCES
     echo "Tarball copied to warewulf-$WWMOD/SOURCES"
fi

# Create a new specfile, adding the standard header
/bin/cat $MISCDIR/HEADER > $SPECFILE

# Add OpenHPC macros (avoiding escape characters)
/bin/cat <<EOF >> $SPECFILE

%include %{_sourcedir}/OHPC_macros

%define dname @WWMOD@
%define pname warewulf-%{dname}
%define wwsrvdir /srv
%define wwextract @EXTRACT@

EOF

sed -i "s/@WWMOD@/$WWMOD/" $SPECFILE
sed -i "s/@EXTRACT@/$EXTRACT/" $SPECFILE

# Copy the source specfile starting at package name, through the Source tag. 
sed -n '/^Name:/,/^Source:/p' $MODDIR/warewulf-$WWMOD.spec.in >> $SPECFILE

# Add patches found in patch directory
PATCHES=($(find $WWDIR/SOURCES -type f -name *.patch -exec basename {} \; | sort))
COUNT=${#PATCHES[@]}
ARMON="-1"
ARMOFF="-1"
for (( i=0; i<$COUNT; i++ )); do
    if [[ ${PATCHES[$i]} =~ '.aarch64.' ]]; then
       [[ "$ARMON" == "-1" ]] && ARMON=$i
       ARMOFF=$((i+1))
    fi
    echo "Patch$i:  ${PATCHES[$i]}" >> $SPECFILE
done

# Update the source
sed -i "s#^Source:.*#Source0: $SOURCE#" $SPECFILE

# Copy the source specfile from the Source tag, through the prep section. 
sed -n '/^Source:/,/^%prep/p' $MODDIR/warewulf-$WWMOD.spec.in >> $SPECFILE

# Create a new prep section.
# Manually perform a CD and clean of the new build directory
# Create a symlink from the default setup directory to the correct subdirectory
# under the soon-to-be-extracted tarball.
# Then run the setup macro, disabling automatic pre-cleaning.
/bin/cat <<EOF >> $SPECFILE
cd %{_builddir}
%{__rm} -rf %{name}-%{version} %{wwextract}
%{__ln_s} %{wwextract}/%{dname} %{name}-%{version}
%setup -q -D
EOF

# Apply patches found in patch directory
for (( i=0; i<$COUNT; i++ )); do
    [[ $i == $ARMON ]] && echo "%ifarch aarch64" >> $SPECFILE
    [[ $i == $ARMOFF ]] && echo "%endif" >> $SPECFILE
    echo "%patch$i -p1" >> $SPECFILE
done

printf "\n\n" >> $SPECFILE

# Copy the remaining specfile contents, starting at the build section
sed -n '/^%build/,/^%changelog/p' $MODDIR/warewulf-$WWMOD.spec.in >> $SPECFILE

# Update the package name
sed -i "s#^Name:.*#Name:    %\{pname\}%{PROJ_DELIM}#" $SPECFILE

# Update the version; also provide original package for dependencies,
# since only 1 WW install can exist on a system.
sed -i "s#^Version:.*#Version: $WWVER\nProvides: warewulf-$WWMOD = $WWVER#" $SPECFILE

# Update Tags to OHPC WW RPMs
VALIDTEXT='warewulf-\(cluster\|common\|ipmi\|provision\|vnfs\)'
sed -i "/^\(Build\)\?Requires:/s/$VALIDTEXT/&%{PROJ_DELIM}/g" $SPECFILE
sed -i "/^Conflicts:/s/$VALIDTEXT/&%{PROJ_DELIM}/g" $SPECFILE

# Update the release and add the group definition
sed -i "s#^Release:.*#Release: 1%{?dist}#" $SPECFILE

# Replace the duplicate source entry with the group definition
sed -i "s#^Source:.*#Group:   %{PROJ_NAME}/provisioning#" $SPECFILE

# Need to add autogen script to build from pristine source
sed -i "/^%build/a ./autogen.sh" $SPECFILE

done

rm -rf $TMPDIR

