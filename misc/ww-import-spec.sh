#!/bin/bash

# Script to build updated Warewulf specfile templates from the latest source 

TARBALL="development.tar.gz"
SOURCE="https://github.com/warewulf/warewulf3/archive/$TARBALL"
EXTRACT="warewulf3-development"
PWDIR=$(pwd)
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
else echo "Warewulf found at $WWDIR"
fi

# Download latest Warewulf tarball
wget --progress=dot:mega -O $TMPDIR/$TARBALL $SOURCE
if [ $? -ne 0 ]; then
    echo "Cannot download Warewulf source."
    exit 1
fi

# Extract it to a temp directory
tar -zxf $TMPDIR/$TARBALL -C $TMPDIR
if [ $? -ne 0 ]; then
    echo "Cannot extract Warewulf tarball."
    exit 1
fi

for WWMOD in cluster common ipmi provision vnfs; do
echo "Creating warewulf-$WWMOD.spec"
MODDIR=$TMPDIR/$EXTRACT/$WWMOD
WWDIR=$MISCDIR/../components/provisioning/warewulf-$WWMOD
SPECFILE=$WWDIR/SPECS/warewulf-$WWMOD.spec.new
cd $MODDIR

# Generate the source specfiles
./autogen.sh > /dev/null

# Create a new specfile, adding the standard header
/bin/cat $MISCDIR/HEADER > $SPECFILE

# Add OpenHPC macros
/bin/cat <<EOF >> $SPECFILE

%include %{_sourcedir}/OHPC_macros

%define dname @WWNAME@
%define pname warewulf-%{dname}
%define wwsrvdir /srv

EOF

# Copy the source specfile starting at package name, through the Source tag. 
sed -n '/^Name:/,/^Source:/p' $MODDIR/warewulf-$WWMOD.spec >> $SPECFILE

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
sed -n '/^Source:/,/^%prep/p' $MODDIR/warewulf-$WWMOD.spec >> $SPECFILE

# Create a new prep section.
# Manually perform a CD and clean of the new build directory
# Create a symlink from the default setup directory to the correct subdirectory
# under the soon-to-be-extracted tarball.
# Then run the setup macro, disabling automatic pre-cleaning.
/bin/cat <<EOF >> $SPECFILE
cd %{_builddir}
%{__rm} -rf %{name}-%{version} @WWEXTRACT@
%{__ln_s} @WWEXTRACT@/%{dname} %{name}-%{version}
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
sed -n '/^%build/,/^%changelog/p' $MODDIR/warewulf-$WWMOD.spec >> $SPECFILE

# Replace the placeholders from the literal text
sed -i "s/@WWEXTRACT@/$EXTRACT/g" $SPECFILE
sed -i "s/@WWNAME@/$WWMOD/g" $SPECFILE

# Update the package name
sed -i "s#^Name:.*#Name:    %\{pname\}%\{PROJ_DELIM\}#" $SPECFILE

# Provide the original package, since non-OHPC parallel install is impossible 
VERSION=$(awk -F '[[:space:]]+' '/^Version:/{print $2}' $MODDIR/warewulf-$WWMOD.spec)
sed -i "/^Version:/a Provides: warewulf-$WWMOD = $VERSION" $SPECFILE

# Update Tags to OHPC WW RPMs
sed -i "/^\(Build\)\?Requires:/s/\bwarewulf-[^,\s]*\b/&\{PROJ_DELIM\}/g" $SPECFILE
sed -i "/^Conflicts:/s/\bwarewulf-[^,\s]*\b/&\{PROJ_DELIM\}/g" $SPECFILE

# Update the release and add the group definition
sed -i "s#^Release:.*#Release: 1%\{\?dist\}#" $SPECFILE

# Replace the duplicate source entry with the group definition
sed -i "s#^Source:.*#Group:   %\{PROJ_NAME\}/provisioning#" $SPECFILE

# Need to add autogen script to build from pristine source
sed -i "/%build/a ./autogen.sh" $SPECFILE

done

cd $PWDIR
rm -rf $TMPDIR
