#!/bin/bash
#
# OpenHPC build script/utilities
#
#-----------------------------------------------------------------------
# Licensed under the Apache License, Version 2.0 (the "License"); you
# may not use this file except in compliance with the License. You may
# obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied. See the License for the specific language governing
# permissions and limitations under the License.
#-----------------------------------------------------------------------
# Utility to derive modules environment variable settings from shell
# script initialization file.
#-----------------------------------------------------------------------

if [ $# -lt 1 ];then
    echo "Usage: $0 <shell-script-to-eval> args"
    exit 1
fi

# environment variables to ignore

ignoreVars="^SHLVL=|^INTEL_LICENSE_FILE=|^PWD=|^_=|^WARNING:"

inFile=$1
shift
export remainArgs="$@"

if [ ! -s $inFile ];then
    echo "$inFile not available locally"
    exit 1
fi

TMPFILE=/tmp/mod_generator.$RANDOM
OUTFILE=$TMPFILE.out

cat <<EOF > $TMPFILE
#!/bin/bash
unset LD_LIBRARY_PATH
unset LIBRARY_PATH

MYPATH=\${PATH}

#source $inFile "$remainArgs" >& /dev/null
source $inFile ${remainArgs} >& /dev/null

/usr/bin/printenv | sed s!:\${MYPATH}!!
EOF


chmod 700 $TMPFILE || exit 1

env -i $TMPFILE | egrep -v $ignoreVars > $OUTFILE

if [ ! -s $OUTFILE ];then
    echo "No environment variables detected"
    exit 1
fi

maxLen=0

# scan for max variable name length

for var in `cat $OUTFILE`; do 
    locLen=`echo $var | awk -F = '{print $1}'| wc -m` || exit 1

    if [ $locLen -gt $maxLen ];then
        maxLen=$locLen
    fi
done

echo " "
echo "# OpenHPC machine generated from `basename $inFile`"

while read var ; do 
    varName=`echo $var | awk -F = '{print $1}'` || exit 1
    varDef=`echo $var | awk -F = '{print $2}'`  || exit 1

    echo $varName | egrep -q "PATH"

    if [ $? -eq 0 ];then
        printf "%-13s %-${maxLen}s %s\n" "prepend-path" $varName $varDef
    else
        # check if this variable needs extra quoting (say because it includes spaces)

        echo "$varDef" | egrep -q '\s'
        if [ $? -eq 0 ];then
            printf "%-13s %-${maxLen}s \"%s\"\n" "setenv" $varName "$varDef"
        else
            printf "%-13s %-${maxLen}s %s\n" "setenv" $varName "$varDef"
        fi
    fi
done < $OUTFILE

echo "# end of machine generated"
echo " "

