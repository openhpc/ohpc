#!/bin/sh
# this script checks the nagios.cfg file and comments out the following line which is incompatible with Nagios v3
# check_result_buffer_slots=4096
# this then allows your existing Nagios v2 nagios.cfg file to be operational with Nagios v3
# You will want to migrate to the Nagios v3 nagios.cfg file as soon as possible to take advantage of new features

# this is the nagios.cfg file we will modify
nagios_cfg=/etc/nagios/nagios.cfg


tmp1=`mktemp /tmp/nagios.cfg.XXXXXXXX`

# search for and replace the check_result_buffer_slots attribute into a temporary file
cat $nagios_cfg | sed --regexp-extended "s/^(\s*check_result_buffer_slots\s*=\s*)/# Line Commented out for Nagios v3 Compatibility\n#\1/g" > $tmp1

# check the diff
diff_output=`diff -u $nagios_cfg $tmp1`
diff_exit=$?

# now decided whether or not to replace the file
if [ "$diff_exit" = "0" ]; then
   echo "No changes were made to the Nagios Config file: $nagios_cfg"
elif [ "$diff_exit" = "1" ]; then
   echo "The following changes were made to the Nagios Config file: $nagios_cfg"
   echo "$diff_output"
   
   # since changes were made, move the temp file into place
   cp $nagios_cfg $nagios_cfg.v2
   mv $tmp1 $nagios_cfg
else
   echo "ERROR: Unexpected exit code from diff. No changes made to file: $nagios_cfg"
fi

rm -f $tmp1
