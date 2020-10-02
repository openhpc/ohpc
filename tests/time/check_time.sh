#!/bin/bash
# Simple check to see if local host is synchronised with NTP server

which chronyc >& /dev/null || exit 1

chronyc tracking | grep "Not synchronised"
if [ $? -eq 0 ];then
    exit 1
else
    exit 0
fi

