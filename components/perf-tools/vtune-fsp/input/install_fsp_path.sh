#!/bin/bash

release=vtune_amplifier_xe_2015_update1
relocate_ver=vtune_amplifier_xe_2015.1.1.380310

for rpm in `ls $release/rpm/*.rpm` `ls $release/CLI_Install/rpm/*.rpm`; do 
    echo $rpm
###     rpm -ivh --nodeps \
### 	--relocate /opt/intel/$relocate_ver=/opt/fsp/pub/vtune/$relocate_ver $rpm
done

#tar cfz intel-$release-fsp.tar.gz /opt/fsp/pub/vtune/$relocate_ver


