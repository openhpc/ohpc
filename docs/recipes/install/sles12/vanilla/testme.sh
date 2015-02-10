export NODE_NAME=master3-sles11sp3
#export COMPUTE_HOSTS="node5, node6, node7, node8"
export COMPUTE_HOSTS="node9, node10, node11, node12"

unset ENV
export IPMI_PASSWORD=root
export REPO=http://fsp-obs.pdx.intel.com:82/ForestPeak:/15.05:/RC1/SLE_12_Intel/ForestPeak:15.05:RC1.repo

../../../parse_doc.pl steps.tex ../../../zeus.mapping
#../../../parse_doc.pl --repo=$REPO steps.tex ../../../zeus.mapping
