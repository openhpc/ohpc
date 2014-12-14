export NODE_NAME=master2-sles11sp3
export COMPUTE_HOSTS="node5, node6, node7, node8"
#export COMPUTE_HOSTS="node9, node10, node11, node12"

unset ENV
export IPMI_PASSWORD=root
../../../parse_doc.pl steps.tex ../../../zeus.mapping
