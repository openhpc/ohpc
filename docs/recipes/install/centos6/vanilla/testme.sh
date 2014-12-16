export NODE_NAME=master4-centos65
export COMPUTE_HOSTS="node5, node6, node7, node8"

unset ENV
export IPMI_PASSWORD=root
../../../parse_doc.pl steps.tex ../../../zeus.mapping
