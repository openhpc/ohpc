export NODE_NAME=master1-sles11sp3
export COMPUTE_HOSTS="node5, node6, node7, node8"

unset ENV
export IPMI_PASSWORD=root
../../../parse_doc.pl steps.tex ../../../zeus.mapping
