export NODE_NAME=master4-sles12
export COMPUTE_HOSTS="node5, node6, node7, node8, node17, node18, node19, node20"

export REPO=http://fsp-obs.pdx.intel.com:82/ForestPeak:/15.05:/RC1/SLE_12_Intel/ForestPeak:15.05:RC1.repo
export USE_DYNAMIC_HOSTNUM=1
export BaseOS=sles12
export BaseOS=centos7.1

#../../parse_doc.pl steps.tex 
../../gen_inputs.pl ../../../zeus.mapping

#../../../parse_doc.pl --repo=$REPO steps.tex ../../../zeus.mapping
