#!/bin/bash

TAU_EBS_KEEP_UNRESOLVED_ADDR=1 tau_exec -v -T shared,param,icpc,papi,mpi,pdt,openmp,profile,trace -ebs ./CXX_tbb_test auto 100 silent
