/*
    Copyright 2005-2013 Intel Corporation.  All Rights Reserved.

    The source code contained or described herein and all documents related
    to the source code ("Material") are owned by Intel Corporation or its
    suppliers or licensors.  Title to the Material remains with Intel
    Corporation or its suppliers and licensors.  The Material is protected
    by worldwide copyright laws and treaty provisions.  No part of the
    Material may be used, copied, reproduced, modified, published, uploaded,
    posted, transmitted, distributed, or disclosed in any way without
    Intel's prior express written permission.

    No license under any patent, copyright, trade secret or other
    intellectual property right is granted to or conferred upon you by
    disclosure or delivery of the Materials, either expressly, by
    implication, inducement, estoppel or otherwise.  Any license under such
    intellectual property rights must be express and approved by Intel in
    writing.
*/

#include "common.h"

#include <cstdlib>
#include <cstdio>
#include <cstring>

// The performance of this example can be significantly better when
// the objects are allocated by the scalable_allocator instead of the
// default "operator new".  The reason is that the scalable_allocator
// typically packs small objects more tightly than the default "operator new",
// resulting in a smaller memory footprint, and thus more efficient use of
// cache and virtual memory.  Also the scalable_allocator works faster for
// multi-threaded allocations.
//
// Pass stdmalloc as the 1st command line parameter to use the default "operator new"
// and see the performance difference.
#include "tbb/scalable_allocator.h"
#include "TreeMaker.h"
#include "tbb/tick_count.h"
#include "tbb/task_scheduler_init.h"

#include "utility.h"

using namespace std;

void Run( const char* which, Value(*SumTree)(TreeNode*), TreeNode* root, bool silent) {
    tbb::tick_count t0;
    if ( !silent ) t0 = tbb::tick_count::now();
    Value result = SumTree(root);
    if ( !silent ) printf ("%24s: time = %.1f msec, sum=%g\n", which, (tbb::tick_count::now()-t0).seconds()*1000, result);
}

int main( int argc, const char *argv[] ) {
    try{
        tbb::tick_count mainStartTime = tbb::tick_count::now();

        // The 1st argument is the function to obtain 'auto' value; the 2nd is the default value
        // The example interprets 0 threads as "run serially, then fully subscribed"
        utility::thread_number_range threads( tbb::task_scheduler_init::default_num_threads, 0 );
        long number_of_nodes = 10000000;
        bool silent = false;
        bool use_stdmalloc = false;

        utility::parse_cli_arguments(argc,argv,
            utility::cli_argument_pack()
            //"-h" option for displaying help is present implicitly
            .positional_arg(threads,"n-of-threads",utility::thread_number_range_desc)
            .positional_arg(number_of_nodes,"number-of-nodes","the number of nodes")
            .arg(silent,"silent","no output except elapsed time")
            .arg(use_stdmalloc,"stdmalloc","use standard allocator")
        );

        TreeNode* root;
        { // In this scope, TBB will use default number of threads for tree creation
            tbb::task_scheduler_init init;

            if( use_stdmalloc ) {
                if ( !silent ) printf("Tree creation using standard operator new\n");
                root = TreeMaker<stdmalloc>::create_and_time( number_of_nodes, silent );
            } else {
                if ( !silent ) printf("Tree creation using TBB scalable allocator\n");
                root = TreeMaker<tbbmalloc>::create_and_time( number_of_nodes, silent );
            }
        }

        // Warm up caches
        SerialSumTree(root);
        if ( !silent ) printf("Calculations:\n");
        if ( threads.first ) {
            for(int p = threads.first;  p <= threads.last; p = threads.step(p) ) {
                if ( !silent ) printf("threads = %d\n", p );
                tbb::task_scheduler_init init( p );
                Run ( "SimpleParallelSumTree", SimpleParallelSumTree, root, silent );
                Run ( "OptimizedParallelSumTree", OptimizedParallelSumTree, root, silent );
            }
        } else { // Number of threads wasn't set explicitly. Run serial and two parallel versions
            Run ( "SerialSumTree", SerialSumTree, root, silent );
            tbb::task_scheduler_init init;
            Run ( "SimpleParallelSumTree", SimpleParallelSumTree, root, silent );
            Run ( "OptimizedParallelSumTree", OptimizedParallelSumTree, root, silent );
        }
        utility::report_elapsed_time((tbb::tick_count::now() - mainStartTime).seconds());
        return 0;
    }catch(std::exception& e){
        std::cerr<<"error occurred. error text is :\"" <<e.what()<<"\"\n";
        return 1;
    }
}
