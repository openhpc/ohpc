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
#include "tbb/task.h"

class OptimizedSumTask: public tbb::task {
    Value* const sum;
    TreeNode* root;
    bool is_continuation;
    Value x, y;
public:
    OptimizedSumTask( TreeNode* root_, Value* sum_ ) : root(root_), sum(sum_), is_continuation(false) {
    }
    tbb::task* execute() {
        tbb::task* next = NULL;
        if( !is_continuation ) {
            if( root->node_count<1000 ) {
                *sum = SerialSumTree(root);
            } else {
                // Create tasks before spawning any of them.
                tbb::task* a = NULL;
                tbb::task* b = NULL;
                if( root->left )
                    a = new( allocate_child() ) OptimizedSumTask(root->left,&x);
                if( root->right )
                    b = new( allocate_child() ) OptimizedSumTask(root->right,&y);
                recycle_as_continuation();
                is_continuation = true;
                set_ref_count( (a!=NULL)+(b!=NULL) );
                if( a ) {
                    if( b ) spawn(*b);
                } else 
                    a = b;
                next = a;
            }
        } else {
            *sum = root->value;
            if( root->left ) *sum += x;
            if( root->right ) *sum += y;
        } 
        return next;
    }
};

Value OptimizedParallelSumTree( TreeNode* root ) {
    Value sum;
    OptimizedSumTask& a = *new(tbb::task::allocate_root()) OptimizedSumTask(root,&sum);
    tbb::task::spawn_root_and_wait(a);
    return sum;
}

