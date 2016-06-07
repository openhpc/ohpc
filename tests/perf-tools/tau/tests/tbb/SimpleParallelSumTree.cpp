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

class SimpleSumTask: public tbb::task {
    Value* const sum;
    TreeNode* root;
public:
    SimpleSumTask( TreeNode* root_, Value* sum_ ) : root(root_), sum(sum_) {}
    task* execute() {
        if( root->node_count<1000 ) {
            *sum = SerialSumTree(root);
        } else {
            Value x, y;
            int count = 1; 
            tbb::task_list list;
            if( root->left ) {
                ++count;
                list.push_back( *new( allocate_child() ) SimpleSumTask(root->left,&x) );
            }
            if( root->right ) {
                ++count;
                list.push_back( *new( allocate_child() ) SimpleSumTask(root->right,&y) );
            }
            // Argument to set_ref_count is one more than size of the list,
            // because spawn_and_wait_for_all expects an augmented ref_count.
            set_ref_count(count);
            spawn_and_wait_for_all(list);
            *sum = root->value;
            if( root->left ) *sum += x;
            if( root->right ) *sum += y;
        }
        return NULL;
    }
};

Value SimpleParallelSumTree( TreeNode* root ) {
    Value sum;
    SimpleSumTask& a = *new(tbb::task::allocate_root()) SimpleSumTask(root,&sum);
    tbb::task::spawn_root_and_wait(a);
    return sum;
}

