// -*- C++ -*-

/*
 * linear_recurrence.cpp
 *
 * Copyright (C) 2009-2012 Intel Corporation. All Rights Reserved.
 *
 * The source code contained or described herein and all
 * documents related to the source code ("Material") are owned by 
 * Intel Corporation or its suppliers or licensors. Title to the
 * Material remains with Intel Corporation or its suppliers and
 * licensors. The Material is protected by worldwide copyright
 * laws and treaty provisions.  No part of the Material may be
 * used, copied, reproduced, modified, published, uploaded,
 * posted, transmitted, distributed,  or disclosed in any way
 * except as expressly provided in the license provided with the
 * Materials.  No license under any patent, copyright, trade
 * secret or other intellectual property right is granted to or
 * conferred upon you by disclosure or delivery of the Materials,
 * either expressly, by implication, inducement, estoppel or
 * otherwise, except as expressly provided in the license
 * provided with the Materials. 
 */

/*
   A demonstration of an Intel(R) Cilk(TM) Plus reducer.
 
   This example demonstrates the use of Intel(R) Cilk(TM) Plus reducerss to compute a
   linear recurrence relation in parallel.  Specifically, 
   given x_0, {a_1, ... , a_n}, and {b_1, ... , b_n}, the program
   computes x_n according to the following recurrence relation:

        x_k = a_k + b_k * x_{k-1}   for 1 <= k <= n.
     
   You would think that the recurrence is inherently sequential, and
   that there is no way to compute x_k without knowing x_{k-1}.
   However, the recurrence can be solved in parallel by first
   rewriting it according to the following trick.  Write the recurrence
   as a linear recurrence over 2x1 vectors:

        [ 1   ]       [ 1    0   ]    [    1    ]
        [     ]   =   [          ] *  [         ]
        [ x_k ]       [ a_k  b_k ]    [ x_{k-1} ]

   Because matrix product is associative, it is clear that the
   problem can be solved  by first computing the product
   of the n 2x2 matrices

                [ 1    0   ] 
         R_k =  [          ]  ,
                [ a_k  b_k ] 

   which can be computed in parallel.

   The reducer Recurrence_Reducer below stores partial products
   of the R_k matrices, and effects the associative reduction via
   a specialized matrix multiplication.
*/

#include <stdio.h>
#include <cstdlib>
#include <iostream>
#include <cilk/cilk.h>
#include <cilk/reducer.h>
#include "cilktime.h"

extern "C++" {
    //structure to hold parameter a, b
    struct Para {
        long a;
        long b;
    };

    //reducer to store partial products of the R_k matrices, 
    //each R_k matrix is represented by value of a_k and b_k, 
    class Recurrence_Reducer
    {
    public:
        // Per-strand view of the data
        struct View
        {
            friend class Recurrence_Reducer;

        public:
            //Identity value for reducer: a = 0, b = 1,
            //which gives a unit 2x2 matrix
            View() : a(0), b(1) { }

        private:
            long a; 
            long b;
        };

    public:
        // View of reducer data
        struct Monoid: cilk::monoid_base<View>
        {
            static void reduce (View *left, View *right) {
                left->a = right->a + left->a * right->b; 
                left->b = right->b * left->b;
            }
        };

    private:
        // Hyperobject to serve up views
        cilk::reducer<Monoid> imp_;

     public:
        Recurrence_Reducer() : imp_() { }

        // Update operations
        inline Recurrence_Reducer& cal_next(const Para& value) {
            View &v = imp_.view();

            v.a = value.a + value.b * v.a; 
            v.b = value.b * v.b;
            return *this; 
        }
        long get_a() { return imp_.view().a; }
        long get_b() { return imp_.view().b; }
    };
}

//parallel code to compute linear recurrence
long compute_linear_rec_parallel (Para *list, int nn, long x_init) {
    Recurrence_Reducer rr;
    cilk_for(int i = 0; i < nn; ++i) {
        rr.cal_next(list[i]);
    }
    return rr.get_a() + rr.get_b() * x_init;
}

inline long cal_next(long a, long b, long x) {
    return a + b * x;
}
//sequential code to compute linear recurrence
long compute_linear_rec_sequential (Para *list, int nn, long x_init) {
    long x = x_init;
    for(int i = 0; i < nn; ++i) {
       x = cal_next(list[i].a, list[i].b, x);
    }
    return x;
}

int main(int argc, char* argv[]) {
    int nn = 5000000;
    
    if (argc == 2) {
        nn = std::atoi(argv[1]);
    }
    
    Para *plist = new Para[nn];
    if (plist == NULL) {
        std::cout << "Failed to create space for parameter list. " << std::endl;
        return 1;
    }

    int x_initial = 12;
    for(int i = 0; i < nn; i++) {
        plist[i].a = rand() % 22 - 10 ;
        plist[i].b = rand() % 13 - 6 ;
    }

    unsigned long long start_tick, end_tick;

    start_tick = cilk_getticks();
    long sr = compute_linear_rec_sequential(plist, nn, x_initial);
    end_tick = cilk_getticks();

    long t1 = (long)(end_tick - start_tick);

    start_tick = cilk_getticks();
    long pr = compute_linear_rec_parallel(plist, nn, x_initial);
    end_tick = cilk_getticks();
    long t2 = (long)(end_tick - start_tick);

    if (sr == pr) {
        std::cout << "the linear recurrence value is " << pr << std::endl;
        std::cout << "time for sequential execution: " << t1 / 1000.f
            << std::endl;
        std::cout << "time for parallel execution: " << t2 / 1000.f
            << std::endl;
        return 0;
    } else {
        std::cout << "error processing linear recurrence: " << std::endl;
        std::cout << "sequential version has value " << sr << " != " 
            << pr << " of the parallel version" << std::endl;
        return 1;
    }
}
