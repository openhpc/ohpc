/*
 * qsort-mutex.cpp
 *
 * Copyright (C) 2009-2013 Intel Corporation. All Rights Reserved.
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
 *
 * Demonstrate Intel(R) Cilk(TM) Plus with Intel(R) Threading Building
 * Blocks mutexes.  This sample counts the total number of times that 
 * the data is partitioned.
 */

#include <iostream>
#include <algorithm>
#include <iterator>
#include <functional>
#include <cstdlib>
#include <tbb/mutex.h>
#include <cilk/cilk.h>
#include "cilktime.h"

#ifndef _WIN32
#include <unistd.h>
#else
#include <Windows.h>
#endif

unsigned partition_count = 0;

// Sort the range between bidiriectional iterators begin and end.
// end is one past the final element in the range.
// Use the Quick Sort algorithm,  using recursive divide and conquer.
// This function is NOT the same as the Standard C Library qsort() function.
// This implementation uses Intel Cilk Plus and Intel TBB mutexes.
template<typename Iter> void sample_qsort(Iter begin, Iter end, tbb::mutex* m)
{
    typedef typename std::iterator_traits<Iter>::value_type T;
    if (begin != end) {
        --end;  // Exclude last element (pivot) from partition
        Iter middle = std::partition(begin, end,
                          std::bind2nd(std::less<T>(), *end));
        // Lock the partition counter while it's being incremented to prevent
        // a race
        while (!m->try_lock()) {
#ifdef _WIN32
            Sleep(10);
#else
            usleep(10000);
#endif
        }
        partition_count++;
        m->unlock();

        using std::swap;
        swap(*end, *middle);    // move pivot to middle
        cilk_spawn sample_qsort(begin, middle, m);
        sample_qsort(++middle, ++end, m); // Exclude pivot and restore end
        cilk_sync;
    }
}

// A simple test harness 
int qmain(int n)
{
    unsigned long long start_tick, end_tick;
    long elapsed_milliseconds;
    int* a = new int[n];

    for (int i = 0; i < n; ++i)
        a[i] = i;

    std::random_shuffle(a, a + n);

    std::cout << "Sorting " << n << " integers" << std::endl;
    tbb::mutex *m = new tbb::mutex;

    start_tick = cilk_getticks();
    sample_qsort(a, a + n, m);
    end_tick = cilk_getticks();

    elapsed_milliseconds = (long)(end_tick - start_tick) / 1000.f;
    std::cout << elapsed_milliseconds << " milliseconds" << std::endl;
    delete m;

    // Confirm that a is sorted and that each element contains the index.
    for (int i = 0; i < n - 1; ++i) {
        if (a[i] >= a[i + 1] || a[i] != i) {
            std::cout << "Sort failed at location i=" << i << " a[i] = " << a[i]
                << " a[i+1] = " << a[i + 1] << std::endl;
            delete[] a;
            return 1;
        }
    }
    std::cout << "Sort succeeded." << std::endl;
    delete[] a;
    return 0;
}

int main(int argc, char* argv[])
{
    int n = 10000000;
    if (argc > 1) {
        n = std::atoi(argv[1]);
    }

    return qmain(n);
}
