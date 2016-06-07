// -*- C++ -*-

/*
 * matrix.cpp
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
 */

#include <cstdlib>
#include <cilk/cilk.h>
#include <iostream>
#include <cstddef>
#include "cilktime.h"

#define RECURSION_THRESHOLD 10

// a naive, iterative, sequential matrix multiplication algorithm
template<typename T> void multiply_iter_seq(int ii, int jj, int kk, T* A, T* B,
    T* C)
{
	// Note the loop order. Exchanging the two inner loops degrades performance
	// because of cache access.
    for (int i = 0; i < ii; ++i)
        for (int k = 0; k < kk; ++k)
            for (int j = 0; j < jj; ++j)
                C[i * jj + j] += A[i * kk + k] + B[k * jj + j];
}

// a naive, iterative, parallel matrix multiplication algorithm
template<typename T> void multiply_iter_par(int ii, int jj, int kk, T* A, T* B,
    T* C)
{
    cilk_for(int i = 0; i < ii; ++i)
        for (int k = 0; k < kk; ++k)
            cilk_for(int j = 0; j < jj; ++j)
                C[i * jj + j] += A[i * kk + k] + B[k * jj + j];
}

// a recursive, sequential matrix multiplication algorithm
template<typename T> void multiply_rec_seq_helper(int i0, int i1, int j0,
    int j1, int k0, int k1, T* A, ptrdiff_t lda, T* B, ptrdiff_t ldb, T* C,
    ptrdiff_t ldc)
{
    int di = i1 - i0;
    int dj = j1 - j0;
    int dk = k1 - k0;
    if (di >= dj && di >= dk && di >= RECURSION_THRESHOLD) {
        int mi = i0 + di / 2;
        multiply_rec_seq_helper(i0, mi, j0, j1, k0, k1, A, lda, B, ldb, C, ldc);
        multiply_rec_seq_helper(mi, i1, j0, j1, k0, k1, A, lda, B, ldb, C, ldc);
    } else if (dj >= dk && dj >= RECURSION_THRESHOLD) {
        int mj = j0 + dj / 2;
        multiply_rec_seq_helper(i0, i1, j0, mj, k0, k1, A, lda, B, ldb, C, ldc);
        multiply_rec_seq_helper(i0, i1, mj, j1, k0, k1, A, lda, B, ldb, C, ldc);
    } else if (dk >= RECURSION_THRESHOLD) {
        int mk = k0 + dk / 2;
        multiply_rec_seq_helper(i0, i1, j0, j1, k0, mk, A, lda, B, ldb, C, ldc);
        multiply_rec_seq_helper(i0, i1, j0, j1, mk, k1, A, lda, B, ldb, C, ldc);
    } else {
        for (int i = i0; i < i1; ++i)
            for (int k = k0; k < k1; ++k)
                for (int j = j0; j < j1; ++j)
                    C[i * ldc + j] += A[i * lda + k] * B[k * ldb + j];
    }
}

template<typename T> inline void multiply_rec_seq(int ii, int jj, int kk, T* A,
    T* B, T* C)
{
    multiply_rec_seq_helper(0, ii, 0, jj, 0, kk, A, kk, B, jj, C, jj);
}

// a recursive, parallel matrix multiplication algorithm
template<typename T> void multiply_rec_par_helper(int i0, int i1, int j0,
    int j1, int k0, int k1, T* A, ptrdiff_t lda, T* B, ptrdiff_t ldb, T* C,
    ptrdiff_t ldc)
{
    int di = i1 - i0;
    int dj = j1 - j0;
    int dk = k1 - k0;
    if (di >= dj && di >= dk && di >= RECURSION_THRESHOLD) {
        int mi = i0 + di / 2;
        cilk_spawn multiply_rec_par_helper(i0, mi, j0, j1, k0, k1, A, lda, B,
            ldb, C, ldc);
        multiply_rec_par_helper(mi, i1, j0, j1, k0, k1, A, lda, B, ldb, C, ldc);
        cilk_sync;
    } else if (dj >= dk && dj >= RECURSION_THRESHOLD) {
        int mj = j0 + dj / 2;
        cilk_spawn multiply_rec_par_helper(i0, i1, j0, mj, k0, k1, A, lda, B,
            ldb, C, ldc);
        multiply_rec_par_helper(i0, i1, mj, j1, k0, k1, A, lda, B, ldb, C, ldc);
        cilk_sync;
    } else if (dk >= RECURSION_THRESHOLD) {
        int mk = k0 + dk / 2;
        // N.B. These two calls cannot be run in parallel without introducing
        //      a race.
        multiply_rec_par_helper(i0, i1, j0, j1, k0, mk, A, lda, B, ldb, C, ldc);
        multiply_rec_par_helper(i0, i1, j0, j1, mk, k1, A, lda, B, ldb, C, ldc);
    } else {
        for (int i = i0; i < i1; ++i)
            for (int k = k0; k < k1; ++k)
                for (int j = j0; j < j1; ++j)
                    C[i * ldc + j] += A[i * lda + k] * B[k * ldb + j];
    }
}

template<typename T> inline void multiply_rec_par(int ii, int jj, int kk, T* A,
    T* B, T* C)
{
    multiply_rec_par_helper(0, ii, 0, jj, 0, kk, A, kk, B, jj, C, jj);
}

int matrix_main(int ii, int jj, int kk)
{
    unsigned long long start_tick, end_tick;

    // Create two random input matrices.

    std::cout << "Multiply a " << ii << "x" << kk << " matrix by a " << kk
        << "x" << jj << " matrix" << std::endl << " to produce a " << ii << "x"
        << jj << " matrix." << std::endl << std::endl;

    double* A = (double*) calloc(ii* kk, sizeof(double));
    cilk_for(int i = 0; i < ii * kk; i++) {
        // Populate A 
        A[i] = (double) ((i * i) % 1024 - 512) / 512;
    }

    double* B = (double*) calloc(kk* jj, sizeof(double));
    cilk_for(int i = 0; i < kk * jj; i++) {
        // Populate A 
        B[i] = (double) ((i * i) % 1024 - 512) / 512;
    }

    // Compare various matrix multiplication algorithms on these two inputs.
    {
        std::cout << "1) Naive, Iterative Algorithm. Sequential and Parallel."
            << std::endl;
        long seq_time; {
            std::cout << "Running Iterative Sequential version..." << std::endl;

            double* C = (double*) calloc(ii* jj, sizeof(double));

            start_tick = cilk_getticks();
            multiply_iter_seq(ii, jj, kk, A, B, C);
            end_tick = cilk_getticks();

            free(C);
            seq_time = (long)(end_tick - start_tick);
            std::cout << "  Iterative Sequential version took "
                    << seq_time / 1000.f << " milliseconds." << std::endl;
        }
        long par_time; {
            std::cout << "Running Iterative Parallel version..." << std::endl;
            double* C = (double*) calloc(ii* jj, sizeof(double));

            start_tick = cilk_getticks();
            multiply_iter_par(ii, jj, kk, A, B, C);
            end_tick = cilk_getticks();

            free(C);
            par_time = (long)(end_tick - start_tick);
            std::cout << "  Iterative Parallel version took   "
                << par_time / 1000.f << " milliseconds." << std::endl;
        }
        float speed_up = 100;
        if (par_time > 0) {
            speed_up = seq_time / (float) par_time;
        }
        std::cout << "  Parallel Speedup: " << speed_up << std::endl
            << std::endl;
    } {
        std::cout << "2) Recursive Algorithm. Sequential and Parallel."
            << std::endl;
        long seq_time; {
            std::cout << "Running Recursive Sequential version..." << std::endl;
            double* C = (double*) calloc(ii* jj, sizeof(double));

            start_tick = cilk_getticks();
            multiply_rec_seq(ii, jj, kk, A, B, C);
            end_tick = cilk_getticks();

            free(C);
            seq_time = (long)(end_tick - start_tick);
            std::cout << "  Recursive Sequential version took "
                << seq_time / 1000.f << " milliseconds." << std::endl;
        }
        long par_time; {
            std::cout << "Running Recursive Parallel version..." << std::endl;
            double* C = (double*) calloc(ii* jj, sizeof(double));

            start_tick = cilk_getticks();
            multiply_rec_par(ii, jj, kk, A, B, C);
            end_tick = cilk_getticks();

            free(C);
            par_time = (long)(end_tick - start_tick);
            std::cout << "  Recursive Parallel version took   "
                << par_time / 1000.f << " milliseconds." << std::endl;
        }

        float speed_up = 100;
        if (par_time > 0) {
            speed_up = seq_time / (float) par_time;
        }
        std::cout << "  Parallel Speedup: " << speed_up << std::endl
            << std::endl;
    }

    free(A);
    free(B);
    return 0;
}

int main(int argc, char* argv[])
{
    int ii = 687, jj = 1107, kk = 837;
    if (argc > 3) {
        ii = std::atoi(argv[1]);
        jj = std::atoi(argv[2]);
        kk = std::atoi(argv[3]);
    }
    return matrix_main(ii, jj, kk);
}
