/*
 * matrix-multiply.ccpp
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

#include <cstdlib>
#include <iostream>
#include <cilk/cilk.h>
#include "cilktime.h"

#define DEFAULT_MATRIX_SIZE 4

// Multiply double precsion square n x n matrices. A = B * C
// Matrices are stored in row major order.
// A is assumed to be initialized.
void matrix_multiply(double* A, double* B, double* C, unsigned int n)
{
    if (n < 1) {
        return;
    }

    cilk_for(unsigned int i = 0; i < n; ++i) {
        // This is the only Intel(R) Cilk(TM) Plus keyword used in this program
		// Note the order of the loops and the code motion of the i * n and k * n
		// computation. This gives a 5-10 performance improvment over exchanging
		// the j and k loops.
		int itn = i * n;
        for (unsigned int k = 0; k < n; ++k) {
            for (unsigned int j = 0; j < n; ++j) {
    	        int ktn = k * n;
                // Compute A[i,j] in the innner loop.
                A[itn + j] += B[itn + k] * C[ktn + j];
            }
        }
    }
    return;
}

void print_matrix(double* M, int nn)
{
    for (int i = 0; i < nn; ++i) {
        for (int j = 0; j < nn; ++j) {
            std::cout << M[i * nn + j] << ",  ";
        }
        std::cout << std::endl;
    }
    return;
}

int main(int argc, char** argv) {
    // Create random input matrices. Override the default size with argv[1]
    // Warning: Matrix indexing is 0 based.
    int nn = DEFAULT_MATRIX_SIZE;
    if (argc > 1) {
        nn = std::atoi(argv[1]);
    }

    std::cout << "Simple algorithm: Multiply two " << nn << " by " << nn
        << " matrices, computing A = B*C" << std::endl;

    double* A = (double*) calloc(nn* nn, sizeof(double));
    double* B = (double*) calloc(nn* nn, sizeof(double));
    double* C = (double*) calloc(nn* nn, sizeof(double));
    if (NULL == A || NULL == B || NULL == C) {
        std::cout << "Fatal Error. Cannot allocate matrices A, B, and C."
            << std::endl;
        return 1;
    }

    // Populate B and C pseudo-randomly - 
    // The matrices are populated with random numbers in the range (-1.0, +1.0)
    cilk_for(int i = 0; i < nn * nn; ++i) {
        B[i] = (float) ((i * i) % 1024 - 512) / 512;
    }
    cilk_for(int i = 0; i < nn * nn; ++i) {
        C[i] = (float) (((i + 1) * i) % 1024 - 512) / 512;
    }

    // Multiply to get A = B*C 
    unsigned long long start_tick, end_tick;
    long elapsed_milliseconds;

    start_tick = cilk_getticks();
    matrix_multiply(A, B, C, (unsigned int)nn);
    end_tick = cilk_getticks();

    float par_time = cilk_ticks_to_seconds(end_tick - start_tick); // (end_tick - start_tick) / 1000.0F;
    std::cout << " Matrix Multiply took " << par_time << " seconds."
        << std::endl;

    // If n is small, print the results
    if (nn <= 8) {
        std::cout << "Matrix A:" << std::endl;
        print_matrix(B, nn);
        std::cout << std::endl << "Matrix B:" << std::endl;
        print_matrix(C, nn);
        std::cout << std::endl << "Matrix C = A * B:" << std::endl;
        print_matrix(A, nn);
    }

    free(A);
    free(B);
    free(C);
    return 0;
}
