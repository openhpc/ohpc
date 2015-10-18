
//@HEADER
// ***************************************************
//
// HPCG: High Performance Conjugate Gradient Benchmark
//
// Contact:
// Michael A. Heroux ( maherou@sandia.gov)
// Jack Dongarra     (dongarra@eecs.utk.edu)
// Piotr Luszczek    (luszczek@eecs.utk.edu)
//
// ***************************************************
//@HEADER

#ifndef COMPUTEDOTPRODUCT_REF_HPP
#define COMPUTEDOTPRODUCT_REF_HPP
#include "Vector.hpp"
int ComputeDotProduct_ref(const local_int_t n, const Vector & x, const Vector & y,
    double & result, double & time_allreduce);

#endif // COMPUTEDOTPRODUCT_REF_HPP
