
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

#ifndef CG_HPP
#define CG_HPP

#include "SparseMatrix.hpp"
#include "Vector.hpp"
#include "CGData.hpp"

int CG(const SparseMatrix & A, CGData & data, const Vector & b, Vector & x,
    const int max_iter, const double tolerance, int & niters, double & normr,  double & normr0,
    double * times, bool doPreconditioning);

// this function will compute the Conjugate Gradient iterations.
// geom - Domain and processor topology information
// A - Matrix
// b - constant
// x - used for return value
// max_iter - how many times we iterate
// tolerance - Stopping tolerance for preconditioned iterations.
// niters - number of iterations performed
// normr - computed residual norm
// normr0 - Original residual
// times - array of timing information
// doPreconditioning - bool to specify whether or not symmetric GS will be applied.

#endif  // CG_HPP
