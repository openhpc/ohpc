
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

#ifndef OPTIMIZEPROBLEM_HPP
#define OPTIMIZEPROBLEM_HPP

#include "SparseMatrix.hpp"
#include "Vector.hpp"
#include "CGData.hpp"

int OptimizeProblem(SparseMatrix & A, CGData & data,  Vector & b, Vector & x, Vector & xexact);

// This helper function should be implemented in a non-trivial way if OptimizeProblem is non-trivial
// It should return as type double, the total number of bytes allocated and retained after calling OptimizeProblem.
// This value will be used to report Gbytes used in ReportResults (the value returned will be divided by 1000000000.0).

double OptimizeProblemMemoryUse(const SparseMatrix & A);

#endif  // OPTIMIZEPROBLEM_HPP
