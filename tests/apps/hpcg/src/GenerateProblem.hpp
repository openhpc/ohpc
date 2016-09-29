
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

#ifndef GENERATEPROBLEM_HPP
#define GENERATEPROBLEM_HPP
#include "SparseMatrix.hpp"
#include "Vector.hpp"

void GenerateProblem(SparseMatrix & A, Vector * b, Vector * x, Vector * xexact);
#endif // GENERATEPROBLEM_HPP
