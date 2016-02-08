
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

#ifndef GENERATEPROBLEM_REF_HPP
#define GENERATEPROBLEM_REF_HPP
#include "SparseMatrix.hpp"
#include "Vector.hpp"

void GenerateProblem_ref(SparseMatrix & A, Vector * b, Vector * x, Vector * xexact);
#endif // GENERATEPROBLEM_REF_HPP
