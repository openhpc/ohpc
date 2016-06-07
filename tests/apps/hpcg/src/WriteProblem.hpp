
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

#ifndef WRITEPROBLEM_HPP
#define WRITEPROBLEM_HPP
#include "Geometry.hpp"
#include "SparseMatrix.hpp"

int WriteProblem( const Geometry & geom, const SparseMatrix & A, const Vector b, const Vector x, const Vector xexact);
#endif // WRITEPROBLEM_HPP
