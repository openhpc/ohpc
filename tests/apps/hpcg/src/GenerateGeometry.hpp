
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

#ifndef GENERATEGEOMETRY_HPP
#define GENERATEGEOMETRY_HPP
#include "Geometry.hpp"
void GenerateGeometry(int size, int rank, int numThreads, int nx, int ny, int nz, Geometry * geom);
#endif // GENERATEGEOMETRY_HPP
