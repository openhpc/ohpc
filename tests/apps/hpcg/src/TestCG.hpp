
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

/*!
 @file TestCG.hpp

 HPCG data structure
 */

#ifndef TESTCG_HPP
#define TESTCG_HPP

#include "hpcg.hpp"
#include "SparseMatrix.hpp"
#include "Vector.hpp"
#include "CGData.hpp"


struct TestCGData_STRUCT {
  int count_pass; //!< number of succesful tests
  int count_fail;  //!< number of succesful tests
  int expected_niters_no_prec; //!< expected number of test CG iterations without preconditioning with diagonally dominant matrix (~12)
  int expected_niters_prec; //!< expected number of test CG iterations with preconditioning and with diagonally dominant matrix (~1-2)
  int niters_max_no_prec; //!< maximum number of test CG iterations without predictitioner
  int niters_max_prec; //!< maximum number of test CG iterations without predictitioner
  double normr; //!< residual norm achieved during test CG iterations
};
typedef struct TestCGData_STRUCT TestCGData;

extern int TestCG(SparseMatrix & A, CGData & data, Vector & b, Vector & x, TestCGData & testcg_data);

#endif  // TESTCG_HPP

