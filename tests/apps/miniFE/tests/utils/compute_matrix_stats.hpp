#ifndef _compute_matrix_stats_hpp_
#define _compute_matrix_stats_hpp_

//@HEADER
// ************************************************************************
//
// MiniFE: Simple Finite Element Assembly and Solve
// Copyright (2006-2013) Sandia Corporation
//
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
//
// ************************************************************************
//@HEADER

#include <cstddef>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <iomanip>

#include <outstream.hpp>
#include <utils.hpp>
#include <YAML_Doc.hpp>

namespace miniFE {

template<typename MatrixType>
size_t
compute_matrix_stats(const MatrixType& A, int myproc, int numprocs, YAML_Doc& ydoc)
{
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

  GlobalOrdinal min_nrows = 0, max_nrows = 0, global_nrows = 0;
  int min_proc = 0, max_proc = 0;

  GlobalOrdinal local_nrows = A.rows.size();

  get_global_min_max(local_nrows, global_nrows, min_nrows, min_proc,
                     max_nrows, max_proc);

  //Gather stats on global, min/max matrix num-nonzeros:

  double local_nnz = A.num_nonzeros();
  double dglobal_nnz = 0, dmin_nnz = 0, dmax_nnz = 0;

  get_global_min_max(local_nnz, dglobal_nnz, dmin_nnz, min_proc,
                     dmax_nnz, max_proc);

  double avg_nrows = global_nrows;
  avg_nrows /= numprocs;
  double avg_nnz = dglobal_nnz;
  avg_nnz /= numprocs;

  double mem_overhead_MB = parallel_memory_overhead_MB(A);

  size_t global_nnz = static_cast<size_t>(std::ceil(dglobal_nnz));
  size_t min_nnz = static_cast<size_t>(std::ceil(dmin_nnz));
  size_t max_nnz = static_cast<size_t>(std::ceil(dmax_nnz));
  size_t global_num_rows = global_nrows;

  if (myproc == 0) {
    ydoc.add("Matrix attributes","");
    ydoc.get("Matrix attributes")->add("Global Nrows",global_num_rows);
    ydoc.get("Matrix attributes")->add("Global NNZ",global_nnz);

    //compute how much memory the matrix occupies:
    //num-bytes = sizeof(GlobalOrdinal)*global_nrows   for A.rows
    //          + sizeof(LocalOrdinal)*global_nrows    for A.rows_offsets
    //          + sizeof(GlobalOrdinal)*global_nnz     for A.packed_cols
    //          + sizeof(Scalar)*global_nnz            for A.packed_coefs

    double invGB = 1.0/(1024*1024*1024);
    double memGB = invGB*global_nrows*sizeof(GlobalOrdinal);
    memGB += invGB*global_nrows*sizeof(LocalOrdinal);
    memGB += invGB*global_nnz*sizeof(GlobalOrdinal);
    memGB += invGB*global_nnz*sizeof(Scalar);
    ydoc.get("Matrix attributes")->add("Global Memory (GB)",memGB);

    ydoc.get("Matrix attributes")->add("Pll Memory Overhead (MB)",mem_overhead_MB);

    size_t min_num_rows = min_nrows;
    size_t max_num_rows = max_nrows;
    ydoc.get("Matrix attributes")->add("Rows per proc MIN",min_num_rows);
    ydoc.get("Matrix attributes")->add("Rows per proc MAX",max_num_rows);
    ydoc.get("Matrix attributes")->add("Rows per proc AVG",avg_nrows);
    ydoc.get("Matrix attributes")->add("NNZ per proc MIN",min_nnz);
    ydoc.get("Matrix attributes")->add("NNZ per proc MAX",max_nnz);
    ydoc.get("Matrix attributes")->add("NNZ per proc AVG",avg_nnz);
  }

  return global_nnz;
}

}//namespace miniFE

#endif

