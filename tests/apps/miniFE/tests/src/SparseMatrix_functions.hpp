#ifndef _SparseMatrix_functions_hpp_
#define _SparseMatrix_functions_hpp_

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
#include <vector>
#include <set>
#include <algorithm>
#include <sstream>
#include <fstream>

#include <Vector.hpp>
#include <Vector_functions.hpp>
#include <ElemData.hpp>
#include <MatrixInitOp.hpp>
#include <MatrixCopyOp.hpp>
#include <exchange_externals.hpp>
#include <mytimer.hpp>

#ifdef MINIFE_HAVE_TBB
#include <LockingMatrix.hpp>
#endif

#ifdef HAVE_MPI
#include <mpi.h>
#endif

namespace miniFE {

template<typename MatrixType>
void init_matrix(MatrixType& M,
                 const std::vector<typename MatrixType::GlobalOrdinalType>& rows,
                 const std::vector<typename MatrixType::LocalOrdinalType>& row_offsets,
                 const std::vector<int>& row_coords,
                 int global_nodes_x,
                 int global_nodes_y,
                 int global_nodes_z,
                 typename MatrixType::GlobalOrdinalType global_nrows,
                 const simple_mesh_description<typename MatrixType::GlobalOrdinalType>& mesh)
{
  MatrixInitOp<MatrixType> mat_init(rows, row_offsets, row_coords,
                                 global_nodes_x, global_nodes_y, global_nodes_z,
                                 global_nrows, mesh, M);

  for(int i=0; i<mat_init.n; ++i) {
    mat_init(i);
  }
}

template<typename T,
         typename U>
void sort_with_companions(ptrdiff_t len, T* array, U* companions)
{
  ptrdiff_t i, j, index;
  U companion;

  for (i=1; i < len; i++) {
    index = array[i];
    companion = companions[i];
    j = i;
    while ((j > 0) && (array[j-1] > index))
    {
      array[j] = array[j-1];
      companions[j] = companions[j-1];
      j = j - 1;
    }
    array[j] = index;
    companions[j] = companion;
  }
}

template<typename MatrixType>
void write_matrix(const std::string& filename, 
                  MatrixType& mat)
{
  typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename MatrixType::ScalarType ScalarType;

  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  std::ostringstream osstr;
  osstr << filename << "." << numprocs << "." << myproc;
  std::string full_name = osstr.str();
  std::ofstream ofs(full_name.c_str());

  size_t nrows = mat.rows.size();
  size_t nnz = mat.num_nonzeros();

  for(int p=0; p<numprocs; ++p) {
    if (p == myproc) {
      if (p == 0) {
        ofs << nrows << " " << nnz << std::endl;
      }
      for(size_t i=0; i<nrows; ++i) {
        size_t row_len = 0;
        GlobalOrdinalType* cols = NULL;
        ScalarType* coefs = NULL;
        mat.get_row_pointers(mat.rows[i], row_len, cols, coefs);

        for(size_t j=0; j<row_len; ++j) {
          ofs << mat.rows[i] << " " << cols[j] << " " << coefs[j] << std::endl;
        }
      }
    }
#ifdef HAVE_MPI
    MPI_Barrier(MPI_COMM_WORLD);
#endif
  }
}

template<typename GlobalOrdinal,typename Scalar>
void
sum_into_row(int row_len,
             GlobalOrdinal* row_indices,
             Scalar* row_coefs,
             int num_inputs,
             const GlobalOrdinal* input_indices,
             const Scalar* input_coefs)
{
  for(size_t i=0; i<num_inputs; ++i) {
    GlobalOrdinal* loc = std::lower_bound(row_indices, row_indices+row_len,
                                          input_indices[i]);
    if (loc-row_indices < row_len && *loc == input_indices[i]) {
//if(flag && *loc==6)
//std::cout<<"  ("<<*loc<<":"<<row_coefs[loc-row_indices]<<" += "<<input_coefs[i]<<")"<<std::endl;
      #pragma omp atomic
      row_coefs[loc-row_indices] += input_coefs[i];
    }
  }
}

template<typename MatrixType>
void
sum_into_row(typename MatrixType::GlobalOrdinalType row,
             size_t num_indices,
             const typename MatrixType::GlobalOrdinalType* col_inds,
             const typename MatrixType::ScalarType* coefs,
             MatrixType& mat)
{
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

  size_t row_len = 0;
  GlobalOrdinal* mat_row_cols = NULL;
  Scalar* mat_row_coefs = NULL;

  mat.get_row_pointers(row, row_len, mat_row_cols, mat_row_coefs);
  if (row_len == 0) return;

  sum_into_row(row_len, mat_row_cols, mat_row_coefs, num_indices, col_inds, coefs);
}

template<typename MatrixType>
void
sum_in_symm_elem_matrix(size_t num,
                   const typename MatrixType::GlobalOrdinalType* indices,
                   const typename MatrixType::ScalarType* coefs,
                   MatrixType& mat)
{
  typedef typename MatrixType::ScalarType Scalar;
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;

//indices is length num (which should be nodes-per-elem)
//coefs is the upper triangle of the element diffusion matrix
//which should be length num*(num+1)/2
//std::cout<<std::endl;

  int row_offset = 0;
  bool flag = false;
  for(size_t i=0; i<num; ++i) {
    GlobalOrdinal row = indices[i];
 
    const Scalar* row_coefs = &coefs[row_offset];
    const GlobalOrdinal* row_col_inds = &indices[i];
    size_t row_len = num - i;
    row_offset += row_len;

    size_t mat_row_len = 0;
    GlobalOrdinal* mat_row_cols = NULL;
    Scalar* mat_row_coefs = NULL;
  
    mat.get_row_pointers(row, mat_row_len, mat_row_cols, mat_row_coefs);
    if (mat_row_len == 0) continue;

    sum_into_row(mat_row_len, mat_row_cols, mat_row_coefs,
                 row_len, row_col_inds, row_coefs);

    int offset = i;
    for(size_t j=0; j<i; ++j) {
      Scalar coef = coefs[offset];
//std::cout<<"i: "<<i<<", j: "<<j<<", offset: "<<offset<<std::endl;
      sum_into_row(mat_row_len, mat_row_cols, mat_row_coefs,
                   1, &indices[j], &coef);
      offset += num - (j+1);
    }
  }
}

template<typename MatrixType>
void
sum_in_elem_matrix(size_t num,
                   const typename MatrixType::GlobalOrdinalType* indices,
                   const typename MatrixType::ScalarType* coefs,
                   MatrixType& mat)
{
  size_t offset = 0;

  for(size_t i=0; i<num; ++i) {
    sum_into_row(indices[i], num,
                 &indices[0], &coefs[offset], mat);
    offset += num;
  }
}

template<typename GlobalOrdinal, typename Scalar,
         typename MatrixType, typename VectorType>
void
sum_into_global_linear_system(ElemData<GlobalOrdinal,Scalar>& elem_data,
                              MatrixType& A, VectorType& b)
{
  sum_in_symm_elem_matrix(elem_data.nodes_per_elem, elem_data.elem_node_ids,
                     elem_data.elem_diffusion_matrix, A);
  sum_into_vector(elem_data.nodes_per_elem, elem_data.elem_node_ids,
                  elem_data.elem_source_vector, b);
}

#ifdef MINIFE_HAVE_TBB
template<typename MatrixType>
void
sum_in_elem_matrix(size_t num,
                   const typename MatrixType::GlobalOrdinalType* indices,
                   const typename MatrixType::ScalarType* coefs,
                   LockingMatrix<MatrixType>& mat)
{
  size_t offset = 0;

  for(size_t i=0; i<num; ++i) {
    mat.sum_in(indices[i], num, &indices[0], &coefs[offset]);
    offset += num;
  }
}

template<typename GlobalOrdinal, typename Scalar,
         typename MatrixType, typename VectorType>
void
sum_into_global_linear_system(ElemData<GlobalOrdinal,Scalar>& elem_data,
                              LockingMatrix<MatrixType>& A, LockingVector<VectorType>& b)
{
  sum_in_elem_matrix(elem_data.nodes_per_elem, elem_data.elem_node_ids,
                     elem_data.elem_diffusion_matrix, A);
  sum_into_vector(elem_data.nodes_per_elem, elem_data.elem_node_ids,
                  elem_data.elem_source_vector, b);
}
#endif

template<typename MatrixType>
void
add_to_diagonal(typename MatrixType::ScalarType value, MatrixType& mat)
{
  for(size_t i=0; i<mat.rows.size(); ++i) {
    sum_into_row(mat.rows[i], 1, &mat.rows[i], &value, mat);
  }
}

template<typename MatrixType>
double
parallel_memory_overhead_MB(const MatrixType& A)
{
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinal;
  double mem_MB = 0;

#ifdef HAVE_MPI
  double invMB = 1.0/(1024*1024);
  mem_MB = invMB*A.external_index.size()*sizeof(GlobalOrdinal);
  mem_MB += invMB*A.external_local_index.size()*sizeof(GlobalOrdinal);
  mem_MB += invMB*A.elements_to_send.size()*sizeof(GlobalOrdinal);
  mem_MB += invMB*A.neighbors.size()*sizeof(int);
  mem_MB += invMB*A.recv_length.size()*sizeof(LocalOrdinal);
  mem_MB += invMB*A.send_length.size()*sizeof(LocalOrdinal);

  double tmp = mem_MB;
  MPI_Allreduce(&tmp, &mem_MB, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
#endif

  return mem_MB;
}

template<typename MatrixType>
void rearrange_matrix_local_external(MatrixType& A)
{
  //This function will rearrange A so that local entries are contiguous at the front
  //of A's memory, and external entries are contiguous at the back of A's memory.
  //
  //A.row_offsets will describe where the local entries occur, and
  //A.row_offsets_external will describe where the external entries occur.

  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

  size_t nrows = A.rows.size();
  std::vector<LocalOrdinal> tmp_row_offsets(nrows*2);
  std::vector<LocalOrdinal> tmp_row_offsets_external(nrows*2);

  LocalOrdinal num_local_nz = 0;
  LocalOrdinal num_extern_nz = 0;

  //First sort within each row of A, so that local entries come
  //before external entries within each row.
  //tmp_row_offsets describe the locations of the local entries, and
  //tmp_row_offsets_external describe the locations of the external entries.
  //
  for(size_t i=0; i<nrows; ++i) {
    GlobalOrdinal* row_begin = &A.packed_cols[A.row_offsets[i]];
    GlobalOrdinal* row_end = &A.packed_cols[A.row_offsets[i+1]];

    Scalar* coef_row_begin = &A.packed_coefs[A.row_offsets[i]];

    tmp_row_offsets[i*2] = A.row_offsets[i];
    tmp_row_offsets[i*2+1] = A.row_offsets[i+1];
    tmp_row_offsets_external[i*2] = A.row_offsets[i+1];
    tmp_row_offsets_external[i*2+1] = A.row_offsets[i+1];

    ptrdiff_t row_len = row_end - row_begin;

    sort_with_companions(row_len, row_begin, coef_row_begin);

    GlobalOrdinal* row_iter = std::lower_bound(row_begin, row_end, nrows);

    LocalOrdinal offset = A.row_offsets[i] + row_iter-row_begin;
    tmp_row_offsets[i*2+1] = offset;
    tmp_row_offsets_external[i*2] = offset;

    num_local_nz += tmp_row_offsets[i*2+1]-tmp_row_offsets[i*2];
    num_extern_nz += tmp_row_offsets_external[i*2+1]-tmp_row_offsets_external[i*2];
  }

  //Next, copy the external entries into separate arrays.

  std::vector<GlobalOrdinal> ext_cols(num_extern_nz);
  std::vector<Scalar> ext_coefs(num_extern_nz);
  std::vector<LocalOrdinal> ext_offsets(nrows+1);
  LocalOrdinal offset = 0;
  for(size_t i=0; i<nrows; ++i) {
    ext_offsets[i] = offset;
    for(LocalOrdinal j=tmp_row_offsets_external[i*2];
                     j<tmp_row_offsets_external[i*2+1]; ++j) {
      ext_cols[offset] = A.packed_cols[j];
      ext_coefs[offset++] = A.packed_coefs[j];
    }
  }
  ext_offsets[nrows] = offset;

  //Now slide all local entries down to the beginning of A's packed arrays

  A.row_offsets.resize(nrows+1);
  offset = 0;
  for(size_t i=0; i<nrows; ++i) {
    A.row_offsets[i] = offset;
    for(LocalOrdinal j=tmp_row_offsets[i*2]; j<tmp_row_offsets[i*2+1]; ++j) {
      A.packed_cols[offset] = A.packed_cols[j];
      A.packed_coefs[offset++] = A.packed_coefs[j];
    }
  }
  A.row_offsets[nrows] = offset;

  //Finally, copy the external entries back into A.packed_cols and
  //A.packed_coefs, starting at the end of the local entries.

  for(LocalOrdinal i=offset; i<offset+ext_cols.size(); ++i) {
    A.packed_cols[i] = ext_cols[i-offset];
    A.packed_coefs[i] = ext_coefs[i-offset];
  }

  A.row_offsets_external.resize(nrows+1);
  for(size_t i=0; i<=nrows; ++i) A.row_offsets_external[i] = ext_offsets[i] + offset;
}

//------------------------------------------------------------------------
template<typename MatrixType>
void
zero_row_and_put_1_on_diagonal(MatrixType& A, typename MatrixType::GlobalOrdinalType row)
{
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

  size_t row_len = 0;
  GlobalOrdinal* cols = NULL;
  Scalar* coefs = NULL;
  A.get_row_pointers(row, row_len, cols, coefs);
  
  for(size_t i=0; i<row_len; ++i) {
    if (cols[i] == row) coefs[i] = 1;
    else coefs[i] = 0;
  }
}

//------------------------------------------------------------------------
template<typename MatrixType,
         typename VectorType>
void
impose_dirichlet(typename MatrixType::ScalarType prescribed_value,
                    MatrixType& A,
                    VectorType& b,
                    int global_nx,
                    int global_ny,
                    int global_nz,
                    const std::set<typename MatrixType::GlobalOrdinalType>& bc_rows)
{
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

  GlobalOrdinal first_local_row = A.rows.size()>0 ? A.rows[0] : 0;
  GlobalOrdinal last_local_row  = A.rows.size()>0 ? A.rows[A.rows.size()-1] : -1;

  typename std::set<GlobalOrdinal>::const_iterator
    bc_iter = bc_rows.begin(), bc_end = bc_rows.end();
  for(; bc_iter!=bc_end; ++bc_iter) {
    GlobalOrdinal row = *bc_iter;
    if (row >= first_local_row && row <= last_local_row) {
      size_t local_row = row - first_local_row;
      b.coefs[local_row] = prescribed_value;
      zero_row_and_put_1_on_diagonal(A, row);
    }
  }

  const int ROW_COUNT = A.rows.size();

  #pragma omp parallel for
  for(MINIFE_GLOBAL_ORDINAL i=0; i < ROW_COUNT; ++i) {
    GlobalOrdinal row = A.rows[i];

    if (bc_rows.find(row) != bc_rows.end()) continue;

    size_t row_length = 0;
    GlobalOrdinal* cols = NULL;
    Scalar* coefs = NULL;
    A.get_row_pointers(row, row_length, cols, coefs);

    Scalar sum = 0;
    for(size_t j=0; j<row_length; ++j) {
      if (bc_rows.find(cols[j]) != bc_rows.end()) {
        sum += coefs[j];
        coefs[j] = 0;
      }
    }

    #pragma omp atomic
    b.coefs[i] -= sum*prescribed_value;
  }
}

static timer_type exchtime = 0;

//------------------------------------------------------------------------
//Compute matrix vector product y = A*x where:
//
// A - input matrix
// x - input vector
// y - result vector
//
#if defined(MINIFE_CSR_MATRIX)
template<typename MatrixType,
         typename VectorType>
struct matvec_std {
void operator()(MatrixType& A,
            VectorType& x,
            VectorType& y)
{
  	exchange_externals(A, x);

  	typedef typename MatrixType::ScalarType ScalarType;
  	typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  	typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;

        const MINIFE_GLOBAL_ORDINAL rows_size     = A.rows.size();
        const LocalOrdinalType* const Arowoffsets = &A.row_offsets[0];
        const GlobalOrdinalType* const Acols      = &A.packed_cols[0];
        const ScalarType* const Acoefs            = &A.packed_coefs[0];
        const ScalarType* const xcoefs __attribute__((aligned(64))) = &x.coefs[0];
        ScalarType* ycoefs __attribute__((aligned(64))) = &y.coefs[0];

        #pragma omp parallel for
        for(MINIFE_GLOBAL_ORDINAL row = 0; row < rows_size; ++row) {
                const MINIFE_GLOBAL_ORDINAL row_start = Arowoffsets[row];
                const MINIFE_GLOBAL_ORDINAL row_end   = Arowoffsets[row+1];

                MINIFE_SCALAR sum = 0;

                #pragma loop_count(15)
		#pragma vector nontemporal
                for(MINIFE_GLOBAL_ORDINAL i = row_start; i < row_end; ++i) {
                        sum += Acoefs[i] * xcoefs[Acols[i]];
                }

                ycoefs[row] = sum;
        }
}
};
#elif defined(MINIFE_ELL_MATRIX)
template<typename MatrixType,
         typename VectorType>
struct matvec_std {
void operator()(MatrixType& A,
            VectorType& x,
            VectorType& y)
{
  exchange_externals(A, x);

  typedef typename MatrixType::ScalarType ScalarType;
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;

  int row_len = A.num_cols_per_row;
  int n = A.rows.size();
  const GlobalOrdinalType* Acols      = &A.cols[0];
  const ScalarType* Acoefs            = &A.coefs[0];
  const ScalarType* xcoefs = &x.coefs[0];
        ScalarType* ycoefs = &y.coefs[0];
  ScalarType beta = 0;

  #pragma omp parallel for
  for(int row=0; row<n; ++row) {
    ScalarType sum = beta*ycoefs[row];

    int row_start=row*row_len;
    int row_end=row_start+row_len;

    for(LocalOrdinalType i=row_start; i<row_end; ++i) {
      sum += Acoefs[i]*xcoefs[Acols[i]];
    }

    ycoefs[row] = sum;
  }
}
};
#endif

#if defined(MINIFE_CSR_MATRIX)
template<typename MatrixType,
         typename VectorType>
struct matvec_std_nowait {
void operator()(MatrixType& A,
            VectorType& x,
            VectorType& y)
{
  	exchange_externals(A, x);

  	typedef typename MatrixType::ScalarType ScalarType;
  	typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  	typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;

        const MINIFE_GLOBAL_ORDINAL rows_size     = A.rows.size();
        const LocalOrdinalType* const Arowoffsets = &A.row_offsets[0];
        const GlobalOrdinalType* const Acols      = &A.packed_cols[0];
        const ScalarType* const Acoefs            = &A.packed_coefs[0];
        const ScalarType* const xcoefs            = &x.coefs[0];
        ScalarType* ycoefs                        = &y.coefs[0];
        const ScalarType beta                     = 0;

        #pragma omp parallel for
        for(MINIFE_GLOBAL_ORDINAL row = 0; row < rows_size; ++row) {
                const MINIFE_GLOBAL_ORDINAL row_start = Arowoffsets[row];
                const MINIFE_GLOBAL_ORDINAL row_end   = Arowoffsets[row+1];

                MINIFE_SCALAR sum = 0;

                #pragma loop_count(15)
                for(MINIFE_GLOBAL_ORDINAL i = row_start; i < row_end; ++i) {
                        sum += Acoefs[i] * xcoefs[Acols[i]];
                }

                ycoefs[row] = sum;
        }
}
};
#elif defined(MINIFE_ELL_MATRIX)
template<typename MatrixType,
         typename VectorType>
struct matvec_std_nowait {
void operator()(MatrixType& A,
            VectorType& x,
            VectorType& y)
{
  exchange_externals(A, x);

  typedef typename MatrixType::ScalarType ScalarType;
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;

  int row_len = A.num_cols_per_row;
  int n = A.rows.size();
  const GlobalOrdinalType* Acols      = &A.cols[0];
  const ScalarType* Acoefs            = &A.coefs[0];
  const ScalarType* xcoefs = &x.coefs[0];
        ScalarType* ycoefs = &y.coefs[0];
  ScalarType beta = 0;

  #pragma omp parallel for
  for(int row=0; row<n; ++row) {
    ScalarType sum = beta*ycoefs[row];

    int row_start=row*row_len;
    int row_end=row_start+row_len;

    for(LocalOrdinalType i=row_start; i<row_end; ++i) {
      sum += Acoefs[i]*xcoefs[Acols[i]];
    }

    ycoefs[row] = sum;
  }
}
};
#endif

template<typename MatrixType,
         typename VectorType>
void matvec_nowait(MatrixType& A, VectorType& x, VectorType& y)
{
  matvec_std_nowait<MatrixType,VectorType> mv;
  mv(A, x, y);
}

template<typename MatrixType,
         typename VectorType>
void matvec(MatrixType& A, VectorType& x, VectorType& y)
{
  matvec_std<MatrixType,VectorType> mv;
  mv(A, x, y);
}

template<typename MatrixType,
         typename VectorType>
struct matvec_overlap {
void operator()(MatrixType& A,
                    VectorType& x,
                    VectorType& y)
{
#ifdef HAVE_MPI
  begin_exchange_externals(A, x);
#endif

  typedef typename MatrixType::ScalarType ScalarType;
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinalType;


  int n = A.rows.size();
  const LocalOrdinalType* Arowoffsets = &A.row_offsets[0];
  const GlobalOrdinalType* Acols      = &A.packed_cols[0];
  const ScalarType* Acoefs            = &A.packed_coefs[0];
  const ScalarType* xcoefs = &x.coefs[0];
        ScalarType* ycoefs = &y.coefs[0];
  ScalarType beta = 0;

  for(int row=0; row<n; ++row) {
    ScalarType sum = beta*ycoefs[row];

    for(LocalOrdinalType i=Arowoffsets[row]; i<Arowoffsets[row+1]; ++i) {
      sum += Acoefs[i]*xcoefs[Acols[i]];
    }

    ycoefs[row] = sum;
  }

#ifdef HAVE_MPI
  finish_exchange_externals(A.neighbors.size());

  Arowoffsets = &A.row_offsets_external[0];
  beta = 1;

  for(int row=0; row<n; ++row) {
    ScalarType sum = beta*ycoefs[row];

    for(LocalOrdinalType i=Arowoffsets[row]; i<Arowoffsets[row+1]; ++i) {
      sum += Acoefs[i]*xcoefs[Acols[i]];
    }

    ycoefs[row] = sum;
  }
#endif
}
};

}//namespace miniFE

#endif

