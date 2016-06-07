
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

#ifndef _MatrixInitOp_hpp_
#define _MatrixInitOp_hpp_

#include <simple_mesh_description.hpp>
#include <box_utils.hpp>

#include <CSRMatrix.hpp>
#include <ELLMatrix.hpp>

#include <algorithm>

template<typename GlobalOrdinal>
void sort_if_needed(GlobalOrdinal* list,
                    GlobalOrdinal list_len)
{
  bool need_to_sort = false;
  for(GlobalOrdinal i=list_len-1; i>=1; --i) {
    if (list[i] < list[i-1]) {
      need_to_sort = true;
      break;
    }
  }

  if (need_to_sort) {
    std::sort(list,list+list_len);
  }
}

template<typename MatrixType>
struct MatrixInitOp {
};

template<>
struct MatrixInitOp<miniFE::CSRMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL> > {
  MatrixInitOp(const std::vector<MINIFE_GLOBAL_ORDINAL>& rows_vec,
               const std::vector<MINIFE_LOCAL_ORDINAL>& row_offsets_vec,
               const std::vector<int>& row_coords_vec,
               int global_nx, int global_ny, int global_nz,
               MINIFE_GLOBAL_ORDINAL global_n_rows,
               const miniFE::simple_mesh_description<MINIFE_GLOBAL_ORDINAL>& input_mesh,
               miniFE::CSRMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL>& matrix)
   : rows(&rows_vec[0]),
     row_offsets(&row_offsets_vec[0]),
     row_coords(&row_coords_vec[0]),
     global_nodes_x(global_nx),
     global_nodes_y(global_ny),
     global_nodes_z(global_nz),
     global_nrows(global_n_rows),
     mesh(&input_mesh),
     dest_rows(&matrix.rows[0]),
     dest_rowoffsets(&matrix.row_offsets[0]),
     dest_cols(&matrix.packed_cols[0]),
     dest_coefs(&matrix.packed_coefs[0]),
     n(matrix.rows.size())
  {
    if (matrix.packed_cols.capacity() != matrix.packed_coefs.capacity()) {
      std::cout<<"Warning, packed_cols.capacity ("<<matrix.packed_cols.capacity()<<") != "
        << "packed_coefs.capacity ("<<matrix.packed_coefs.capacity()<<")"<<std::endl;
    }

    size_t nnz = row_offsets_vec[n];
    if (matrix.packed_cols.capacity() < nnz) {
      std::cout<<"Warning, packed_cols.capacity ("<<matrix.packed_cols.capacity()<<") < "
        " nnz ("<<nnz<<")"<<std::endl;
    }

    matrix.packed_cols.resize(nnz);
    matrix.packed_coefs.resize(nnz);
    dest_rowoffsets[n] = nnz;
  }

  typedef MINIFE_GLOBAL_ORDINAL GlobalOrdinalType;
  typedef MINIFE_LOCAL_ORDINAL LocalOrdinalType;
  typedef MINIFE_SCALAR ScalarType;

  const GlobalOrdinalType* rows;
  const LocalOrdinalType*  row_offsets;
  const int*               row_coords;

  int global_nodes_x;
  int global_nodes_y;
  int global_nodes_z;

  GlobalOrdinalType global_nrows;

  GlobalOrdinalType* dest_rows;
  LocalOrdinalType*  dest_rowoffsets;
  GlobalOrdinalType* dest_cols;
  ScalarType*        dest_coefs;
  int n;

  const miniFE::simple_mesh_description<GlobalOrdinalType>* mesh;

  inline void operator()(int i)
  {
    dest_rows[i] = rows[i];
    int offset = row_offsets[i];
    dest_rowoffsets[i] = offset;
    int ix = row_coords[i*3];
    int iy = row_coords[i*3+1];
    int iz = row_coords[i*3+2];
    GlobalOrdinalType nnz = 0;
    for(int sz=-1; sz<=1; ++sz) {
      for(int sy=-1; sy<=1; ++sy) {
        for(int sx=-1; sx<=1; ++sx) {
          GlobalOrdinalType col_id =
              miniFE::get_id<GlobalOrdinalType>(global_nodes_x, global_nodes_y, global_nodes_z,
                                   ix+sx, iy+sy, iz+sz);
          if (col_id >= 0 && col_id < global_nrows) {
            GlobalOrdinalType col = mesh->map_id_to_row(col_id);
            dest_cols[offset+nnz] = col;
            dest_coefs[offset+nnz] = 0;
            ++nnz;
          }
        }
      }
    }

    sort_if_needed(&dest_cols[offset], nnz);
  }
};

template<>
struct MatrixInitOp<miniFE::ELLMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL> > {
  MatrixInitOp(const std::vector<MINIFE_GLOBAL_ORDINAL>& rows_vec,
               const std::vector<MINIFE_LOCAL_ORDINAL>& /*row_offsets_vec*/,
               const std::vector<int>& row_coords_vec,
               int global_nx, int global_ny, int global_nz,
               MINIFE_GLOBAL_ORDINAL global_n_rows,
               const miniFE::simple_mesh_description<MINIFE_GLOBAL_ORDINAL>& input_mesh,
               miniFE::ELLMatrix<MINIFE_SCALAR,MINIFE_LOCAL_ORDINAL,MINIFE_GLOBAL_ORDINAL>& matrix)
   : rows(&rows_vec[0]),
     row_coords(&row_coords_vec[0]),
     global_nodes_x(global_nx),
     global_nodes_y(global_ny),
     global_nodes_z(global_nz),
     global_nrows(global_n_rows),
     mesh(&input_mesh),
     dest_rows(&matrix.rows[0]),
     dest_cols(&matrix.cols[0]),
     dest_coefs(&matrix.coefs[0]),
     n(matrix.rows.size()),
     ncols_per_row(matrix.num_cols_per_row)
  {
  }

  typedef MINIFE_GLOBAL_ORDINAL GlobalOrdinalType;
  typedef MINIFE_LOCAL_ORDINAL LocalOrdinalType;
  typedef MINIFE_SCALAR ScalarType;

  const GlobalOrdinalType* rows;
  const int*               row_coords;

  int global_nodes_x;
  int global_nodes_y;
  int global_nodes_z;

  GlobalOrdinalType global_nrows;

  GlobalOrdinalType* dest_rows;
  GlobalOrdinalType* dest_cols;
  ScalarType*        dest_coefs;
  int n;
  int ncols_per_row;

  const miniFE::simple_mesh_description<GlobalOrdinalType>* mesh;

  inline void operator()(int i)
  {
    dest_rows[i] = rows[i];
    int offset = i*ncols_per_row;
    int ix = row_coords[i*3];
    int iy = row_coords[i*3+1];
    int iz = row_coords[i*3+2];
    GlobalOrdinalType nnz = 0;
    for(int sz=-1; sz<=1; ++sz)
      for(int sy=-1; sy<=1; ++sy)
        for(int sx=-1; sx<=1; ++sx) {
          GlobalOrdinalType col_id =
              miniFE::get_id<GlobalOrdinalType>(global_nodes_x, global_nodes_y, global_nodes_z,
                                   ix+sx, iy+sy, iz+sz);
          if (col_id >= 0 && col_id < global_nrows) {
            GlobalOrdinalType col = mesh->map_id_to_row(col_id);
            dest_cols[offset+nnz] = col;
            dest_coefs[offset+nnz] = 0;
            ++nnz;
          }
        }

    sort_if_needed(&dest_cols[offset], nnz);
  }
};

#endif

