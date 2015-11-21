#ifndef _generate_matrix_structure_hpp_
#define _generate_matrix_structure_hpp_

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

#include <sstream>
#include <stdexcept>
#include <map>
#include <algorithm>

#include <simple_mesh_description.hpp>
#include <SparseMatrix_functions.hpp>
#include <box_utils.hpp>
#include <utils.hpp>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

namespace miniFE {

template<typename MatrixType>
int
generate_matrix_structure(const simple_mesh_description<typename MatrixType::GlobalOrdinalType>& mesh,
                          MatrixType& A)
{
  int myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  int threw_exc = 0;
  try {

  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinal;

  const int global_nodes_x = mesh.global_box[0][1]+1;
  const int global_nodes_y = mesh.global_box[1][1]+1;
  const int global_nodes_z = mesh.global_box[2][1]+1;
  Box box;
  copy_box(mesh.local_box, box);

  //num-owned-nodes in each dimension is num-elems+1
  //only if num-elems > 0 in that dimension *and*
  //we are at the high end of the global range in that dimension:
  if (box[0][1] > box[0][0] && box[0][1] == mesh.global_box[0][1]) ++box[0][1];
  if (box[1][1] > box[1][0] && box[1][1] == mesh.global_box[1][1]) ++box[1][1];
  if (box[2][1] > box[2][0] && box[2][1] == mesh.global_box[2][1]) ++box[2][1];

  GlobalOrdinal global_nrows = global_nodes_x;
  global_nrows *= global_nodes_y*global_nodes_z;

  GlobalOrdinal nrows = get_num_ids<GlobalOrdinal>(box);
  try {
    A.reserve_space(nrows, 27);
  }
  catch(std::exception& exc) {
    std::ostringstream osstr;
    osstr << "One of A.rows.resize, A.row_offsets.resize, A.packed_cols.reserve or A.packed_coefs.reserve: nrows=" <<nrows<<": ";
    osstr << exc.what();
    std::string str1 = osstr.str();
    throw std::runtime_error(str1);
  }

  std::vector<GlobalOrdinal> rows(nrows);
  std::vector<LocalOrdinal> row_offsets(nrows+1);
  std::vector<LocalOrdinal> row_coords(nrows*3);

  const MINIFE_GLOBAL_ORDINAL z_width = box[2][1] - box[2][0];
  const MINIFE_GLOBAL_ORDINAL y_width = box[1][1] - box[1][0];
  const MINIFE_GLOBAL_ORDINAL x_width = box[0][1] - box[0][0];
  const MINIFE_GLOBAL_ORDINAL r_n = (box[2][1] - box[2][0]) *
					(box[1][1] - box[1][0]) *
					(box[0][1] - box[0][0]);
  const MINIFE_GLOBAL_ORDINAL xy_width = x_width * y_width;
        MINIFE_GLOBAL_ORDINAL* const row_ptr = &rows[0];
        MINIFE_LOCAL_ORDINAL* const row_offset_ptr = &row_offsets[0];
        MINIFE_LOCAL_ORDINAL* const row_coords_ptr = &row_coords[0];

	#pragma omp parallel for
	for(int r = 0; r < r_n; ++r) {
		int iz = r / (xy_width) + box[2][0];
		int iy = (r / x_width) % y_width + box[1][0];
		int ix = r % x_width + box[0][0];

        	GlobalOrdinal row_id =
                           	get_id<GlobalOrdinal>(global_nodes_x, global_nodes_y, global_nodes_z,
                               	ix, iy, iz);
                       	row_ptr[r] = mesh.map_id_to_row(row_id);
                       	row_coords_ptr[r*3] = ix;
                       	row_coords_ptr[r*3+1] = iy;
                        row_coords_ptr[r*3+2] = iz;

			MINIFE_LOCAL_ORDINAL nnz = 0;
                        for(int sz=-1; sz<=1; ++sz) {
                               	for(int sy=-1; sy<=1; ++sy) {
                                       	for(int sx=-1; sx<=1; ++sx) {
                                               	GlobalOrdinal col_id =
get_id<GlobalOrdinal>(global_nodes_x, global_nodes_y, global_nodes_z,
	                                   ix+sx, iy+sy, iz+sz);

                                               	if (col_id >= 0 && col_id < global_nrows) {
                                               	++nnz;
                                               	}
                                       	}
                               	}
                       	}
                       	row_offset_ptr[r+1] = nnz;

	}

  const MINIFE_GLOBAL_ORDINAL n = row_offsets.size() - 1;
  for(int i = 0; i < n; ++i) {
  	row_offset_ptr[i+1] += row_offset_ptr[i];
  }

  init_matrix(A, rows, row_offsets, row_coords,
              global_nodes_x, global_nodes_y, global_nodes_z, global_nrows, mesh);
  }
  catch(...) {
    std::cout << "proc " << myproc << " threw an exception in generate_matrix_structure, probably due to running out of memory." << std::endl;
    threw_exc = 1;
  }
#ifdef HAVE_MPI
  int global_throw = 0;
  MPI_Allreduce(&threw_exc, &global_throw, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  threw_exc = global_throw;
#endif
  if (threw_exc) {
    return 1;
  }

  return 0;
}

}//namespace miniFE

#endif

