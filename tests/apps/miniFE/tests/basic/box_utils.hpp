#ifndef _box_utils_hpp_
#define _box_utils_hpp_

//@HEADER
// ************************************************************************
// 
//               miniFE: simple finite-element assembly and linear-solve
//                 Copyright (2006) Sandia Corporation
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
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER

#include <vector>
#include <map>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include <TypeTraits.hpp>
#include <Box.hpp>

namespace miniFE {

inline void copy_box(const Box& from_box, Box& to_box)
{
  for(int i=0; i<3; ++i) {
    to_box[i][0] = from_box[i][0];
    to_box[i][1] = from_box[i][1];
  }
}

template<typename GlobalOrdinal>
void get_int_coords(GlobalOrdinal ID, int nx, int ny, int nz,
                int& x, int& y, int& z)
{
  z = ID/(nx*ny);
  y = (ID%(nx*ny))/nx;
  x = ID%nx;
}

template<typename GlobalOrdinal,typename Scalar>
void get_coords(GlobalOrdinal ID, int nx, int ny, int nz,
                Scalar& x, Scalar& y, Scalar& z)
{
  const int xdiv = nx>1 ? nx-1 : 1;
  const int ydiv = ny>1 ? ny-1 : 1;
  const int zdiv = nz>1 ? nz-1 : 1;

//This code assumes that ID is 0-based.
//
//compute coordinates that lie on (or in) the unit cube.
//that's why we're dividing by nz,ny,nx:
  z = (1.0*(ID/(nx*ny)))/zdiv;
  y = 1.0*((ID%(nx*ny))/nx)/ydiv;
  x = 1.0*(ID%nx)/xdiv;
}

template<typename GlobalOrdinal>
GlobalOrdinal get_num_ids(const Box& box)
{
  int nx = box[0][1] - box[0][0];
  int ny = box[1][1] - box[1][0];
  int nz = box[2][1] - box[2][0];
  GlobalOrdinal tmp = nx*ny;
  tmp *= nz;
  return tmp;
}

template<typename GlobalOrdinal>
GlobalOrdinal get_id(int nx, int ny, int nz,
                     int x, int y, int z)
{
  if (x<0 || y<0 || z<0) return -1;
  if (x>=nx || y>=ny || z>=nz) return -1;

  //form x + nx*y + nx*ny*z:

  GlobalOrdinal tmp = nx*ny;
  tmp *= z;
  tmp = x + nx * y + tmp;
  return tmp;
}

template<typename GlobalOrdinal>
void get_ids(int nx, int ny, int nz,
             const Box& box,
             GlobalOrdinal* ids)
{
  unsigned offset = 0;
  for(int z=box[2][0]; z<box[2][1]; ++z) {
    for(int y=box[1][0]; y<box[1][1]; ++y) {
      for(int x=box[0][0]; x<box[0][1]; ++x) {
        ids[offset++] = get_id<GlobalOrdinal>(nx, ny, nz, x, y, z);
      }
    }
  }
}

template<typename GlobalOrdinal>
void create_map_id_to_row(int global_nx, int global_ny, int global_nz,
                     const Box& box,
                     std::map<GlobalOrdinal,GlobalOrdinal>& id_to_row)
{
  GlobalOrdinal num_my_ids = get_num_ids<GlobalOrdinal>(box);
  GlobalOrdinal my_first_row = 0;

#ifdef HAVE_MPI
  int numprocs = 1, myproc = 0;
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);

  typename std::vector<GlobalOrdinal> tmp_buffer(numprocs, 0);
  tmp_buffer[myproc] = num_my_ids;
  typename std::vector<GlobalOrdinal> global_offsets(numprocs);
  MPI_Datatype mpi_dtype = TypeTraits<GlobalOrdinal>::mpi_type();
  MPI_Allreduce(&tmp_buffer[0], &global_offsets[0], numprocs, mpi_dtype,
                MPI_SUM, MPI_COMM_WORLD);
  GlobalOrdinal offset = 0;
  for(int i=0; i<numprocs; ++i) {
    GlobalOrdinal tmp = global_offsets[i];
    global_offsets[i] = offset;
    offset += tmp;
  }

  my_first_row = global_offsets[myproc];
#endif

  typename std::vector<GlobalOrdinal> all_my_ids(num_my_ids);
  get_ids(global_nx, global_ny, global_nz, box, &all_my_ids[0]);

  typename std::vector<GlobalOrdinal> ids;
  typename std::vector<GlobalOrdinal> rows;

  if (all_my_ids.size() > 0) {
    ids.push_back(all_my_ids[0]);
    rows.push_back(my_first_row);
  }

  for(size_t i=1; i<all_my_ids.size(); ++i) {
    if (all_my_ids[i] != all_my_ids[i-1]+1) {
      ids.push_back(all_my_ids[i]);
      rows.push_back(my_first_row+i);
    }
  }

#ifdef HAVE_MPI
  int len = ids.size();
  std::vector<int> lengths(numprocs);
  MPI_Allgather(&len, 1, MPI_INT, &lengths[0], 1, MPI_INT, MPI_COMM_WORLD);

  std::vector<int> displs(lengths);
  int displ = 0;
  for(int i=0; i<numprocs; ++i) {
    int tmp = lengths[i];
    displs[i] = displ;
    displ += tmp;
  }

  typename std::vector<GlobalOrdinal> global_ids(displ);
  typename std::vector<GlobalOrdinal> global_rows(displ);

  MPI_Allgatherv(&ids[0], len, mpi_dtype, &global_ids[0],
                 &lengths[0], &displs[0], mpi_dtype, MPI_COMM_WORLD);
  MPI_Allgatherv(&rows[0], len, mpi_dtype, &global_rows[0],
                 &lengths[0], &displs[0], mpi_dtype, MPI_COMM_WORLD);

  ids = global_ids;
  rows = global_rows;
#endif

  for(size_t i=0; i<ids.size(); ++i) {
    id_to_row.insert(std::make_pair(ids[i], rows[i]));
  }
}

}//namespace miniFE

#endif

