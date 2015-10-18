#ifndef _box_utils_hpp_
#define _box_utils_hpp_

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

#include <vector>
#include <set>
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
#ifdef __CUDACC__
__host__ __device__ __inline__
#endif
void get_int_coords(GlobalOrdinal ID, int nx, int ny, int nz,
                int& x, int& y, int& z)
{
  z = ID/(nx*ny);
  y = (ID%(nx*ny))/nx;
  x = ID%nx;
}

template<typename GlobalOrdinal,typename Scalar>
#ifdef __CUDACC__
__host__ __device__ __inline__
#endif
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
#ifdef __CUDACC__
__host__ __device__ __inline__
#endif
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
             std::vector<GlobalOrdinal>& ids,
             bool include_ghost_layer=false)
{
  ids.clear();
  int minz = box[2][0];
  int maxz = box[2][1];
  int miny = box[1][0];
  int maxy = box[1][1];
  int minx = box[0][0];
  int maxx = box[0][1];

  if (include_ghost_layer) {
    if (minz > 0) minz--;
    if (miny > 0) miny--;
    if (minx > 0) minx--;
    if (maxz < nz) maxz++;
    if (maxy < ny) maxy++;
    if (maxx < nx) maxx++;
  }

  size_t ids_size = ((maxz - minz) * (maxy - miny)) * (maxx - minx);
  ids.reserve(ids_size);

  for(int z=minz; z<maxz; ++z) {
    for(int y=miny; y<maxy; ++y) {
      for(int x=minx; x<maxx; ++x) {
        ids.push_back(get_id<GlobalOrdinal>(nx, ny, nz, x, y, z));
      }
    }
  }
}

template<typename GlobalOrdinal>
void get_ghost_ids(int nx, int ny, int nz,
             const Box& box,
             std::vector<GlobalOrdinal>& ids)
{
  ids.clear();
  int minz,maxz,miny,maxy,minx,maxx;
  int orig_minz = minz = box[2][0];
  int orig_maxz = maxz = box[2][1];
  int orig_miny = miny = box[1][0];
  int orig_maxy = maxy = box[1][1];
  int orig_minx = minx = box[0][0];
  int orig_maxx = maxx = box[0][1];

  if (minz > 0) minz--;
  if (miny > 0) miny--;
  if (minx > 0) minx--;
  if (maxz < nz) maxz++;
  if (maxy < ny) maxy++;
  if (maxx < nx) maxx++;

  for(int z=minz; z<maxz; ++z) {
    for(int y=miny; y<maxy; ++y) {
      for(int x=minx; x<maxx; ++x) {
        bool x_in_ghost_layer = (x < orig_minx) || (x >= orig_maxx);
        bool y_in_ghost_layer = (y < orig_miny) || (y >= orig_maxy);
        bool z_in_ghost_layer = (z < orig_minz) || (z >= orig_maxz);
        //we are in the ghost layer if any one of x,y,z are in the ghost layer
        if (!x_in_ghost_layer && !y_in_ghost_layer && !z_in_ghost_layer) continue;
        ids.push_back(get_id<GlobalOrdinal>(nx, ny, nz, x, y, z));
      }
    }
  }
}

 inline void print_box(int myproc, const char* name, const Box& box,
                      const char* name2, const Box& box2)
{
  std::cout << "proc " << myproc << " "<<name
      <<" ("<<box[0][0]<<","<<box[0][1]<<") "
      <<" ("<<box[1][0]<<","<<box[1][1]<<") "
      <<" ("<<box[2][0]<<","<<box[2][1]<<") "
      <<name2
      <<" ("<<box2[0][0]<<","<<box2[0][1]<<") "
      <<" ("<<box2[1][0]<<","<<box2[1][1]<<") "
      <<" ("<<box2[2][0]<<","<<box2[2][1]<<") "<<std::endl;
}

bool is_neighbor(const Box& box1, const Box& box2)
{
  //neighbors in the x dimension if:
  bool x_neighbor = (box1[0][1] == box2[0][0]) || (box1[0][0] == box2[0][1]) || // min matches max
                    (box1[0][0] == box2[0][0]) || (box1[0][1] == box2[0][1]) || // mins or maxs match
                    (box1[0][0] >  box2[0][0]  &&  box1[0][1] <  box2[0][1]) || // range contains other
                    (box2[0][0] >  box1[0][0]  &&  box2[0][1] <  box1[0][1]) || // range contains other
                    (box1[0][0] >  box2[0][0]  &&  box1[0][0] <  box2[0][1]) || // min contained in rng
                    (box2[0][0] >  box1[0][0]  &&  box2[0][0] <  box1[0][1]);   // min contained in rng
  if (!x_neighbor) {
    x_neighbor = (box1[0][1] == box2[0][0]-1) || (box1[0][0] == box2[0][1]+1);
  }

  bool y_neighbor = (box1[1][1] == box2[1][0]) || (box1[1][0] == box2[1][1]) || // min matches max
                    (box1[1][0] == box2[1][0]) || (box1[1][1] == box2[1][1]) || // mins or maxs match
                    (box1[1][0] >  box2[1][0]  &&  box1[1][1] <  box2[1][1]) || // range contains other
                    (box2[1][0] >  box1[1][0]  &&  box2[1][1] <  box1[1][1]) || // range contains other
                    (box1[1][0] >  box2[1][0]  &&  box1[1][0] <  box2[1][1]) || // min contained in rng
                    (box2[1][0] >  box1[1][0]  &&  box2[1][0] <  box1[1][1]);   // min contained in rng
  if (!y_neighbor) {
    y_neighbor = (box1[1][1] == box2[1][0]-1) || (box1[1][0] == box2[1][1]+1);
  }

  bool z_neighbor = (box1[2][1] == box2[2][0]) || (box1[2][0] == box2[2][1]) || // min matches max
                    (box1[2][0] == box2[2][0]) || (box1[2][1] == box2[2][1]) || // mins or maxs match
                    (box1[2][0] >  box2[2][0]  &&  box1[2][1] <  box2[2][1]) || // range contains other
                    (box2[2][0] >  box1[2][0]  &&  box2[2][1] <  box1[2][1]) || // range contains other
                    (box1[2][0] >  box2[2][0]  &&  box1[2][0] <  box2[2][1]) || // min contained in rng
                    (box2[2][0] >  box1[2][0]  &&  box2[2][0] <  box1[2][1]);   // min contained in rng
  if (!z_neighbor) {
    z_neighbor = (box1[2][1] == box2[2][0]-1) || (box1[2][0] == box2[2][1]+1);
  }

  return x_neighbor && y_neighbor && z_neighbor;
}

template<typename GlobalOrdinal>
void create_map_id_to_row(int global_nx, int global_ny, int global_nz,
                     const Box& box,
                     std::map<GlobalOrdinal,GlobalOrdinal>& id_to_row)
{
  GlobalOrdinal num_my_ids = get_num_ids<GlobalOrdinal>(box);

  typename std::vector<GlobalOrdinal> all_ids;
  bool include_ghost_layer = false;
  get_ids(global_nx, global_ny, global_nz, box, all_ids, include_ghost_layer);

  GlobalOrdinal my_first_row = 0;
  typename std::vector<GlobalOrdinal> global_offsets;
  std::vector<int> all_boxes;
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);

  GlobalOrdinal local_num_ids = num_my_ids;
  global_offsets.resize(numprocs);
  MPI_Datatype mpi_dtype = TypeTraits<GlobalOrdinal>::mpi_type();
  MPI_Allgather(&local_num_ids, 1, mpi_dtype, &global_offsets[0], 1, mpi_dtype, MPI_COMM_WORLD);
  GlobalOrdinal offset = 0;
  for(int i=0; i<numprocs; ++i) {
    GlobalOrdinal tmp = global_offsets[i];
    global_offsets[i] = offset;
    offset += tmp;
  }

  my_first_row = global_offsets[myproc];

  all_boxes.resize(6*numprocs);
  int* local_box_ranges = const_cast<int*>(&box.ranges[0]);
  MPI_Allgather(local_box_ranges, 6, MPI_INT, &all_boxes[0], 6, MPI_INT, MPI_COMM_WORLD);
#endif

  if (all_ids.size() > 0) {
    id_to_row.insert(std::make_pair(all_ids[0], my_first_row));
  }

  for(size_t i=1; i<all_ids.size(); ++i) {
    if (all_ids[i] != all_ids[i-1]+1) {
      id_to_row.insert(std::make_pair(all_ids[i], my_first_row+i));
    }
  }

//  int num_neighbors = 0;
  for(int i=0; i<numprocs; ++i) {
    if (i == myproc) continue;
    Box box_i;
    for(int r=0; r<6; ++r) box_i.ranges[r] = all_boxes[i*6 + r];
//    bool neighbor= is_neighbor(box, box_i);
//if(myproc==2) {
//  std::cout<<"i: "<<i<<" "<<neighbor<<" ";
//  print_box(myproc, " ", box, " ", box_i);
//}
    if (!is_neighbor(box, box_i)) {
//      if (myproc==50) {
//        std::cout<<"box ("<<box[0][0]<<","<<box[0][1]<<" - "<<box[1][0]<<","<<box[1][1]<<" - "<<box[2][0]<<","<<box[2][1]<<")"<<std::endl<<" and ("<<box_i[0][0]<<","<<box_i[0][1]<<" - "<<box_i[1][0]<<","<<box_i[1][1]<<" - "<<box_i[2][0]<<","<<box_i[2][1]<<") not neighbors."<<std::endl;
//      }
      continue;
    }
//    ++num_neighbors;

    get_ids(global_nx, global_ny, global_nz, box_i, all_ids, include_ghost_layer);

    GlobalOrdinal first_row = global_offsets[i];
    if (all_ids.size() > 0) {
      id_to_row.insert(std::make_pair(all_ids[0], first_row));
    }
    for(size_t j=1; j<all_ids.size(); ++j) {
      if (all_ids[j] != all_ids[j-1]+1) {
        id_to_row.insert(std::make_pair(all_ids[j], first_row+j));
      }
    }
  }

//std::cout<<"proc "<<myproc<<": num_neighbors: "<<num_neighbors<<", id_to_row.size(): "<<id_to_row.size()<<std::endl;
//typename std::map<GlobalOrdinal,GlobalOrdinal>::iterator iter = id_to_row.begin(), end = id_to_row.end();
//for(; iter!=end; ++iter) {
//  std::cout<<"proc "<<myproc<<": "<<iter->first<<" :: "<<iter->second<<std::endl;
//}
}

}//namespace miniFE

#endif

