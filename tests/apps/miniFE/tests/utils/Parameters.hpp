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

#ifndef _parameters_hpp_
#define _parameters_hpp_

#include <string>

namespace miniFE {

struct Parameters {
  Parameters()
   : nx(5), ny(nx), nz(nx), numthreads(1),
     mv_overlap_comm_comp(0), use_locking(0),
     load_imbalance(0), name(), elem_group_size(1),
     use_elem_mat_fields(1), verify_solution(0),
     device(0),num_devices(2),skip_device(9999),numa(1)
  {}

  int nx;
  int ny;
  int nz;
  int numthreads;
  int mv_overlap_comm_comp;
  int use_locking;
  float load_imbalance;
  std::string name;
  int elem_group_size;
  int use_elem_mat_fields;
  int verify_solution;
  int device;
  int num_devices;
  int skip_device;
  int numa;
};//struct Parameters

}//namespace miniFE

#endif

