
#ifndef _simple_mesh_description_hpp_
#define _simple_mesh_description_hpp_

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

#include <utils.hpp>
#include <set>
#include <map>

namespace miniFE {

template<typename GlobalOrdinal>
class simple_mesh_description {
public:
  simple_mesh_description(const Box& global_box_in, const Box& local_box_in)
  {
   Box local_node_box;
    for(int i=0; i<3; ++i) {
      global_box[i][0] = global_box_in[i][0];
      global_box[i][1] = global_box_in[i][1];
      local_box[i][0] = local_box_in[i][0];
      local_box[i][1] = local_box_in[i][1];
      local_node_box[i][0] = local_box_in[i][0];
      local_node_box[i][1] = local_box_in[i][1];
      //num-owned-nodes == num-elems+1 in this dimension if the elem box is not empty
      //and we are at the high end of the global range in that dimension:
      if (local_box_in[i][1] > local_box_in[i][0] && local_box_in[i][1] == global_box[i][1]) local_node_box[i][1] += 1;
    }

    int max_node_x = global_box[0][1]+1;
    int max_node_y = global_box[1][1]+1;
    int max_node_z = global_box[2][1]+1;
    create_map_id_to_row(max_node_x, max_node_y, max_node_z, local_node_box,
                         map_ids_to_rows);

    //As described in analytic_soln.hpp,
    //we will impose a 0 boundary-condition on faces x=0, y=0, z=0, y=1, z=1
    //we will impose a 1 boundary-condition on face x=1

#ifdef MINIFE_DEBUG
std::cout<<std::endl;
#endif
    const int X=0;
    const int Y=1;
    const int Z=2;

    const int x1 = max_node_x - 1;
    const int y1 = max_node_y - 1;
    const int z1 = max_node_z - 1;

    //if we're on the x=0 face:
    if (global_box[X][0] == local_box[X][0]) {
      int miny = local_node_box[Y][0];
      int minz = local_node_box[Z][0];
      int maxy = local_node_box[Y][1];
      int maxz = local_node_box[Z][1];
      //expand y and z dimensions to include ghost layer
      if (local_node_box[Y][0] > 0) --miny;
      if (local_node_box[Z][0] > 0) --minz;
      if (local_node_box[Y][1] < max_node_y) ++maxy;
      if (local_node_box[Z][1] < max_node_z) ++maxz;

      for(int iz=minz; iz<maxz; ++iz) {
        for(int iy=miny; iy<maxy; ++iy) {
          GlobalOrdinal nodeID = get_id<GlobalOrdinal>(max_node_x, max_node_y, max_node_z,
             0, iy, iz);
#ifdef MINIFE_DEBUG
std::cout<<"x=0 BC, node "<<nodeID<<", (0,"<<iy<<","<<iz<<")"<<std::endl;
#endif
          bc_rows_0.insert(map_id_to_row(nodeID));
        }
      }
    }

    //if we're on the y=0 face:
    if (global_box[Y][0] == local_box[Y][0]) {
      int minx = local_node_box[X][0];
      int minz = local_node_box[Z][0];
      int maxx = local_node_box[X][1];
      int maxz = local_node_box[Z][1];
      //expand x and z dimensions to include ghost layer
      if (local_node_box[X][0] > 0) --minx;
      if (local_node_box[Z][0] > 0) --minz;
      if (local_node_box[X][1] < max_node_x) ++maxx;
      if (local_node_box[Z][1] < max_node_z) ++maxz;

      for(int iz=minz; iz<maxz; ++iz) {
        for(int ix=minx; ix<maxx; ++ix) {
          GlobalOrdinal nodeID = get_id<GlobalOrdinal>(max_node_x, max_node_y, max_node_z,
             ix, 0, iz);
#ifdef MINIFE_DEBUG
std::cout<<"y=0 BC, node "<<nodeID<<", ("<<ix<<",0,"<<iz<<")"<<std::endl;
#endif
          GlobalOrdinal row = map_id_to_row(nodeID);
          if (row < 0) {
            std::cout<<"on the y==0 face (ix="<<ix<<", iz="<<iz<<"), ERROR: found negative row ("<<row<<") for nodeID="<<nodeID<<std::endl;
          }
          bc_rows_0.insert(row);
        }
      }
    }

    //if we're on the z=0 face:
    if (global_box[Z][0] == local_box[Z][0]) {
      int minx = local_node_box[X][0];
      int miny = local_node_box[Y][0];
      int maxx = local_node_box[X][1];
      int maxy = local_node_box[Y][1];
      //expand x and y dimensions to include ghost layer
      if (local_node_box[X][0] > 0) --minx;
      if (local_node_box[Y][0] > 0) --miny;
      if (local_node_box[X][1] < max_node_x) ++maxx;
      if (local_node_box[Y][1] < max_node_y) ++maxy;

      for(int iy=miny; iy<maxy; ++iy) {
        for(int ix=minx; ix<maxx; ++ix) {
          GlobalOrdinal nodeID = get_id<GlobalOrdinal>(max_node_x, max_node_y, max_node_z,
             ix, iy, 0);
#ifdef MINIFE_DEBUG
std::cout<<"z=0 BC, node "<<nodeID<<", ("<<ix<<","<<iy<<",0)"<<std::endl;
#endif
          bc_rows_0.insert(map_id_to_row(nodeID));
        }
      }
    }

    //if we're on the x=1 face:
    if (global_box[X][1] == local_box[X][1]) {
      int minz = local_node_box[Z][0];
      int miny = local_node_box[Y][0];
      int maxz = local_node_box[Z][1];
      int maxy = local_node_box[Y][1];
      //expand z and y dimensions to include ghost layer
      if (local_node_box[Z][0] > 0) --minz;
      if (local_node_box[Y][0] > 0) --miny;
      if (local_node_box[Z][1] < max_node_z) ++maxz;
      if (local_node_box[Y][1] < max_node_y) ++maxy;

      for(int iy=miny; iy<maxy; ++iy) {
        for(int iz=minz; iz<maxz; ++iz) {
          GlobalOrdinal nodeID = get_id<GlobalOrdinal>(max_node_x, max_node_y, max_node_z,
             x1, iy, iz);
          GlobalOrdinal row = map_id_to_row(nodeID);
#ifdef MINIFE_DEBUG
std::cout<<"x=1 BC, node "<<nodeID<<", row "<<row<<", ("<<x1<<","<<iy<<","<<iz<<")"<<std::endl;
#endif
          bc_rows_1.insert(row);
        }
      }
    }

    //if we're on the y=1 face:
    if (global_box[Y][1] == local_box[Y][1]) {
      int minz = local_node_box[Z][0];
      int minx = local_node_box[X][0];
      int maxz = local_node_box[Z][1];
      int maxx = local_node_box[X][1];
      //expand z and x dimensions to include ghost layer
      if (local_node_box[Z][0] > 0) --minz;
      if (local_node_box[X][0] > 0) --minx;
      if (local_node_box[Z][1] < max_node_z) ++maxz;
      if (local_node_box[X][1] < max_node_x) ++maxx;

      for(int ix=minx; ix<maxx; ++ix) {
        for(int iz=minz; iz<maxz; ++iz) {
          GlobalOrdinal nodeID = get_id<GlobalOrdinal>(max_node_x, max_node_y, max_node_z,
             ix, y1, iz);
#ifdef MINIFE_DEBUG
std::cout<<"y=1 BC, node "<<nodeID<<", ("<<ix<<","<<y1<<","<<iz<<")"<<std::endl;
#endif
          bc_rows_0.insert(map_id_to_row(nodeID));
        }
      }
    }

    //if we're on the z=1 face:
    if (global_box[Z][1] == local_box[Z][1]) {
      int miny = local_node_box[Y][0];
      int minx = local_node_box[X][0];
      int maxy = local_node_box[Y][1];
      int maxx = local_node_box[X][1];
      //expand x and y dimensions to include ghost layer
      if (local_node_box[Y][0] > 0) --miny;
      if (local_node_box[X][0] > 0) --minx;
      if (local_node_box[Y][1] < max_node_y) ++maxy;
      if (local_node_box[X][1] < max_node_x) ++maxx;

      for(int ix=minx; ix<maxx; ++ix) {
        for(int iy=miny; iy<maxy; ++iy) {
          GlobalOrdinal nodeID = get_id<GlobalOrdinal>(max_node_x, max_node_y, max_node_z,
             ix, iy, z1);
#ifdef MINIFE_DEBUG
std::cout<<"z=1 BC, node "<<nodeID<<", ("<<ix<<","<<iy<<","<<z1<<")"<<std::endl;
#endif
          bc_rows_0.insert(map_id_to_row(nodeID));
        }
      }
    }

  }

  GlobalOrdinal map_id_to_row(const GlobalOrdinal& id) const
  {
    return find_row_for_id(id, map_ids_to_rows);
  }

  GlobalOrdinal max_row_in_map() const {
    if (map_ids_to_rows.empty()) return 0;
    typename std::map<GlobalOrdinal,GlobalOrdinal>::const_iterator mend = map_ids_to_rows.end();
    --mend;
    return mend->second;
  }
  std::set<GlobalOrdinal> bc_rows_0;
  std::set<GlobalOrdinal> bc_rows_1;
  std::map<GlobalOrdinal,GlobalOrdinal> map_ids_to_rows;
  Box global_box;
  Box local_box;
};//class simple_mesh_description

}//namespace miniFE

#endif
