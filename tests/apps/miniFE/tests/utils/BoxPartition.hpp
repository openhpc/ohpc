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

#ifndef _BoxPartition_hpp_
#define _BoxPartition_hpp_

#include <Box.hpp>

/** \brief Recursively split a box into (up-ip) sub-boxes
 */
void box_partition( int ip , int up , int axis ,
                    const Box& box ,
                    Box* p_box );

/** \brief  Partition a { [ix,jx) X [iy,jy) X [iz,jz) } box.
 *
 *  Use recursive coordinate bisection to partition a box 
 *  into np disjoint sub-boxes.  Allocate (via malloc) and
 *  populate the sub-boxes, mapping the local (x,y,z) to
 *  a local ordinal, and mappings for the send-recv messages
 *  to update the ghost cells.
 *
 *  usage:
 *
 *  my_nx = pbox[my_p][0][1] - pbox[my_p][0][0] ;
 *  my_ny = pbox[my_p][1][1] - pbox[my_p][1][0] ;
 *  my_nz = pbox[my_p][2][1] - pbox[my_p][2][0] ;
 *
 *  for ( x = -ghost ; x < my_nx + ghost ; ++x ) {
 *  for ( y = -ghost ; y < my_ny + ghost ; ++y ) {
 *  for ( z = -ghost ; z < my_nz + ghost ; ++z ) {
 *    const int x_global = x + pbox[my_p][0][0] ;
 *    const int y_global = y + pbox[my_p][1][0] ;
 *    const int z_global = z + pbox[my_p][2][0] ;
 *
 *    const int local_ordinal =
 *      box_map_local( pbox[my_p], ghost, map_local_id, x, y, z );
 *
 *    if ( 0 <= local_ordinal ) {
 *    }
 *  }
 *  
 *  for ( i = 1 ; i < np ; ++i ) {
 *    const int recv_processor = ( my_p + i ) % np ;
 *    const int recv_ordinal_begin = map_recv_pc[i];
 *    const int recv_ordinal_end   = map_recv_pc[i+1];
 *  }
 *
 *  for ( i = 1 ; i < np ; ++i ) {
 *    const int send_processor = ( my_p + i ) % np ;
 *    const int send_map_begin = map_send_pc[i];
 *    const int send_map_end   = map_send_pc[i+1];
 *    for ( j = send_map_begin ; j < send_map_end ; ++j ) {
 *      send_ordinal = map_send_id[j] ;
 *    }
 *  }
 */
void box_partition_rcb( 
  const int np            /**< [in]  Number of partitions */ ,
  const int my_p          /**< [in]  My partition rank    */ ,
  const Box& root_box     /**< [in]  3D Box to partition  */ ,
  const int ghost         /**< [in]  Ghost cell boundary  */ ,
  Box* pbox               /**< [out] Partition's 3D boxes */ ,
  int ** map_local_id     /**< [out] Map local cells */ ,
  int ** map_recv_pc      /**< [out] Receive spans per processor */ ,
  int ** map_send_pc      /**< [out] Send prefix counts per processor */ ,
  int ** map_send_id      /**< [out] Send message ordinals */ );

/* \brief  Map a local (x,y,z) to a local ordinal.
 */
int box_map_local( const Box& box_local ,
                   const int ghost ,
                   const int map_local_id[] ,
                   const int local_x ,
                   const int local_y ,
                   const int local_z );

#endif

