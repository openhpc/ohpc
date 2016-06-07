#ifndef _make_local_matrix_hpp_
#define _make_local_matrix_hpp_
#include <assert.h>

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

#include <map>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

namespace miniFE {

template<typename MatrixType>
void
make_local_matrix(MatrixType& A)
{
#ifdef HAVE_MPI
  int numprocs = 1, myproc = 0;
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);

  if (numprocs < 2) {
    A.num_cols = A.rows.size();
    A.has_local_indices = true;
    return;
  }

  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::LocalOrdinalType LocalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

  std::map<GlobalOrdinal,GlobalOrdinal> externals;
  LocalOrdinal num_external = 0;

  //Extract Matrix pieces

  size_t local_nrow = A.rows.size();
  GlobalOrdinal start_row = local_nrow>0 ? A.rows[0] : -1;
  GlobalOrdinal stop_row  = local_nrow>0 ? A.rows[local_nrow-1] : -1;

  // We need to convert the index values for the rows on this processor
  // to a local index space. We need to:
  // - Determine if each index reaches to a local value or external value
  // - If local, subtract start_row from index value to get local index
  // - If external, find out if it is already accounted for.
  //   - If so, then do nothing,
  //   - otherwise
  //     - add it to the list of external indices,
  //     - find out which processor owns the value.
  //     - Set up communication for sparse MV operation

  ///////////////////////////////////////////
  // Scan the indices and transform to local
  ///////////////////////////////////////////

  std::vector<GlobalOrdinal>& external_index = A.external_index;

  for(size_t i=0; i<A.rows.size(); ++i) {
    GlobalOrdinal* Acols = NULL;
    Scalar* Acoefs = NULL;
    size_t row_len = 0;
    A.get_row_pointers(A.rows[i], row_len, Acols, Acoefs);

    for(size_t j=0; j<row_len; ++j) {
      GlobalOrdinal cur_ind = Acols[j];
      if (start_row <= cur_ind && cur_ind <= stop_row) {
        Acols[j] -= start_row;
      }
      else { // Must find out if we have already set up this point
        if (externals.find(cur_ind) == externals.end()) {
          externals[cur_ind] = num_external++;
          external_index.push_back(cur_ind);
        }
        // Mark index as external by adding 1 and negating it
        Acols[j] = -(Acols[j] + 1);
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////
  // Go through list of externals to find out which processors must be accessed.
  ////////////////////////////////////////////////////////////////////////

  std::vector<GlobalOrdinal> tmp_buffer(numprocs, 0); // Temp buffer space needed below

  // Build list of global index offset

  std::vector<GlobalOrdinal> global_index_offsets(numprocs, 0);

  tmp_buffer[myproc] = start_row; // This is my start row

  // This call sends the start_row of each ith processor to the ith
  // entry of global_index_offsets on all processors.
  // Thus, each processor knows the range of indices owned by all
  // other processors.
  // Note: There might be a better algorithm for doing this, but this
  //       will work...

  MPI_Datatype mpi_dtype = TypeTraits<GlobalOrdinal>::mpi_type();
  MPI_Allreduce(&tmp_buffer[0], &global_index_offsets[0], numprocs, mpi_dtype,
                MPI_SUM, MPI_COMM_WORLD);

  // Go through list of externals and find the processor that owns each
  std::vector<int> external_processor(num_external);

  for(LocalOrdinal i=0; i<num_external; ++i) {
    GlobalOrdinal cur_ind = external_index[i];
    for(int j=numprocs-1; j>=0; --j) {
      if (global_index_offsets[j] <= cur_ind && global_index_offsets[j] >= 0) {
        external_processor[i] = j;
        break;
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////
  // Sift through the external elements. For each newly encountered external
  // point assign it the next index in the sequence. Then look for other
  // external elements who are updated by the same node and assign them the next
  // set of index numbers in the sequence (ie. elements updated by the same node
  // have consecutive indices).
  /////////////////////////////////////////////////////////////////////////

  size_t count = local_nrow;
  std::vector<GlobalOrdinal>& external_local_index = A.external_local_index;
  external_local_index.assign(num_external, -1);

  for(LocalOrdinal i=0; i<num_external; ++i) {
    if (external_local_index[i] == -1) {
      external_local_index[i] = count++;

      for(LocalOrdinal j=i+1; j<num_external; ++j) {
        if (external_processor[j] == external_processor[i])
          external_local_index[j] = count++;
      }
    }
  }

  for(size_t i=0; i<local_nrow; ++i) {
    GlobalOrdinal* Acols = NULL;
    Scalar* Acoefs = NULL;
    size_t row_len = 0;
    A.get_row_pointers(A.rows[i], row_len, Acols, Acoefs);

    for(size_t j=0; j<row_len; ++j) {
      if (Acols[j] < 0) { // Change index values of externals
        GlobalOrdinal cur_ind = -Acols[j] - 1;
        Acols[j] = external_local_index[externals[cur_ind]];
      }
    }
  }

  std::vector<int> new_external_processor(num_external, 0);

  for(int i=0; i<num_external; ++i) {
    new_external_processor[external_local_index[i]-local_nrow] =
      external_processor[i];
  }

  ////////////////////////////////////////////////////////////////////////
  ///
  // Count the number of neighbors from which we receive information to update
  // our external elements. Additionally, fill the array tmp_neighbors in the
  // following way:
  //      tmp_neighbors[i] = 0   ==>  No external elements are updated by
  //                              processor i.
  //      tmp_neighbors[i] = x   ==>  (x-1)/numprocs elements are updated from
  //                              processor i.
  ///
  ////////////////////////////////////////////////////////////////////////

  std::vector<GlobalOrdinal> tmp_neighbors(numprocs, 0);

  int num_recv_neighbors = 0;
  int length             = 1;

  for(LocalOrdinal i=0; i<num_external; ++i) {
    if (tmp_neighbors[new_external_processor[i]] == 0) {
      ++num_recv_neighbors;
      tmp_neighbors[new_external_processor[i]] = 1;
    }
    tmp_neighbors[new_external_processor[i]] += numprocs;
  }

  /// sum over all processor all the tmp_neighbors arrays ///

  MPI_Allreduce(&tmp_neighbors[0], &tmp_buffer[0], numprocs, mpi_dtype,
                MPI_SUM, MPI_COMM_WORLD);

  // decode the combined 'tmp_neighbors' (stored in tmp_buffer)
  // array from all the processors

  GlobalOrdinal num_send_neighbors = tmp_buffer[myproc] % numprocs;

  /// decode 'tmp_buffer[myproc] to deduce total number of elements
  //  we must send

  GlobalOrdinal total_to_be_sent = (tmp_buffer[myproc] - num_send_neighbors) / numprocs;

  ///////////////////////////////////////////////////////////////////////
  ///
  // Make a list of the neighbors that will send information to update our
  // external elements (in the order that we will receive this information).
  ///
  ///////////////////////////////////////////////////////////////////////

  std::vector<int> recv_list;
  recv_list.push_back(new_external_processor[0]);
  for(LocalOrdinal i=1; i<num_external; ++i) {
    if (new_external_processor[i-1] != new_external_processor[i]) {
      recv_list.push_back(new_external_processor[i]);
    }
  }

  //
  // Send a 0 length message to each of our recv neighbors
  //

  std::vector<int> send_list(num_send_neighbors, 0);

  //
  // first post receives, these are immediate receives
  // Do not wait for result to come, will do that at the
  // wait call below.
  //
  int MPI_MY_TAG = 99;

  std::vector<MPI_Request> request(num_send_neighbors);
  for(int i=0; i<num_send_neighbors; ++i) {
    MPI_Irecv(&tmp_buffer[i], 1, mpi_dtype, MPI_ANY_SOURCE, MPI_MY_TAG,
              MPI_COMM_WORLD, &request[i]);
  }

  // send messages

  for(int i=0; i<num_recv_neighbors; ++i) {
    MPI_Send(&tmp_buffer[i], 1, mpi_dtype, recv_list[i], MPI_MY_TAG,
             MPI_COMM_WORLD);
  }

  ///
  // Receive message from each send neighbor to construct 'send_list'.
  ///

  MPI_Status status;
  for(int i=0; i<num_send_neighbors; ++i) {
    if (MPI_Wait(&request[i], &status) != MPI_SUCCESS) {
      std::cerr << "MPI_Wait error\n"<<std::endl;
      MPI_Abort(MPI_COMM_WORLD, -1);
    }
    send_list[i] = status.MPI_SOURCE;
  }

  //////////////////////////////////////////////////////////////////////
  ///
  // Compare the two lists. In most cases they should be the same.
  // However, if they are not then add new entries to the recv list
  // that are in the send list (but not already in the recv list).
  ///
  //////////////////////////////////////////////////////////////////////

  for(int j=0; j<num_send_neighbors; ++j) {
    int found = 0;
    for(int i=0; i<num_recv_neighbors; ++i) {
      if (recv_list[i] == send_list[j]) found = 1;
    }

    if (found == 0) {
      recv_list.push_back(send_list[j]);
      ++num_recv_neighbors;
    }
  }

  num_send_neighbors = num_recv_neighbors;
  request.resize(num_send_neighbors);

  A.elements_to_send.assign(total_to_be_sent, 0);
  A.send_buffer.assign(total_to_be_sent, 0);

  //
  // Create 'new_external' which explicitly put the external elements in the
  // order given by 'external_local_index'
  //

  std::vector<GlobalOrdinal> new_external(num_external);
  for(LocalOrdinal i=0; i<num_external; ++i) {
    new_external[external_local_index[i] - local_nrow] = external_index[i];
  }

  /////////////////////////////////////////////////////////////////////////
  //
  // Send each processor the global index list of the external elements in the
  // order that I will want to receive them when updating my external elements.
  //
  /////////////////////////////////////////////////////////////////////////

  std::vector<int> lengths(num_recv_neighbors);

  ++MPI_MY_TAG;

  // First post receives

  for(int i=0; i<num_recv_neighbors; ++i) {
    int partner = recv_list[i];
    MPI_Irecv(&lengths[i], 1, MPI_INT, partner, MPI_MY_TAG, MPI_COMM_WORLD,
              &request[i]);
  }

  std::vector<int>& neighbors = A.neighbors;
  std::vector<int>& recv_length = A.recv_length;
  std::vector<int>& send_length = A.send_length;

  neighbors.resize(num_recv_neighbors, 0);
  A.request.resize(num_recv_neighbors);
  recv_length.resize(num_recv_neighbors, 0);
  send_length.resize(num_recv_neighbors, 0);

  LocalOrdinal j = 0;
  for(int i=0; i<num_recv_neighbors; ++i) {
    int start = j;
    int newlength = 0;

    //go through list of external elements until updating
    //processor changes

    while((j < num_external) &&
          (new_external_processor[j] == recv_list[i])) {
      ++newlength;
      ++j;
      if (j == num_external) break;
    }

    recv_length[i] = newlength;
    neighbors[i] = recv_list[i];

    length = j - start;
    MPI_Send(&length, 1, MPI_INT, recv_list[i], MPI_MY_TAG, MPI_COMM_WORLD);
  }

  // Complete the receives of the number of externals

  for(int i=0; i<num_recv_neighbors; ++i) {
    if (MPI_Wait(&request[i], &status) != MPI_SUCCESS) {
      std::cerr << "MPI_Wait error\n"<<std::endl;
      MPI_Abort(MPI_COMM_WORLD, -1);
    }
    send_length[i] = lengths[i];
  }

  ////////////////////////////////////////////////////////////////////////
  // Build "elements_to_send" list. These are the x elements I own
  // that need to be sent to other processors.
  ////////////////////////////////////////////////////////////////////////

  ++MPI_MY_TAG;

  j = 0;
  for(int i=0; i<num_recv_neighbors; ++i) {
    MPI_Irecv(&A.elements_to_send[j], send_length[i], mpi_dtype, neighbors[i],
              MPI_MY_TAG, MPI_COMM_WORLD, &request[i]);
    j += send_length[i];
  }

  j = 0;
  for(int i=0; i<num_recv_neighbors; ++i) {
    LocalOrdinal start = j;
    LocalOrdinal newlength = 0;

    // Go through list of external elements
    // until updating processor changes. This is redundant, but
    // saves us from recording this information.

    while((j < num_external) &&
          (new_external_processor[j] == recv_list[i])) {
      ++newlength;
      ++j;
      if (j == num_external) break;
    }
    MPI_Send(&new_external[start], j-start, mpi_dtype, recv_list[i],
             MPI_MY_TAG, MPI_COMM_WORLD);
  }

  // receive from each neighbor the global index list of external elements

  for(int i=0; i<num_recv_neighbors; ++i) {
    if (MPI_Wait(&request[i], &status) != MPI_SUCCESS) {
      std::cerr << "MPI_Wait error\n"<<std::endl;
      MPI_Abort(MPI_COMM_WORLD, -1);
    }
  }

  /// replace global indices by local indices ///

  for(GlobalOrdinal i=0; i<total_to_be_sent; ++i) {
    A.elements_to_send[i] -= start_row;
    if (A.elements_to_send[i] >= A.rows.size()) {
//std::cout<<"start_row: "<<start_row<<", A.elements_to_send[i]: "<<A.elements_to_send[i]<<", A.rows.size(): "<<A.rows.size()<<std::endl;
    assert(A.elements_to_send[i] < A.rows.size());
    }
  }

  //////////////////
  // Finish up !!
  //////////////////

  A.num_cols = local_nrow + num_external;

#else
  A.num_cols = A.rows.size();
#endif

  A.has_local_indices = true;
}

}//namespace miniFE

#endif

