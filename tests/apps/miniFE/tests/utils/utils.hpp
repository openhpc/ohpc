#ifndef _utils_hpp_
#define _utils_hpp_

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

#include <cstdlib>
#include <cmath>
#include <vector>
#include <map>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include <TypeTraits.hpp>
#include <Parameters.hpp>

namespace miniFE {

void get_parameters(int argc, char** argv, Parameters& params);

void broadcast_parameters(Parameters& params);

void initialize_mpi(int argc, char** argv, int& numprocs, int& myproc);

void finalize_mpi();

template<typename Scalar>
Scalar percentage_difference(Scalar value, Scalar average)
{
  //result will be the difference between value and average, represented as
  //a percentage of average.
  //Examples:
  //  if value=100 and average=50, result is 100%
  //  if value=500 and average=400, result is 25%

  //Note: if average is 0, result is undefined. We'll return -1.0;

  Scalar result = std::abs(value-average);
  if (std::abs(average) > 1.e-5) {
    result /= average;
    result *= 100;
  }
  else result = -1;

  return result;
}

template<typename GlobalOrdinal>
void get_global_min_max(GlobalOrdinal local_n,
                        GlobalOrdinal& global_n,
                        GlobalOrdinal& min_n,
                        int& min_proc,
                        GlobalOrdinal& max_n,
                        int& max_proc)
{
//Given a local_n, compute global_n, min/max, etc. All computed results
//will be returned on all processors.
//
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  std::vector<GlobalOrdinal> all_n(numprocs, 0);
  all_n[myproc] = local_n;
#ifdef HAVE_MPI
  std::vector<GlobalOrdinal> tmp(all_n);
  MPI_Datatype mpi_dtype = TypeTraits<GlobalOrdinal>::mpi_type();
  MPI_Allreduce(&tmp[0], &all_n[0], numprocs, mpi_dtype, MPI_MAX, MPI_COMM_WORLD);
#endif

  global_n = 0;
  min_n= 5*local_n;
  min_proc = 0;
  max_n= 0;
  max_proc = 0;

  for(int i=0; i<numprocs; ++i) {
    global_n += all_n[i];
    //min_proc will be the lowest-numbered proc with n = min_n
    if (all_n[i] < min_n) {
      min_n = all_n[i];
      min_proc = i;
    }
    //max_proc will be the highest-numbered proc with n = max_n
    if (all_n[i] >= max_n) {
      max_n = all_n[i];
      max_proc = i;
    }
  }
}

template<typename Scalar>
Scalar compute_std_dev_as_percentage(Scalar local_nrows,
                                     Scalar avg_nrows)
{
//compute and return a standard deviation for the deviation of local_nrows from the average.
//the std. dev. will be expressed as a percentage of avg_nrows.
//
//Input argument local_nrows is really a integer, but taking it as a floating-point scalar is
//harmless.
//
#ifdef HAVE_MPI
  int numprocs = 1, myproc = 0;
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
  MPI_Datatype mpi_dtype = TypeTraits<Scalar>::mpi_type();

//If it's significantly more efficient, we may consider using MPI_Gather below instead of
//MPI_Allgather. We really only need to compute std.dev. on proc 0...
//
//(But for now, use MPI_Allgather and compute on all procs.)

  std::vector<Scalar> all_nrows(numprocs, 0);
  MPI_Allgather(&local_nrows, 1, mpi_dtype, &all_nrows[0], 1, mpi_dtype, MPI_COMM_WORLD);

  //turn all_nrows contents into deviations, add to sum-of-squares-of-deviations:
  Scalar sum_sqr_dev = 0;
  for(size_t i=0; i<all_nrows.size(); ++i) {
    all_nrows[i] -= avg_nrows;
    all_nrows[i] *= all_nrows[i];
    sum_sqr_dev += all_nrows[i];
  }
  Scalar tmp1 = sum_sqr_dev;
  Scalar std_dev = numprocs>1 ? std::sqrt(tmp1/(numprocs-1)) : 0;

  //std_dev is now the standard deviation of rows-per-processor with respect
  //to avg_nrows.
  //Next turn std_dev into a percentage of avg_nrows:
  std_dev /= avg_nrows;
  std_dev *= 100;
  return std_dev;
#else
  return 0;
#endif
}

template<typename GlobalOrdinal>
GlobalOrdinal find_row_for_id(GlobalOrdinal id,
                              const std::map<GlobalOrdinal,GlobalOrdinal>& ids_to_rows)
{
  typename std::map<GlobalOrdinal,GlobalOrdinal>::const_iterator
    iter = ids_to_rows.lower_bound(id);

  if (iter == ids_to_rows.end() || iter->first != id) {
    if (ids_to_rows.size() > 0) {
      --iter;
    }
    else {
      std::cout << "ERROR, failed to map id to row."<<std::endl;
      return -99;
    }
  }

  if (iter->first == id) {
    return iter->second;
  }

  if (iter == ids_to_rows.begin() && iter->first > id) {
    std::cout << "ERROR, id:" << id << ", ids_to_rows.begin(): " << iter->first<<std::endl;
    return -99;
  }

  GlobalOrdinal offset = id - iter->first;

  if (offset < 0) {
    std::cout << "ERROR, negative offset in find_row_for_id for id="<<id<<std::endl;
    return -99;
  }

  return iter->second + offset;
}

}//namespace miniFE

#endif

