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

#ifndef _imbalance_hpp_
#define _imbalance_hpp_

#include <cmath>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include <box_utils.hpp>
#include <utils.hpp>
#include <YAML_Doc.hpp>

namespace miniFE {

const int X = 0;
const int Y = 1;
const int Z = 2;
const int NONE = 3;

const int LOWER = 0;
const int UPPER = 1;

template<typename GlobalOrdinal>
void
compute_imbalance(const Box& global_box,
                  const Box& local_box,
                  float& largest_imbalance,
                  float& std_dev,
                  YAML_Doc& doc,
                  bool record_in_doc)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  GlobalOrdinal local_nrows = get_num_ids<GlobalOrdinal>(local_box);
  GlobalOrdinal min_nrows = 0, max_nrows = 0, global_nrows = 0;
  int min_proc = myproc, max_proc = myproc;
  get_global_min_max(local_nrows, global_nrows, min_nrows, min_proc,
                     max_nrows, max_proc);

  float avg_nrows = global_nrows;
  avg_nrows /= numprocs;

  //largest_imbalance will be the difference between the min (or max)
  //rows-per-processor and avg_nrows, represented as a percentage:
  largest_imbalance = percentage_difference<float>(min_nrows, avg_nrows);

  float tmp = percentage_difference<float>(max_nrows, avg_nrows);
  if (tmp > largest_imbalance) largest_imbalance = tmp;

  std_dev = compute_std_dev_as_percentage<float>(local_nrows, avg_nrows);

  if (myproc == 0 && record_in_doc) {
    doc.add("Rows-per-proc Load Imbalance","");
    doc.get("Rows-per-proc Load Imbalance")->add("Largest (from avg, %)",largest_imbalance);
    doc.get("Rows-per-proc Load Imbalance")->add("Std Dev (%)",std_dev);
  }
}

std::pair<int,int>
decide_how_to_grow(const Box& global_box, const Box& local_box)
{
  std::pair<int,int> result(NONE,UPPER);

  if (local_box[Z][UPPER] < global_box[Z][UPPER]) {
    result.first = Z;
    result.second = UPPER;
    return result;
  }
  if (local_box[Z][LOWER] > global_box[Z][LOWER]) {
    result.first = Z;
    result.second = LOWER;
    return result;
  }
  if (local_box[Y][UPPER] < global_box[Y][UPPER]) {
    result.first = Y;
    result.second = UPPER;
    return result;
  }
  if (local_box[Y][LOWER] > global_box[Y][LOWER]) {
    result.first = Y;
    result.second = LOWER;
    return result;
  }
  if (local_box[X][UPPER] < global_box[X][UPPER]) {
    result.first = X;
    result.second = UPPER;
    return result;
  }
  if (local_box[X][LOWER] > global_box[X][LOWER]) {
    result.first = X;
    result.second = LOWER;
    return result;
  }
  return result;
}

std::pair<int,int>
decide_how_to_shrink(const Box& global_box, const Box& local_box)
{
  std::pair<int,int> result(NONE,UPPER);

  if (local_box[Z][UPPER] < global_box[Z][UPPER] && local_box[Z][UPPER]-local_box[Z][LOWER] > 2) {
    result.first = Z;
    result.second = UPPER;
    return result;
  }
  if (local_box[Z][LOWER] > global_box[Z][LOWER] && local_box[Z][UPPER]-local_box[Z][LOWER] > 2) {
    result.first = Z;
    result.second = LOWER;
    return result;
  }
  if (local_box[Y][UPPER] < global_box[Y][UPPER] && local_box[Y][UPPER]-local_box[Y][LOWER] > 2) {
    result.first = Y;
    result.second = UPPER;
    return result;
  }
  if (local_box[Y][LOWER] > global_box[Y][LOWER] && local_box[Y][UPPER]-local_box[Y][LOWER] > 2) {
    result.first = Y;
    result.second = LOWER;
    return result;
  }
  if (local_box[X][UPPER] < global_box[X][UPPER] && local_box[X][UPPER]-local_box[X][LOWER] > 2) {
    result.first = X;
    result.second = UPPER;
    return result;
  }
  if (local_box[X][LOWER] > global_box[X][LOWER] && local_box[X][UPPER]-local_box[X][LOWER] > 2) {
    result.first = X;
    result.second = LOWER;
    return result;
  }
  return result;
}

template<typename GlobalOrdinal>
void
add_imbalance(const Box& global_box,
              Box& local_box,
              float imbalance,
              YAML_Doc& doc)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  if (numprocs == 1) {
    return;
  }

  float cur_imbalance = 0, cur_std_dev = 0;
  compute_imbalance<GlobalOrdinal>(global_box, local_box,
                                  cur_imbalance, cur_std_dev, doc, false);

  while (cur_imbalance < imbalance) {
    GlobalOrdinal local_nrows = get_num_ids<GlobalOrdinal>(local_box);
    GlobalOrdinal min_nrows = 0, max_nrows = 0, global_nrows = 0;
    int min_proc = myproc, max_proc = myproc;
    get_global_min_max(local_nrows, global_nrows, min_nrows, min_proc,
                       max_nrows, max_proc);

    std::pair<int,int> grow(NONE,UPPER);
    int grow_axis_val = -1;
    std::pair<int,int> shrink(NONE,UPPER);
    int shrink_axis_val = -1;

    if (myproc == max_proc) {
      grow = decide_how_to_grow(global_box, local_box);
      if (grow.first != NONE) {
        grow_axis_val = local_box[grow.first][grow.second];
      }
    }
    if (myproc == min_proc) {
      shrink = decide_how_to_shrink(global_box, local_box);
      if (shrink.first != NONE) {
        shrink_axis_val = local_box[shrink.first][shrink.second];
      }
    }

    int grow_info[8] = {grow.first, grow.second,
                        local_box[X][0], local_box[X][1],
                        local_box[Y][0], local_box[Y][1],
                        local_box[Z][0], local_box[Z][1]};

    int shrink_info[8] = {shrink.first, shrink.second,
                        local_box[X][0], local_box[X][1],
                        local_box[Y][0], local_box[Y][1],
                        local_box[Z][0], local_box[Z][1]};
#ifdef HAVE_MPI
    MPI_Bcast(&grow_info[0], 8, MPI_INT, max_proc, MPI_COMM_WORLD);
    MPI_Bcast(&shrink_info[0], 8, MPI_INT, min_proc, MPI_COMM_WORLD);
#endif

    int grow_axis = grow_info[0];
    int grow_end = grow_info[1];
    int shrink_axis = shrink_info[0];
    int shrink_end = shrink_info[1];
    int grow_incr = 1;
    if (grow_end == LOWER) grow_incr = -1;
    int shrink_incr = -1;
    if (shrink_end == LOWER) shrink_incr = 1;
    if (grow_axis != NONE) grow_axis_val = grow_info[2+grow_axis*2+grow_end];
    if (shrink_axis != NONE) shrink_axis_val = shrink_info[2+shrink_axis*2+shrink_end];

    if (grow_axis == NONE && shrink_axis == NONE) break;

    bool grow_status = grow_axis==NONE ? false : true;
    if (grow_axis != NONE) {
      if ((grow_incr ==  1 && local_box[grow_axis][0] == grow_axis_val) ||
          (grow_incr == -1 && local_box[grow_axis][1] == grow_axis_val)) {
        if (local_box[grow_axis][1] - local_box[grow_axis][0] < 2) {
          grow_status = false;
        }
      }
    }

    bool shrink_status = shrink_axis==NONE ? false : true;
    if (shrink_axis != NONE) {
      if ((shrink_incr ==  1 && local_box[shrink_axis][0] == shrink_axis_val) ||
          (shrink_incr == -1 && local_box[shrink_axis][1] == shrink_axis_val)) {
        if (local_box[shrink_axis][1] - local_box[shrink_axis][0] < 2) {
          shrink_status = false;
        }
      }
    }

#ifdef HAVE_MPI
    int statusints[2] = { grow_status ? 0 : 1, shrink_status ? 0 : 1 };
    int globalstatus[2] = { 0, 0 };
    MPI_Allreduce(&statusints, &globalstatus, 2, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    grow_status = globalstatus[0]>0 ? false : true;
    shrink_status = globalstatus[1]>0 ? false : true;
#endif

    if (grow_status == false && shrink_status == false) break;

    if (grow_status && grow_axis != NONE) {
      if (local_box[grow_axis][0] == grow_axis_val) {
        local_box[grow_axis][0] += grow_incr;
      }

      if (local_box[grow_axis][1] == grow_axis_val) {
        local_box[grow_axis][1] += grow_incr;
      }
    }

    if (shrink_status && shrink_axis != NONE) {
      if (local_box[shrink_axis][0] == shrink_axis_val) {
        local_box[shrink_axis][0] += shrink_incr;
      }

      if (local_box[shrink_axis][1] == shrink_axis_val) {
        local_box[shrink_axis][1] += shrink_incr;
      }
    }

    compute_imbalance<GlobalOrdinal>(global_box, local_box,
                                    cur_imbalance, cur_std_dev, doc, false);
  }
}

}//namespace miniFE

#endif

