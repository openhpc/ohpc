#ifndef _perform_element_loop_TBB_pipe_hpp_
#define _perform_element_loop_TBB_pipe_hpp_

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

#ifdef MINIFE_HAVE_TBB

#include <LockingMatrix.hpp>
#include <LockingVector.hpp>
#include <BoxIterator.hpp>
#include <simple_mesh_description.hpp>
#include <SparseMatrix_functions.hpp>
#include <Hex8_box_utils.hpp>
#include <Hex8_ElemData.hpp>

#include <tbb/pipeline.h>

namespace miniFE {

//---------------------------------------------------------------------
//This file contains three 'filter' classes, and a 'perform_element_loop'
//function that uses those filter classes to run a TBB pipeline.
//
//The filter classes are as follows:
//1. GetElemNodesCoords
//     For each element in the mesh, create an elem-data object with coords
//     and node-ids.
//2. Compute_FE_Operators
//     Given an elem-data object (with coords and node-ids), compute the
//     diffusion-matrix and source-vector.
//3. LockingSumIntoLinearSystem
//     Given an elem-data object (with diffusion-matrix and source-vector),
//     assemble into global-sparse linear-system. Uses a lock on each
//     matrix row to ensure that multiple threads don't update the same row
//     at the same time.
//... or:
//3. SumIntoLinearSystem
//     Given an elem-data object (with diffusion-matrix and source-vector),
//     assemble into global-sparse linear-system.
//     There are several of these filters, usually 1 per thread, and each
//     will be responsible for a certain slice of equations. It will check
//     the elem-data for equations that are in its slice, assemble those, and
//     pass the elem-data on so that the next SumIntoLinearSystem filter can
//     deal with equations in a different 'slice'.
//
//---------------------------------------------------------------------

//---------------------------------------------------------------------

/** Filter 1.: GetElemNodesCoords
 */
template<typename GlobalOrdinal,typename Scalar>
class GetElemNodesCoords : public tbb::filter {
public:
  GetElemNodesCoords(const std::vector<GlobalOrdinal>& elemIDs,
                     const simple_mesh_description<GlobalOrdinal>& mesh,
                     size_t num_elems_at_a_time)
   : tbb::filter(/*is_serial=*/true),
     elemIDs_(elemIDs),
     i_(0),
     mesh_(mesh),
     num_elems_(num_elems_at_a_time)
  {
    if (num_elems_ < 1) num_elems_ = 1;
  }

  ~GetElemNodesCoords(){}

private:
  /** This operator launches an elem-data object for a 'group' (size num_elems_)
    * of elements. When all elements have been launched, return NULL to signal
    * that we're done issuing data.
   */
  void* operator()(void* item) {
    if (i_ >= elemIDs_.size()) return NULL;

    size_t num = num_elems_;
    if (i_+num > elemIDs_.size()) num = elemIDs_.size() - i_;

    std::vector<ElemData<GlobalOrdinal,Scalar> >* elemdata_vec = new std::vector<ElemData<GlobalOrdinal,Scalar> >(num);

    size_t i=0;
    while (i_ < elemIDs_.size() && i < num) {
      get_elem_nodes_and_coords(mesh_, elemIDs_[i_], (*elemdata_vec)[i]);
      ++i_;
      ++i;
    }

    return elemdata_vec;
  }

  const std::vector<GlobalOrdinal>& elemIDs_;
  size_t i_;
  const simple_mesh_description<GlobalOrdinal>& mesh_;
  size_t num_elems_;
};

//---------------------------------------------------------------------

/** Filter 2.: Compute_FE_Operators
 */
template<typename GlobalOrdinal,typename Scalar>
class Compute_FE_Operators : public tbb::filter {
public:
  Compute_FE_Operators() : tbb::filter(/*is_serial=*/false) {}
  ~Compute_FE_Operators() {}

private:
  /** This operator takes a vector of elem-data objects which are assumed
    * to have nodal-coordinates already populated, and computes the
    * element-diffusion-matrix and element-source-vector for each.
   */
  void* operator()(void* item) {
    if (item == NULL) return NULL;
    std::vector<ElemData<GlobalOrdinal,Scalar> >* elemdata = static_cast<std::vector<ElemData<GlobalOrdinal,Scalar> >*>(item);

    for(size_t i=0; i<elemdata->size(); ++i) {
      compute_element_matrix_and_vector((*elemdata)[i]);
    }
    return elemdata;
  }
};

//---------------------------------------------------------------------

/** Filter 3.: SumIntoLinearSystem
 */
template<typename MatrixType, typename VectorType>
class SumIntoLinearSystem : public tbb::filter {
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

public:
  SumIntoLinearSystem(GlobalOrdinal myFirstRow,
                      GlobalOrdinal myLastRow,
                      MatrixType& mat, VectorType& vec)
   : tbb::filter(/*is_serial=*/true),
     A_(mat), b_(vec),
     myFirstRow_(myFirstRow),
     myLastRow_(myLastRow)
  {
  }

  ~SumIntoLinearSystem() {}

private:
  /** This operator takes a vector of elem-data objects which have an
    * element-diffusion-matrix and source-vector, looks through it for
    * any rows in this filter's slice of the global matrix, assembles
    * those rows into the linear-system, then passes the elem-data object
    * on for use by the next assembly filter.
    * If this assembly filter is responsible for the last slice of the
    * row-space, then this is the last filter and so we delete the
    * elem-data object.
    */
  void* operator()(void* item) {
    if (item == NULL) return NULL;
    std::vector<ElemData<GlobalOrdinal,Scalar> >* elemdata_vec = static_cast<std::vector<ElemData<GlobalOrdinal,Scalar> >*>(item);

    for(size_t e=0; e<elemdata_vec->size(); ++e) {
      ElemData<GlobalOrdinal,Scalar>& elemdata = (*elemdata_vec)[e];
      size_t nnodes = elemdata.nodes_per_elem;
      for(size_t i=0; i<nnodes; ++i) {
        GlobalOrdinal row = elemdata.elem_node_ids[i];
        if (row < myFirstRow_ || row > myLastRow_) continue;
  
        sum_into_row(row, nnodes, elemdata.elem_node_ids,
                     &(elemdata.elem_diffusion_matrix[i*nnodes]), A_);
        sum_into_vector(1, &row, &(elemdata.elem_source_vector[i]), b_);
      }
    }

    if (myLastRow_ >= A_.rows.size()) {
      delete elemdata_vec;
      return NULL;
    }

    return elemdata_vec;
  }

  MatrixType& A_;
  VectorType& b_;
  GlobalOrdinal myFirstRow_;
  GlobalOrdinal myLastRow_;
};

//---------------------------------------------------------------------

static tbb::atomic<size_t> matrix_suminto;

/** Filter 3.: SumIntoLinearSystem with locking
 */
template<typename MatrixType, typename VectorType>
class LockingSumIntoLinearSystem : public tbb::filter {
  typedef typename MatrixType::GlobalOrdinalType GlobalOrdinal;
  typedef typename MatrixType::ScalarType Scalar;

public:
  LockingSumIntoLinearSystem(MatrixType& mat, VectorType& vec)
   : tbb::filter(/*is_serial=*/false),
     A_(mat), b_(vec)
  {
  }

  ~LockingSumIntoLinearSystem() {}

private:
  /** This operator takes a vector of elem-data objects which have an
    * element-diffusion-matrix and source-vector, and assembles into
    * the linear-system, using locking to make sure no other
    * thread is assembling the same global row at the same time.
    */
  void* operator()(void* item) {
    if (item == NULL) return NULL;
    std::vector<ElemData<GlobalOrdinal,Scalar> >* elemdata_vec = static_cast<std::vector<ElemData<GlobalOrdinal,Scalar> >*>(item);

    for(size_t e=0; e<elemdata_vec->size(); ++e) {
      ElemData<GlobalOrdinal,Scalar>& elemdata = (*elemdata_vec)[e];
      size_t nnodes = elemdata.nodes_per_elem;
      size_t offset = 0;
      for(size_t i=0; i<nnodes; ++i) {
        GlobalOrdinal row = elemdata.elem_node_ids[i];
        //The contiguous row starting from the diagonal is the upper triangle.
        const Scalar* row_coefs = &elemdata.elem_diffusion_matrix[offset];
        const GlobalOrdinal* col_inds = &elemdata.elem_node_ids[i];
        size_t row_len = nnodes-i;

        ++matrix_suminto;
  
        A_.sum_in(row, row_len, col_inds, row_coefs);

        //Now we have to loop to sum in the lower triangle:
        for(size_t j=i+1; j<nnodes; ++j) {
          const Scalar* row_coef = &row_coefs[j];
          const GlobalOrdinal* col = &col_inds[j];
          A_.sum_in(*col, 1, &row, row_coef);
        }

        b_.sum_in(1, &row, &(elemdata.elem_source_vector[i]));
      }
    }

    delete elemdata_vec;
    return NULL;
  }

  LockingMatrix<MatrixType> A_;
  LockingVector<VectorType> b_;
};

//---------------------------------------------------------------------

template<typename GlobalOrdinal,
         typename MatrixType, typename VectorType>
void
perform_element_loop(const simple_mesh_description<GlobalOrdinal>& mesh,
                     const Box& local_elem_box,
                     MatrixType& A, VectorType& b,
                     Parameters& params)
{
  typedef typename MatrixType::ScalarType Scalar;

  if (A.rows.size() == 0) return;

  int num_threads = params.numthreads;

  //We will iterate the local-element-box (local portion of the mesh), and
  //assemble the FE operators into the global sparse linear-system.

  tbb::pipeline pipe;
  
  int global_elems_x = mesh.global_box[0][1];
  int global_elems_y = mesh.global_box[1][1];
  int global_elems_z = mesh.global_box[2][1];

  GlobalOrdinal num_elems = get_num_ids<GlobalOrdinal>(local_elem_box);
  std::vector<GlobalOrdinal> elemIDs(num_elems);

  BoxIterator iter = BoxIterator::begin(local_elem_box);
  BoxIterator end  = BoxIterator::end(local_elem_box);

  for(size_t i=0; iter != end; ++iter, ++i) {
    elemIDs[i] = get_id<GlobalOrdinal>(global_elems_x, global_elems_y, global_elems_z,
                                       iter.x, iter.y, -iter.z);
  }

  //Create the first stage of the pipeline, the filter that will
  //launch elem-data from the mesh, through the pipeline.
  GetElemNodesCoords<GlobalOrdinal,Scalar> get_nodes_coords(elemIDs, mesh, params.elem_group_size);

  //Create the second stage of the pipeline, the parallel filter that will
  //compute element-matrices and element-vectors.
  Compute_FE_Operators<GlobalOrdinal,Scalar> fe_ops;

  //Add the filters to the pipeline:
  pipe.add_filter(get_nodes_coords);
  pipe.add_filter(fe_ops);

  LockingSumIntoLinearSystem<MatrixType,VectorType>* sum_into_linsys = NULL;
  size_t num_assembly_filters = 0;
  std::vector<SumIntoLinearSystem<MatrixType,VectorType>*> linsys;

  bool use_locking = params.use_locking==1;
  if (use_locking) {
    sum_into_linsys = new LockingSumIntoLinearSystem<MatrixType,VectorType>(A, b);
    pipe.add_filter(*sum_into_linsys);
  }
  else {
    //If not using locking, create several assembly filters, each of which
    //will be responsible for assembling rows into a certain slice of the
    //global matrix.
  
    num_assembly_filters = num_threads/3;
    if (num_assembly_filters == 0) num_assembly_filters = 1;
    num_assembly_filters = 2;
  
    size_t num_rows = A.rows.size();
    size_t rows_per_thread = num_rows/num_assembly_filters;
    if (num_rows % num_assembly_filters > 0) ++rows_per_thread;
    size_t first_row = A.rows[0];
    for(int i=0; i<num_assembly_filters; ++i) {
      size_t last_row = first_row + rows_per_thread - 1;
      SumIntoLinearSystem<MatrixType,VectorType> * sum_into = new SumIntoLinearSystem<MatrixType,VectorType>(first_row, last_row, A, b);
      linsys.push_back(sum_into);
      pipe.add_filter(*sum_into);
  
      first_row += rows_per_thread;
    }
  }

  //Running the pipeline carries out the element-loop and assembly.
  pipe.run(num_threads);

  pipe.clear();

  if (use_locking) {
    std::cout << "\n{number of matrix conflicts: " << miniFE_num_matrix_conflicts << "}"<<std::endl;
    std::cout << "{number of vector conflicts: " << miniFE_num_vector_conflicts << "}"<<std::endl;
    std::cout << "matrix_suminto: " << matrix_suminto << std::endl;
  }
  else {
    std::cout << "no locking, num-assembly-filters: "<<num_assembly_filters<<std::endl;
  }

  delete sum_into_linsys;
  for(size_t i=0; i<linsys.size(); ++i) delete linsys[i];
}

}//namespace miniFE

#else
#error "ERROR, this file shouldn't be compiled if MINIFE_HAVE_TBB is not defined."
#endif

#endif

