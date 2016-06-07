#ifndef _Vector_functions_hpp_
#define _Vector_functions_hpp_

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
#include <sstream>
#include <fstream>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#ifdef MINIFE_HAVE_TBB
#include <LockingVector.hpp>
#endif

#include <TypeTraits.hpp>
#include <Vector.hpp>

#define MINIFE_MIN(X, Y)  ((X) < (Y) ? (X) : (Y))

namespace miniFE {


template<typename VectorType>
void write_vector(const std::string& filename,
                  const VectorType& vec)
{
  int numprocs = 1, myproc = 0;
#ifdef HAVE_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#endif

  std::ostringstream osstr;
  osstr << filename << "." << numprocs << "." << myproc;
  std::string full_name = osstr.str();
  std::ofstream ofs(full_name.c_str());

  typedef typename VectorType::ScalarType ScalarType;

  const std::vector<ScalarType>& coefs = vec.coefs;
  for(int p=0; p<numprocs; ++p) {
    if (p == myproc) {
      if (p == 0) {
        ofs << vec.local_size << std::endl;
      }
  
      typename VectorType::GlobalOrdinalType first = vec.startIndex;
      for(size_t i=0; i<vec.local_size; ++i) {
        ofs << first+i << " " << coefs[i] << std::endl;
      }
    }
#ifdef HAVE_MPI
    MPI_Barrier(MPI_COMM_WORLD);
#endif
  }
}

template<typename VectorType>
void sum_into_vector(size_t num_indices,
                     const typename VectorType::GlobalOrdinalType* indices,
                     const typename VectorType::ScalarType* coefs,
                     VectorType& vec)
{
  typedef typename VectorType::GlobalOrdinalType GlobalOrdinal;
  typedef typename VectorType::ScalarType Scalar;

  GlobalOrdinal first = vec.startIndex;
  GlobalOrdinal last = first + vec.local_size - 1;

  //std::vector<Scalar>& vec_coefs = vec.coefs;
  MINIFE_SCALAR* vec_coefs = vec.coefs;

  for(size_t i=0; i<num_indices; ++i) {
    if (indices[i] < first || indices[i] > last) continue;
    size_t idx = indices[i] - first;

    #pragma omp atomic
    vec_coefs[idx] += coefs[i];
  }
}

#ifdef MINIFE_HAVE_TBB
template<typename VectorType>
void sum_into_vector(size_t num_indices,
                     const typename VectorType::GlobalOrdinalType* indices,
                     const typename VectorType::ScalarType* coefs,
                     LockingVector<VectorType>& vec)
{
  vec.sum_in(num_indices, indices, coefs);
}
#endif

//------------------------------------------------------------
//Compute the update of a vector with the sum of two scaled vectors where:
//
// w = alpha*x + beta*y
//
// x,y - input vectors
//
// alpha,beta - scalars applied to x and y respectively
//
// w - output vector
//
template<typename VectorType>
void
  waxpby(typename VectorType::ScalarType alpha, const VectorType& x,
         typename VectorType::ScalarType beta, const VectorType& y,
         VectorType& w)
{
  typedef typename VectorType::ScalarType ScalarType;

#ifdef MINIFE_DEBUG_OPENMP
  std::cout << "Starting WAXPBY..." << std::endl;
#endif

#ifdef MINIFE_DEBUG
  if (y.local_size < x.local_size || w.local_size < x.local_size) {
    std::cerr << "miniFE::waxpby ERROR, y and w must be at least as long as x." << std::endl;
    return;
  }
#endif

  const int n = x.local_size;
  const ScalarType* MINIFE_RESTRICT xcoefs __attribute__ ((aligned (64))) = &x.coefs[0];
  const ScalarType* MINIFE_RESTRICT ycoefs __attribute__ ((aligned (64))) = &y.coefs[0];
        ScalarType* MINIFE_RESTRICT wcoefs __attribute__ ((aligned (64))) = &w.coefs[0];

  if(beta == 0.0) {
	if(alpha == 1.0) {
  		#pragma omp parallel for
		#pragma vector nontemporal
		#pragma unroll(8)
  		for(int i=0; i<n; ++i) {
    			wcoefs[i] = xcoefs[i];
  		}
  	} else {
  		#pragma omp parallel for
		#pragma vector nontemporal
		#pragma unroll(8)
  		for(int i=0; i<n; ++i) {
    			wcoefs[i] = alpha * xcoefs[i];
  		}
  	}
  } else {
	if(alpha == 1.0) {
  		#pragma omp parallel for
		#pragma vector nontemporal
		#pragma unroll(8)
  		for(int i=0; i<n; ++i) {
    			wcoefs[i] = xcoefs[i] + beta * ycoefs[i];
  		}
  	} else {
  		#pragma omp parallel for
		#pragma vector nontemporal
		#pragma unroll(8)
  		for(int i=0; i<n; ++i) {
    			wcoefs[i] = alpha * xcoefs[i] + beta * ycoefs[i];
  		}
  	}
  }

#ifdef MINIFE_DEBUG_OPENMP
  std::cout << "Finished WAXPBY." << std::endl;
#endif
}

template<typename VectorType>
void
  daxpby(const MINIFE_SCALAR alpha,
	const VectorType& x,
	const MINIFE_SCALAR beta,
	VectorType& y)
{

  const MINIFE_LOCAL_ORDINAL n = MINIFE_MIN(x.local_size, y.local_size);
  const MINIFE_SCALAR* MINIFE_RESTRICT xcoefs __attribute__ ((aligned (64))) = &x.coefs[0];
        MINIFE_SCALAR* MINIFE_RESTRICT ycoefs __attribute__ ((aligned (64))) = &y.coefs[0];

  if(alpha == 1.0 && beta == 1.0) {
	  #pragma omp parallel for
	  #pragma vector nontemporal
	  #pragma unroll(8)
	  for(int i = 0; i < n; ++i) {
	    ycoefs[i] += xcoefs[i];
  	  }
  } else if (beta == 1.0) {
	  #pragma omp parallel for
	  #pragma vector nontemporal
	  #pragma unroll(8)
	  for(int i = 0; i < n; ++i) {
	    ycoefs[i] += alpha * xcoefs[i];
  	  }
  } else if (alpha == 1.0) {
	  #pragma omp parallel for
	  #pragma vector nontemporal
	  #pragma unroll(8)
	  for(int i = 0; i < n; ++i) {
	    ycoefs[i] = xcoefs[i] + beta * ycoefs[i];
  	  }
  } else if (beta == 0.0) {
	  #pragma omp parallel for
	  #pragma vector nontemporal
	  #pragma unroll(8)
	  for(int i = 0; i < n; ++i) {
	    ycoefs[i] = alpha * xcoefs[i];
  	  }
  } else {
	  #pragma omp parallel for
	  #pragma vector nontemporal
	  #pragma unroll(8)
	  for(int i = 0; i < n; ++i) {
	    ycoefs[i] = alpha * xcoefs[i] + beta * ycoefs[i];
  	  }
  }

}

//-----------------------------------------------------------
//Compute the dot product of two vectors where:
//
// x,y - input vectors
//
// result - return-value
//
template<typename Vector>
MINIFE_SCALAR dot(const Vector& x,
      const Vector& y)
{
  const MINIFE_LOCAL_ORDINAL n = x.local_size;

  typedef typename Vector::ScalarType Scalar;
  typedef typename TypeTraits<typename Vector::ScalarType>::magnitude_type magnitude;

  const Scalar* MINIFE_RESTRICT xcoefs __attribute__ ((aligned (64))) = &x.coefs[0];
  const Scalar* MINIFE_RESTRICT ycoefs __attribute__ ((aligned (64))) = &y.coefs[0];

  MINIFE_SCALAR result = 0;

  #pragma omp parallel for reduction(+:result)
  for(int i=0; i<n; ++i) {
  	result += xcoefs[i] * ycoefs[i];
  }

#ifdef HAVE_MPI
  magnitude local_dot = result, global_dot = 0;
  MPI_Datatype mpi_dtype = TypeTraits<magnitude>::mpi_type();  
  MPI_Allreduce(&local_dot, &global_dot, 1, mpi_dtype, MPI_SUM, MPI_COMM_WORLD);
  return global_dot;
#else
  return result;
#endif
}

template<typename Vector>
MINIFE_SCALAR dot_r2(const Vector& x)
{
#ifdef MINIFE_DEBUG_OPENMP
 	int myrank;
	MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
	std::cout << "[" << myrank << "] Starting dot..." << std::endl;
#endif

  const MINIFE_LOCAL_ORDINAL n = x.local_size;

#ifdef MINIFE_DEBUG
  if (y.local_size < n) {
    std::cerr << "miniFE::dot ERROR, y must be at least as long as x."<<std::endl;
    n = y.local_size;
  }
#endif

  typedef typename Vector::ScalarType Scalar;
  typedef typename TypeTraits<typename Vector::ScalarType>::magnitude_type magnitude;

  const MINIFE_SCALAR* MINIFE_RESTRICT xcoefs __attribute__ ((aligned (64))) = &x.coefs[0];
  MINIFE_SCALAR result = 0;

  #pragma omp parallel for reduction(+:result)
  #pragma unroll(8)
  for(MINIFE_LOCAL_ORDINAL i = 0; i < n; ++i) {
  	result += xcoefs[i] * xcoefs[i];
  }

#ifdef HAVE_MPI
  magnitude local_dot = result, global_dot = 0;
  MPI_Datatype mpi_dtype = TypeTraits<magnitude>::mpi_type();  
  MPI_Allreduce(&local_dot, &global_dot, 1, mpi_dtype, MPI_SUM, MPI_COMM_WORLD);
#ifdef MINIFE_DEBUG_OPENMP
 	std::cout << "[" << myrank << "] Completed dot." << std::endl;
#endif
  return global_dot;
#else
#ifdef MINIFE_DEBUG_OPENMP
 	std::cout << "[" << myrank << "] Completed dot." << std::endl;
#endif
 return result;
#endif
}

}//namespace miniFE

#endif

