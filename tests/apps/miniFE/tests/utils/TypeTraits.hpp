#ifndef _TypeTraits_hpp_
#define _TypeTraits_hpp_

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

#include <complex>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

namespace miniFE {

template<typename T> struct TypeTraits {};

template<>
struct TypeTraits<float> {
  typedef float magnitude_type;

  static const char* name() {return "float";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_FLOAT;}
#endif
};

template<>
struct TypeTraits<double> {
  typedef double magnitude_type;

  static const char* name() {return "double";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_DOUBLE;}
#endif
};

template<>
struct TypeTraits<int> {
  typedef int magnitude_type;

  static const char* name() {return "int";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_INT;}
#endif
};

template<>
struct TypeTraits<long int> {
  typedef long int magnitude_type;

  static const char* name() {return "long int";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_LONG;}
#endif
};

#ifndef MINIFE_NO_LONG_LONG

template<>
struct TypeTraits<long long> {
  typedef long long magnitude_type;

  static const char* name() {return "long long";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_LONG_LONG;}
#endif
};

#endif

template<>
struct TypeTraits<unsigned> {
  typedef unsigned magnitude_type;

  static const char* name() {return "unsigned";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_UNSIGNED;}
#endif
};

template<>
struct TypeTraits<std::complex<float> > {
  typedef float magnitude_type;

  static const char* name() {return "std::complex<float>";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_COMPLEX;}
#endif
};

template<>
struct TypeTraits<std::complex<double> > {
  typedef double magnitude_type;

  static const char* name() {return "std::complex<double>";}

#ifdef HAVE_MPI
  static MPI_Datatype mpi_type() {return MPI_DOUBLE_COMPLEX;}
#endif
};

}//namespace miniFE

#endif

