
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
#include <iostream>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include <utest_case.hpp>
#include <utest_cases.hpp>

int main(int argc, char** argv) {

#ifdef HAVE_MPI
  MPI_Init(&argc, &argv);
#endif

  //utest_case.hpp declares the 'get_utest_cases' function.

  std::vector<utest_case*>& utest_cases = get_utest_cases();
  bool tests_passed = true;

  for(size_t i=0; i<utest_cases.size(); ++i) {
    bool passed = utest_cases[i]->run();
    if (passed) std::cout << "   pass: " << utest_cases[i]->name() << std::endl;
    else {
      std::cout << "!!!FAIL: " << utest_cases[i]->name() << std::endl;
      tests_passed = false;
    }
  }

  if (!tests_passed) {
    std::cout << "at least 1 test failed."<<std::endl;
  }

#ifdef HAVE_MPI
  MPI_Finalize();
#endif

  return 0;
}

