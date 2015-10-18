
//@HEADER
// ************************************************************************
// 
//               Mantevo: A collection of mini-applications for HPC
//                 Copyright (2008) Sandia Corporation
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

// Changelog
//
// Version 0.1


#include <string>
#include <iostream>

#include <vector>
#include <map>
#include "vectorTests.hpp"
#include "YAML_Element.hpp"
#include "YAML_Doc.hpp"
#ifdef HAVE_MPI
#include <mpi.h> // If this routine is compiled with -DHAVE_MPI
                 // then include mpi.h
#endif
void addResults(YAML_Element * currentElement, const std::vector<double> & times, double fnops);


#undef DEBUG
int main(int argc, char *argv[]) {
#ifdef HAVE_MPI
  // Initialize MPI
  MPI_Init(&argc, &argv);
  int size, rank; // Number of MPI processes, My process ID
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  // I'm alive !!!
  if (size < 100) std::cout << "Process "<<rank<<" of "<<size<<" is alive." <<std::endl;

#else
  int size = 1; // Serial case (not using MPI)
  int rank = 0; 
#endif
  
  YAML_Doc doc("NewMiniApp","0.1");


#ifdef DEBUG
  int junk = 0;
  std::cout << "Press enter to continue"<< std::endl;
  std::cin >> junk;
#endif

  if(argc != 2) {
    std::cerr << "Usage:" << std::endl
	   << argv[0] << " n" << std::endl
	   << "     where n is the problem size" << std::endl;
    std::exit(1);
  }
  
  size_t n = atoi(argv[1]);
  int numTrials = 1000000/n; if (numTrials<1) numTrials = 1;
  double fnops = 2.0 * ((double) size) *((double) n)*((double) numTrials);
  std::vector<double> times(4,0.0);
  doc.add("Problem_size",n);
  doc.add("Number_of_timing_trials",numTrials);

  std::vector<double> dx(n, 1.0), dy(n, 1.0);

  int ierr = vectorTests<double>(numTrials, dx, dy, times); 

  if (ierr) std::cerr << "Error in call to vectorTests: " << ierr << ".\n" << std::endl;

  if (rank==0) { // Only PE 0 needs to compute and report timing results

      doc.add("Total_time_for_vector_tests_in_double",times[0]);


      doc.add("Double_precision_results","");
      doc.get("Double_precision_results")->add("performance_summary","");
      YAML_Element * currentElement = doc.get("Double_precision_results");
      addResults(currentElement, times, fnops);
  }

#ifdef HAVE_MPI
      MPI_Barrier(MPI_COMM_WORLD);
#endif

  std::vector<float> fx(n, 1.0f), fy(n, 1.0f);
  ierr = vectorTests<float>(numTrials, fx, fy, times);
  if (ierr) std::cerr << "Error in call to vectorTests: " << ierr << ".\n" << std::endl;

  if (rank==0) { // Only PE 0 needs to compute and report timing results

      doc.add("Total_time_for_vector_tests_in_float",times[0]);


      doc.add("Float_precision_results","");
      doc.get("Float_precision_results")->add("performance_summary","");
      YAML_Element * currentElement = doc.get("Float_precision_results");
      addResults(currentElement, times, fnops);
  }

  if (rank==0) { // Only PE 0 needs to compute and report timing results

    std::string yaml = doc.generateYAML();
    std::cout << yaml;
  }
  // Finish up
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  return 0;
} 

void addResults(YAML_Element * currentElement, const std::vector<double> & times, double fnops) {

      currentElement->get("performance_summary")->add("total","");
      currentElement->get("performance_summary")->get("total")->add("time",times[0]);
      currentElement->get("performance_summary")->get("total")->add("flops",3.0*fnops);
      currentElement->get("performance_summary")->get("total")->add("mflops",3.0*fnops/times[0]/1.0E6);

      currentElement->get("performance_summary")->add("std_vector_bracket_notation","");
      currentElement->get("performance_summary")->get("std_vector_bracket_notation")->add("time",times[1]);
      currentElement->get("performance_summary")->get("std_vector_bracket_notation")->add("flops",fnops);
      currentElement->get("performance_summary")->get("std_vector_bracket_notation")->add("mflops",fnops/times[1]/1.0E6);

      currentElement->get("performance_summary")->add("raw_pointer_bracket_notation","");
      currentElement->get("performance_summary")->get("raw_pointer_bracket_notation")->add("time",times[2]);
      currentElement->get("performance_summary")->get("raw_pointer_bracket_notation")->add("flops",fnops);
      currentElement->get("performance_summary")->get("raw_pointer_bracket_notation")->add("mflops",fnops/times[2]/1.0E6);

      currentElement->get("performance_summary")->add("raw_pointer_deref_plusplus_notation","");
      currentElement->get("performance_summary")->get("raw_pointer_deref_plusplus_notation")->add("time",times[3]);
      currentElement->get("performance_summary")->get("raw_pointer_deref_plusplus_notation")->add("flops",fnops);
      currentElement->get("performance_summary")->get("raw_pointer_deref_plusplus_notation")->add("mflops",fnops/times[3]/1.0E6);

      return;
}
