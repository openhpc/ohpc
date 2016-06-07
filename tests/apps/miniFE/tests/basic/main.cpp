
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
#include <ctime>
#include <cstdlib>
#include <vector>

#include <miniFE_version.h>

#include <outstream.hpp>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

//--------------------------------------------------------------------
#include <ComputeNodeType.hpp>
//--------------------------------------------------------------------

#include <Box.hpp>
#include <BoxPartition.hpp>
#include <box_utils.hpp>
#include <Parameters.hpp>
#include <utils.hpp>
#include <driver.hpp>
#include <YAML_Doc.hpp>

#if MINIFE_INFO != 0
#include <miniFE_info.hpp>
#else
#include <miniFE_no_info.hpp>
#endif

//The following macros should be specified as compile-macros in the
//makefile. They are defaulted here just in case...
#ifndef MINIFE_SCALAR
#define MINIFE_SCALAR double
#endif
#ifndef MINIFE_LOCAL_ORDINAL
#define MINIFE_LOCAL_ORDINAL int
#endif
#ifndef MINIFE_GLOBAL_ORDINAL
#define MINIFE_GLOBAL_ORDINAL int
#endif

// ************************************************************************

void add_params_to_yaml(YAML_Doc& doc, miniFE::Parameters& params);
void add_configuration_to_yaml(YAML_Doc& doc, int numprocs, int numthreads);
void add_timestring_to_yaml(YAML_Doc& doc);

inline void print_box(int myproc, const char* name, const Box& box,
                      const char* name2, const Box& box2)
{
  std::cout << "proc " << myproc << " "<<name
      <<" ("<<box[0][0]<<","<<box[0][1]<<") "
      <<" ("<<box[1][0]<<","<<box[1][1]<<") "
      <<" ("<<box[2][0]<<","<<box[2][1]<<") "
      <<name2
      <<" ("<<box2[0][0]<<","<<box2[0][1]<<") "
      <<" ("<<box2[1][0]<<","<<box2[1][1]<<") "
      <<" ("<<box2[2][0]<<","<<box2[2][1]<<") "<<std::endl;
}

//
//We will create a 'box' of size nx X ny X nz, partition it among processors,
//then call miniFE::driver which will use the partitioned box as the domain
//from which to assemble finite-element matrices into a global matrix and
//vector, then solve the linear-system using Conjugate Gradients.
//

int main(int argc, char** argv) {
  miniFE::Parameters params;
  miniFE::get_parameters(argc, argv, params);

  int numprocs = 1, myproc = 0;
  miniFE::initialize_mpi(argc, argv, numprocs, myproc);

  miniFE::timer_type start_time = miniFE::mytimer();

#ifdef MINIFE_DEBUG
  outstream(numprocs, myproc);
#endif

  //make sure each processor has the same parameters:
  miniFE::broadcast_parameters(params);


  Box global_box = { 0, params.nx, 0, params.ny, 0, params.nz };
  std::vector<Box> local_boxes(numprocs);

  box_partition(0, numprocs, 2, global_box, &local_boxes[0]);

  Box& my_box = local_boxes[myproc];

//print_box(myproc, "global-box", global_box, "local-box", my_box);

  std::ostringstream osstr;
  osstr << "miniFE." << params.nx << "x" << params.ny << "x" << params.nz;
#ifdef HAVE_MPI
  osstr << ".P"<<numprocs;
#endif
#if defined(MINIFE_HAVE_TPI) || defined(MINIFE_HAVE_TBB)
  osstr << "xT"<<params.numthreads;
#endif
  osstr << ".";
  if (params.name != "") osstr << params.name << ".";

  YAML_Doc doc("miniFE", MINIFE_VERSION, ".", osstr.str());
  if (myproc == 0) {
    add_params_to_yaml(doc, params);
    add_configuration_to_yaml(doc, numprocs, params.numthreads);
    add_timestring_to_yaml(doc);
  }

#if defined(MINIFE_HAVE_TBB)
  TBBNode compute_node(params.numthreads);
#ifdef MINIFE_HAVE_CUDA
  CUDANode::singleton(0,8,512);
#endif
#elif defined(MINIFE_HAVE_TPI)
  TPINode compute_node(params.numthreads);
#elif defined(MINIFE_HAVE_CUDA)
  CUDANode compute_node(0,2,64);
#else
  SerialComputeNode compute_node;
#endif

  //Most of the program is performed in the 'driver' function, which is
  //templated on < Scalar, LocalOrdinal, GlobalOrdinal, NodeType >.
  //To run miniFE with float instead of double, or 'long long' instead of int,
  //etc., change these template-parameters by changing the macro definitions in
  //the makefile or on the make command-line.

  miniFE::driver< MINIFE_SCALAR, MINIFE_LOCAL_ORDINAL, MINIFE_GLOBAL_ORDINAL,
                  ComputeNodeType>(global_box, my_box, compute_node, params, doc);

  miniFE::timer_type total_time = miniFE::mytimer() - start_time;

  if (myproc == 0) {
    doc.add("Total Program Time",total_time);
    std::cout << doc.generateYAML() << std::endl;
  }

  miniFE::finalize_mpi();

  return 0;
}

void add_params_to_yaml(YAML_Doc& doc, miniFE::Parameters& params)
{
  doc.add("Global Run Parameters","");
  doc.get("Global Run Parameters")->add("dimensions","");
  doc.get("Global Run Parameters")->get("dimensions")->add("nx",params.nx);
  doc.get("Global Run Parameters")->get("dimensions")->add("ny",params.ny);
  doc.get("Global Run Parameters")->get("dimensions")->add("nz",params.nz);
  doc.get("Global Run Parameters")->add("load_imbalance", params.load_imbalance);
  if (params.mv_overlap_comm_comp == 1) {
    std::string val("1 (yes)");
    doc.get("Global Run Parameters")->add("mv_overlap_comm_comp", val);
  }
  else {
    std::string val("0 (no)");
    doc.get("Global Run Parameters")->add("mv_overlap_comm_comp", val);
  }
}

void add_configuration_to_yaml(YAML_Doc& doc, int numprocs, int numthreads)
{
  doc.get("Global Run Parameters")->add("number of processors", numprocs);
  std::string threading("none");

#ifdef MINIFE_HAVE_TPI
  threading = "TPI";
#endif
#ifdef MINIFE_HAVE_TBB
  threading = "TBB";
#endif
#ifdef MINIFE_HAVE_CUDA
  threading = "CUDA";
#endif
  if (threading != "none") {
    doc.get("Global Run Parameters")->add("(per proc) numthreads",numthreads);
  }

  doc.add("Platform","");
  doc.get("Platform")->add("hostname",MINIFE_HOSTNAME);
  doc.get("Platform")->add("kernel name",MINIFE_KERNEL_NAME);
  doc.get("Platform")->add("kernel release",MINIFE_KERNEL_RELEASE);
  doc.get("Platform")->add("processor",MINIFE_PROCESSOR);

  doc.add("Build","");
  doc.get("Build")->add("CXX",MINIFE_CXX);
  doc.get("Build")->add("compiler version",MINIFE_CXX_VERSION);
  doc.get("Build")->add("CXXFLAGS",MINIFE_CXXFLAGS);
  std::string using_mpi("no");
#ifdef HAVE_MPI
  using_mpi = "yes";
#endif
  doc.get("Build")->add("using MPI",using_mpi);
  doc.get("Build")->add("Threading",threading.c_str());
}

void add_timestring_to_yaml(YAML_Doc& doc)
{
  std::time_t rawtime;
  struct tm * timeinfo;
  std::time(&rawtime);
  timeinfo = std::localtime(&rawtime);
  std::ostringstream osstr;
  osstr.fill('0');
  osstr << timeinfo->tm_year+1900 << "-";
  osstr.width(2); osstr << timeinfo->tm_mon+1 << "-";
  osstr.width(2); osstr << timeinfo->tm_mday << ", ";
  osstr.width(2); osstr << timeinfo->tm_hour << "-";
  osstr.width(2); osstr << timeinfo->tm_min << "-";
  osstr.width(2); osstr << timeinfo->tm_sec;
  std::string timestring = osstr.str();
  doc.add("Run Date/Time",timestring);
}

