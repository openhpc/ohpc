
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

#include <iostream>
#include <ctime>
#include <cstdlib>
#include <vector>

#include <miniFE_version.h>

#include <outstream.hpp>

#ifdef _OPENMP
#include <omp.h>
#endif

#ifdef HAVE_MPI
#include <mpi.h>
#endif

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

  if(myproc==0) {
    std::cout << "MiniFE Mini-App, OpenMP Peer Implementation" << std::endl;
    std::cout << "Creating OpenMP Thread Pool..." << std::endl;
  }
  int value = 0;

#ifdef _OPENMP
  const int thread_count = omp_get_max_threads();
#else
  const int thread_count = 1;
#endif

#pragma omp parallel for reduction(+:value)
  for(int i = 0; i < thread_count; ++i) {
	value += 1;
  }
  double global_threadcount;
  double local_threadcount = value;

#ifdef HAVE_MPI
  MPI_Allreduce(&local_threadcount,&global_threadcount,1,MPI_DOUBLE,MPI_SUM,MPI_COMM_WORLD);
#else
  global_threadcount = local_threadcount;
#endif
  if(myproc==0) {
    std::cout << "Counted: " << global_threadcount << " threads." << std::endl;
    std::cout << "Running MiniFE Mini-App..." << std::endl;
  }

  //make sure each processor has the same parameters:
  miniFE::broadcast_parameters(params);


  Box global_box = { 0, params.nx, 0, params.ny, 0, params.nz };
  std::vector<Box> local_boxes(numprocs);

  box_partition(0, numprocs, 2, global_box, &local_boxes[0]);

  Box& my_box = local_boxes[myproc];

  MINIFE_GLOBAL_ORDINAL num_my_ids = miniFE::get_num_ids<MINIFE_GLOBAL_ORDINAL>(my_box);
  MINIFE_GLOBAL_ORDINAL min_ids = num_my_ids;

#ifdef HAVE_MPI
  MPI_Datatype mpi_dtype = miniFE::TypeTraits<MINIFE_GLOBAL_ORDINAL>::mpi_type();
  MPI_Allreduce(&num_my_ids, &min_ids, 1, mpi_dtype, MPI_MIN, MPI_COMM_WORLD);
#endif

  if (min_ids == 0) {
    std::cout<<"One or more processors have 0 equations. Not currently supported. Exiting."<<std::endl;

    miniFE::finalize_mpi();

    return 1;
  }

  std::ostringstream osstr;
  osstr << "miniFE." << params.nx << "x" << params.ny << "x" << params.nz;
#ifdef HAVE_MPI
  osstr << ".P"<<numprocs;
#endif
  osstr << ".";
  if (params.name != "") osstr << params.name << ".";

  YAML_Doc doc("miniFE", MINIFE_VERSION, ".", osstr.str());
  if (myproc == 0) {
    add_params_to_yaml(doc, params);
    add_configuration_to_yaml(doc, numprocs, params.numthreads);
    add_timestring_to_yaml(doc);
  }

  //Most of the program is performed in the 'driver' function, which is
  //templated on < Scalar, LocalOrdinal, GlobalOrdinal >.
  //To run miniFE with float instead of double, or 'long long' instead of int,
  //etc., change these template-parameters by changing the macro definitions in
  //the makefile or on the make command-line.

  int return_code =
     miniFE::driver< MINIFE_SCALAR, MINIFE_LOCAL_ORDINAL, MINIFE_GLOBAL_ORDINAL>(global_box, my_box, params, doc);

  miniFE::timer_type total_time = miniFE::mytimer() - start_time;

  if (myproc == 0) {
    doc.add("Total Program Time",total_time);
    doc.generateYAML();
  }

  miniFE::finalize_mpi();

  return return_code;
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

  doc.add("Platform","");
  doc.get("Platform")->add("hostname",MINIFE_HOSTNAME);
  doc.get("Platform")->add("kernel name",MINIFE_KERNEL_NAME);
  doc.get("Platform")->add("kernel release",MINIFE_KERNEL_RELEASE);
  doc.get("Platform")->add("processor",MINIFE_PROCESSOR);

  doc.add("Build","");
  doc.get("Build")->add("CXX",MINIFE_CXX);
#if MINIFE_INFO != 0
  doc.get("Build")->add("compiler version",MINIFE_CXX_VERSION);
#endif
  doc.get("Build")->add("CXXFLAGS",MINIFE_CXXFLAGS);
  std::string using_mpi("no");
#ifdef HAVE_MPI
  using_mpi = "yes";
#endif

#ifdef _OPENMP
  doc.get("Build")->add("using OpenMP", "yes");
#else
  doc.get("Build")->add("using OpenMP", "no");
#endif
  doc.get("Build")->add("using MPI",using_mpi);
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

