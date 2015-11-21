
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

#include <string>
#include <iostream>
#include <sstream>
#include <fstream>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#ifdef MINIFE_HAVE_TPI
#include <TPI.h>
#endif

#ifdef MINIFE_HAVE_TBB
#include <tbb/task_scheduler_init.h>
#endif

#include <param_utils.hpp>
#include <Parameters.hpp>
#include <utils.hpp>

namespace miniFE {

//-------------------------------------------------------------
void get_parameters(int argc, char** argv, Parameters& params)
{
  std::string argstring;
  Mantevo::read_args_into_string(argc, argv, argstring);

  std::string garbage("garbage");
  std::string filename =
      Mantevo::parse_parameter<std::string>(argstring, "input_file", garbage);

  if (filename != garbage) {
    Mantevo::read_file_into_string(filename, argstring);
  }

  params.nx = Mantevo::parse_parameter<int>(argstring, "nx", 10);
  params.ny = Mantevo::parse_parameter<int>(argstring, "ny", params.nx);
  params.nz = Mantevo::parse_parameter<int>(argstring, "nz", params.ny);
  params.load_imbalance =
      Mantevo::parse_parameter<float>(argstring, "load_imbalance", 0);
  params.numthreads = Mantevo::parse_parameter<int>(argstring, "numthreads", 1);
  params.mv_overlap_comm_comp = Mantevo::parse_parameter<int>(argstring, "mv_overlap_comm_comp", 0);
  params.use_locking = Mantevo::parse_parameter<int>(argstring, "use_locking", 0);
  params.name = Mantevo::parse_parameter<std::string>(argstring, "name","");
  params.elem_group_size = Mantevo::parse_parameter<int>(argstring, "elem_group_size", 1);
  params.use_elem_mat_fields = Mantevo::parse_parameter<int>(argstring, "use_elem_mat_fields", 1);
  params.verify_solution = Mantevo::parse_parameter<int>(argstring, "verify_solution", 0);
  params.device = Mantevo::parse_parameter<int>(argstring, "device", 0);
  params.num_devices = Mantevo::parse_parameter<int>(argstring, "num_devices", 2);
  params.skip_device = Mantevo::parse_parameter<int>(argstring, "skip_device", 9999);
  params.numa = Mantevo::parse_parameter<int>(argstring, "numa", 1);
}

//-------------------------------------------------------------
void broadcast_parameters(Parameters& params)
{
#ifdef HAVE_MPI
  const int num_int_params = 13;
  int iparams[num_int_params] = {params.nx, params.ny, params.nz, params.numthreads, params.mv_overlap_comm_comp, params.use_locking,
		     params.elem_group_size, params.use_elem_mat_fields, params.verify_solution,
		     params.device, params.num_devices,params.skip_device,params.numa};
  MPI_Bcast(&iparams[0], num_int_params, MPI_INT, 0, MPI_COMM_WORLD);
  params.nx = iparams[0];
  params.ny = iparams[1];
  params.nz = iparams[2];
  params.numthreads = iparams[3];
  params.mv_overlap_comm_comp = iparams[4];
  params.use_locking = iparams[5];
  params.elem_group_size = iparams[6];
  params.use_elem_mat_fields = iparams[7];
  params.verify_solution = iparams[8];
  params.device = iparams[9];
  params.num_devices = iparams[10];
  params.skip_device = iparams[11];
  params.numa = iparams[12];

  float fparams[1] = {params.load_imbalance};
  MPI_Bcast(&fparams[0], 1, MPI_FLOAT, 0, MPI_COMM_WORLD);
  params.load_imbalance = fparams[0];

#endif
}

//-------------------------------------------------------------
void initialize_mpi(int argc, char** argv, int& numprocs, int& myproc)
{
#ifdef HAVE_MPI
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myproc);
#else
  numprocs = 1;
  myproc = 0;
#endif
}

//-------------------------------------------------------------
void finalize_mpi()
{
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
}

}//namespace miniFE

