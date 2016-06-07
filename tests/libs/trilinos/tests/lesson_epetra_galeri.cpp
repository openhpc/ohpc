//
// https://code.google.com/p/trilinos/wiki/GaleriLinearSystem
//
#include "Galeri_Maps.h"
#include "Galeri_CrsMatrices.h"
#include "Galeri_Utils.h"
#ifdef HAVE_MPI
#include "Epetra_MpiComm.h"
#include "mpi.h"
#else
#include "Epetra_SerialComm.h"
#endif
#include "Epetra_Map.h"
#include "Epetra_CrsMatrix.h"
#include "Teuchos_ParameterList.hpp"
#include "Teuchos_RCP.hpp"

using namespace Galeri;

// =========== //
// main driver //
// =========== //

int main(int argv, char* argc[])
{
  using Teuchos::RCP;
  using Teuchos::rcp;

#ifdef HAVE_MPI
  MPI_Init (&argv, &argc);
  Epetra_MpiComm Comm (MPI_COMM_WORLD);
#else
  Epetra_SerialComm Comm;
#endif

  // Create a parameter list
  Teuchos::ParameterList GaleriList;

  // Set the number of discretization points in the x and y direction.
  GaleriList.set ("nx", 10 * Comm.NumProc ());
  GaleriList.set ("ny", 10);

  // Create the map and matrix using the parameter list for a 2D Laplacian.
  RCP<Epetra_Map> Map = rcp (CreateMap ("Cartesian2D", Comm, GaleriList));
  RCP<Epetra_CrsMatrix> Matrix = rcp (CreateCrsMatrix ("Laplace2D", &*Map, GaleriList));

  // Print out the map and matrices  
  Map->Print (std::cout);
  Matrix->Print (std::cout);

#ifdef HAVE_MPI
  MPI_Finalize ();
#endif
  return 0;
}
