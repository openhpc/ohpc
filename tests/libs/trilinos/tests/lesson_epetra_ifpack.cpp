//
// https://code.google.com/p/trilinos/wiki/IfpackFactory
//
#include "Ifpack_ConfigDefs.h"

#ifdef HAVE_MPI
#include "Epetra_MpiComm.h"
#else
#include "Epetra_SerialComm.h"
#endif
#include "Epetra_CrsMatrix.h"
#include "Epetra_MultiVector.h"
#include "Epetra_LinearProblem.h"
#include "Galeri_Maps.h"
#include "Galeri_CrsMatrices.h"
#include "Teuchos_ParameterList.hpp"
#include "Teuchos_RCP.hpp"
#include "Ifpack.h"
#include "Ifpack_AdditiveSchwarz.h"

int 
main (int argc, char *argv[])
{
  using Teuchos::ParameterList;
  using Teuchos::RCP;
  using Teuchos::rcp;

#ifdef HAVE_MPI
  MPI_Init (&argc,&argv);
  Epetra_MpiComm Comm (MPI_COMM_WORLD);
#else
  Epetra_SerialComm Comm;
#endif

  ParameterList GaleriList;

  // The problem is defined on a 2D grid, global size is nx * nx.
  const int nx = 30; 
  GaleriList.set("n", nx * nx);
  GaleriList.set("nx", nx);
  GaleriList.set("ny", nx);
  RCP<Epetra_Map> Map = rcp (Galeri::CreateMap ("Linear", Comm, GaleriList));
  // The "&*Map" expression turns the RCP<Epetra_Map> into an
  // Epetra_Map* (a raw pointer), since that's what Galeri expects).
  RCP<Epetra_RowMatrix> A = 
    rcp (Galeri::CreateCrsMatrix ("Laplace2D", &*Map, GaleriList));

  // =============================================================== //
  // B E G I N N I N G   O F   I F P A C K   C O N S T R U C T I O N //
  // =============================================================== //

  // Allocate an IFPACK factory.  No data is associated with this
  // object.  It only exposes the Create() method.
  Ifpack Factory;

  //
  // Create the preconditioner using the Factory.  Please check the
  // IFPACK documentation for valid values of PrecType.
  //
  std::string PrecType = "ILU"; // Incomplete LU factorization
  int OverlapLevel = 1;    // Must be >= 0. Ignored if Comm.NumProc() == 1.
  RCP<Ifpack_Preconditioner> Prec = 
    rcp (Factory.Create (PrecType, &*A, OverlapLevel));

  //
  // Set parameters for configuring the IFPACK preconditioner.
  //

  // List of parameters, to be filled in below.
  ParameterList List;

  // Specify parameters for ILU: drop tolerance, and fill level.
  List.set ("fact: drop tolerance", 1e-9);
  List.set ("fact: level-of-fill", 1);

  // IFPACK's implementation of overlapping Schwarz domain
  // decomposition offers different modes for combining overlapping
  // values on different MPI processes:
  // 
  // "Add", "Zero", "Insert", "InsertAdd", "Average", "AbsMax"
  //
  // See Epetra_CombineMode.h for the meaning of each of these modes.
  List.set ("schwarz: combine mode", "Add");

  // Set the parameters.
  IFPACK_CHK_ERR(Prec->SetParameters(List));

  // Initialize the preconditioner. At this point the matrix must have
  // been FillComplete()'d, but the values of its entries are ignored.
  IFPACK_CHK_ERR(Prec->Initialize());

  // Build the preconditioner by examining the values in the matrix.
  IFPACK_CHK_ERR(Prec->Compute());

  // =================================================== //
  // E N D   O F   I F P A C K   C O N S T R U C T I O N //
  // =================================================== //

  // IFPACK preconditioners know how to print themselves in parallel.
  std::cout << *Prec;

#ifdef HAVE_MPI
  MPI_Finalize() ; 
#endif
  return EXIT_SUCCESS;
}
