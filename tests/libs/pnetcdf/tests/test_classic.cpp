#include <stdio.h>
#include <string.h>
#include <libgen.h> /* basename() */
#include <iostream>
#include <pnetcdf>
#include <testutils.h>

using namespace std;
using namespace PnetCDF;
using namespace PnetCDF::exceptions;

int main( int argc, char *argv[] )
{
   char filename[256];
   int rank, nerrs=0, verbose=0;

   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   if (argc > 2) {
       if (!rank) printf("Usage: %s [filename]\n",argv[0]);
       MPI_Finalize();
       return 0;
   }
   if (argc == 2) snprintf(filename, 256, "%s", argv[1]);
   else           strcpy(filename, "testfile.nc");

   if (rank == 0) {
       char *cmd_str = (char*)malloc(strlen(argv[0]) + 256);
       sprintf(cmd_str, "*** TESTING C++ %s for creation of classic format file", basename(argv[0]));
       printf("%-66s ------ ", cmd_str);
       free(cmd_str);
   }

   try
   {
      if (verbose) cout << "Test creation of classic format file" << endl;
      {
	 NcmpiFile ncFile(MPI_COMM_WORLD, filename, NcmpiFile::replace,
                          NcmpiFile::classic);
	 NcmpiDim dim1 = ncFile.addDim("dim1",11);
	 NcmpiDim dim2 = ncFile.addDim("dim2");
	 NcmpiDim dim3 = ncFile.addDim("dim3",13);

	 NcmpiVar var_gw  = ncFile.addVar("George_Washington", ncmpiInt, dim1);
	 // add a 2D record variable
	 vector<NcmpiDim> dimArray(2);
	 dimArray[0]=dim2;
	 dimArray[1]=dim1;
	 NcmpiVar varA1_3  = ncFile.addVar("varA1_3", ncmpiInt, dimArray);

	 // ncFile.enddef(); is no need in C++ program

         // and inserting some data that needs leaving the define mode
         if (verbose) cout << "testing the switch to DATA mode..." << endl;
         int arr[] = {1,2,3,4,5,6,7,8,9,10,11};
         var_gw.putVar_all(arr);
      }

      // Now test reading.
      {
	 NcmpiFile ncFile(MPI_COMM_WORLD, filename, NcmpiFile::read);

	 if (ncFile.getVarCount() != 2)
	    throw NcmpiException( "Holy Mother of Pearl!", __FILE__, __LINE__);
      }

      // and redefinition
      {
        NcmpiFile ncFile(MPI_COMM_WORLD, filename, NcmpiFile::write);
        if (verbose) cout << "testing the switch to DEFINE mode..." << endl;
        ncFile.putAtt(string("name"),string("value"));
      }

      if (verbose) cout << "    -----------   passed\n";
   }
   catch(NcmpiException& e)
   {
      cout << e.what() << " error code=" << e.errorCode() << " Error!\n";
      nerrs++;
   }

    MPI_Offset malloc_size, sum_size;
    int err = ncmpi_inq_malloc_size(&malloc_size);
    if (err == NC_NOERR) {
        MPI_Reduce(&malloc_size, &sum_size, 1, MPI_OFFSET, MPI_SUM, 0, MPI_COMM_WORLD);
        if (rank == 0 && sum_size > 0)
            printf("heap memory allocated by PnetCDF internally has %lld bytes yet to be freed\n",
                   sum_size);
    }

    if (rank == 0) {
        if (nerrs) printf(FAIL_STR,nerrs);
        else       printf(PASS_STR);
    }

   MPI_Finalize();
   return 0;
}
