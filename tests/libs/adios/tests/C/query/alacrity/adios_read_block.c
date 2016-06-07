/*
 * read_block.c
 *
 *  Created on: Dec 5, 2014
 *      Author: xczou
 */


#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include "adios_selection.h"
#include "adios_query.h"
#include "adios_error.h"
#include <mxml.h>
#include <sys/stat.h>
#include "adios_query_xml_parse.h"

int main(int argc, char ** argv) {

    enum ADIOS_READ_METHOD method = ADIOS_READ_METHOD_BP;

    MPI_Comm comm = MPI_COMM_WORLD;

    ADIOS_QUERY_TEST_INFO *queryInfo;
    ADIOS_FILE *f;


    MPI_Init(&argc, &argv);


   f = adios_read_open_file(argv[1], method, comm);
   if (f == NULL) {
	   fprintf(stderr," can not open file %s \n", argv[1]);
	   MPI_Abort(comm, 1);
   }

   int block = 111;

   if (argc >=3) {
	   block = atoi(argv[2]);
   }
	void *data = malloc(sizeof(double) * 44*36*22);

   ADIOS_SELECTION* currBatch = adios_selection_writeblock(block);

   adios_schedule_read (f, currBatch , "/temp", 0, 1, data);
   adios_perform_reads(f, 1);

   int i = 0;
   for (i = 0; i <44 * 36 *22; i++) {
        fprintf(stdout,"%.6f\n", ((double*)data)[i]);
   }

   adios_selection_delete(currBatch);
   free(data);
   adios_read_close(f);
   adios_read_finalize_method(ADIOS_READ_METHOD_BP);

   MPI_Finalize();

}
