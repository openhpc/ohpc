/*
 * adios_show_bp_file.c
 *
 *  Created on: Jun 21, 2014
 *      Author: xczou
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include "mpi.h"
#include "adios_read.h"
#include "adios_read_ext.h"

void showBPFile(ADIOS_FILE * fp, char *varName );

void showBPFile(ADIOS_FILE * fp, char *varName ){
	int i  = 0, j = 0;
	 printf("adios file : ");
	    printf("nvar %d", fp->nvars);
	    for(i=0; i < fp->nvars; i++) {
	    	printf(" { %s }", fp->var_namelist[i]);
	    }
	    printf(" || nattrs %d", fp->nattrs);
	    for(i=0; i < fp->nattrs; i++) {
	       	printf(" { %s }", fp->attr_namelist[i]);
	    }
	    printf("\n");


		ADIOS_VARINFO* v = adios_inq_var(fp, varName);
		if (v == NULL) {
			printf(" Error! no such var:%s \n", varName);
			return ;
		}

		printf("var info: ");
		printf("varId: %d, type: %d, ndim: %d, nsteps: %d, total nblocks: %d, ",
				v->varid, v->type, v->ndim, v->nsteps, v->sum_nblocks);

		printf("dims: {");
		for(i = 0; i < v->ndim; i++){
			printf("%" PRIu64 ",", v->dims[i]);
		}
		printf("}\n");

		i = adios_inq_var_blockinfo(fp, v);
		if (i != 0){
			printf("error from adios_inq_var_blockinfo \n");
			return ;
		}

		printf("number of blocks in each timestep: {");
		for(i = 0 ; i < v->nsteps; i++){
			printf ("%d,", v->nblocks[i]); // it is an array of length n->nsteps;
		}
		printf("}\n");

		ADIOS_VARBLOCK * bi = v->blockinfo;
		printf("block info: ");
		for(i=0; i< v->sum_nblocks; i++){
			printf("{ ");
			for(j = 0; j < v->ndim; j ++){
				printf("%" PRIu64 ": %"PRIu64 " , ", bi[i].start[j], bi[i].count[j]);
			}
			printf(" }");
		}
		printf("\n");
}


/*
 * RUN with one processor is enough
 */
int main (int argc, char ** argv)
{

    char        filename [256];
    int         i, j, datasize, if_any;
    MPI_Comm    comm = MPI_COMM_WORLD;
    enum ADIOS_READ_METHOD method = ADIOS_READ_METHOD_BP;
    ADIOS_SELECTION * sel1, * sel2;
    ADIOS_VARCHUNK * chunk = 0;
    double * data = NULL;
    uint64_t start[2], count[2], npoints, * points;

    MPI_Init (&argc, &argv);

    if (argc < 3 ){
    	printf(" usage: %s {input bp file} {variable name} \n", argv[0]);
    	return 1;
    }
    adios_read_init_method (method, comm, NULL);

    ADIOS_FILE * fp = adios_read_open_file (argv[1], method, comm);

    if ( fp == NULL){
    	printf(" can not open file %s \n", argv[1]);
    	return 1;
    }

    char varName[256];
    strcpy(varName, argv[2]);
    ADIOS_VARINFO* v = adios_inq_var(fp, varName);
    showBPFile(fp, varName);

    adios_read_close (fp);

    adios_read_finalize_method (ADIOS_READ_METHOD_BP);


    MPI_Finalize ();
    return 0;
}

