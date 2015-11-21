/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "mpi.h"
#include "adios_read.h"

#define PRINTDIMS(ndim, dims, ivar) if (ndim==0) \
        printf("\tscalar\n"); \
    else { \
        printf("\t%dD variable: [%lld", ndim, dims[0]); \
        for (ivar=1; ivar<ndim; ivar++) printf(", %lld", dims[ivar]); \
        printf("]\n"); \
    }

int main (int argc, char ** argv)
{
    char * filename;
    int    rank, pe_size; 

    int     nvar;
    int     i,j,k;    

    void    * var = NULL;
    MPI_Comm comm; 

    ADIOS_FILE *fp;
    ADIOS_GROUP *gp;
    ADIOS_VARINFO *vi;

    filename = argv[1];
    if (!filename)
        filename = "testbp_c.bp";

    comm = MPI_COMM_WORLD;
    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &pe_size);

    /* Open ADIOS file.
       The index is read only by rank=0 process which is then broadcasted
       to all processes.
    */
    fp = adios_fopen (filename, comm);
    if (fp == NULL) {
        fprintf(stderr, "Error: %s\n", adios_errmsg());
        exit(adios_errno);
    }

    if (rank == 0) 
        adios_print_fileinfo (fp); // function in src/adios_read.c, not exposed in .h files

    /* Open a group in the file.
       A file has one or more adios groups. They are handled separately.
    */
    gp = adios_gopen_byid (fp, 0); // open the first group
    if (gp == NULL) {
        int retval = adios_errno;
        fprintf(stderr, "Error: %s\n", adios_errmsg());
        adios_fclose(fp);
        exit(retval);
    }
    if (rank == 0) 
        adios_print_groupinfo (gp); // function in src/adios_read.c, not exposed in .h files

    /* Read a variable
       Variables should be queried first to get type and size information.
       Then memory should be allocated for the data slice to be read in.
       Then the variable can be read.
    */
    uint64_t start[3], size[3];
    const char *type_str;

    /* variable int_1D */
    vi = adios_inq_var (gp, "int_1D");
    if (vi == NULL) {
        int retval = adios_errno;
        fprintf(stderr, "Error: %s\n", adios_errmsg());
        adios_fclose(fp);
        adios_gclose(gp);
        exit(retval);
    }

    type_str = adios_type_to_string (vi->type);
    printf("%s:\n\thas timesteps: %s\n\ttype: %s\n",
            "int_1D", (vi->timedim > -1 ? "yes" : "no"), type_str);
    PRINTDIMS(vi->ndim, vi->dims, i)
    size[0] = 1;
    start[0] = 0;
    adios_read_var (gp, "int_1D", start, size, &nvar);
    printf("\tfirst element value: %d\n",(nvar));
    free(vi);


    /* variable int_2D */
    vi = adios_inq_var (gp, "int_2D");
    if (vi == NULL) {
        int retval = adios_errno;
        fprintf(stderr, "Error: %s\n", adios_errmsg());
        adios_fclose(fp);
        adios_gclose(gp);
        exit(retval);
    }
    
    type_str = adios_type_to_string (vi->type);
    printf("%s:\n\thas timesteps: %s\n\ttype: %s\n",
            "int_2D", (vi->timedim > -1 ? "yes" : "no"), type_str);
    PRINTDIMS(vi->ndim, vi->dims, i)
    start[0]=0;
    start[1]=0;
    size[0]=10;
    size[1]=2;
    var = malloc (sizeof(int) * size[0]*size[1]);

    // time step should be no less than zero
    // vnamelist[14] is not written at time step 0
    // so the function returns as error
    adios_read_var (gp, "int_2D", start, size, var);
    printf("\tslice (%lld:%lld, %lld:%lld) = ", 
           start[0], start[0]+size[0]-1,
           start[1], start[1]+size[1]-1);
    k=0;
    for (j=0;j<size[0];j++) {
        printf("\n\t");
        for (i=0;i<size[1];i++){
            printf("%d  ",((int*)(var))[k]);
            k++;
        }
    }
    printf("\n");
    free(vi);
    free(var);


    /* variable int_3D */
    vi = adios_inq_var (gp, "int_3D");
    if (vi == NULL) {
        int retval = adios_errno;
        fprintf(stderr, "Error: %s\n", adios_errmsg());
        adios_fclose(fp);
        adios_gclose(gp);
        exit(retval);
    }
    
    type_str = adios_type_to_string (vi->type);
    printf("%s:\n\thas timesteps: %s\n\ttype: %s\n",
            "int_3D", (vi->timedim > -1 ? "yes" : "no"), type_str);
    PRINTDIMS(vi->ndim, vi->dims, i)
    
    start[0]=0;
    start[1]=0;
    start[2]=0;
    size[0]=2;
    size[1]=5;
    size[2]=3;
    var = malloc ( sizeof(int) * size[0]*size[1]*size[2]);
    adios_read_var (gp, "int_3D", start, size, var);
    printf("\t[%lld:%lld, %lld:%lld, %lld:%lld]", 
           start[0], start[0]+size[0]-1,
           start[1], start[1]+size[1]-1,
           start[2], start[2]+size[2]-1
           );
    k=0;
    for (nvar=0;nvar<size[0];nvar++) {
        for (j=0;j<size[1];j++) {
            printf("\n\t");
            for (i=0;i<size[2];i++){
                printf("%d  ",((int*)(var))[k]);
                k++;
            }
        }
        printf("\n");
    }
    free(vi);
    free(var);

    printf("\ndone\n");

        
    adios_gclose (gp);
    adios_fclose (fp);
    MPI_Finalize ();

    return 0;
}

