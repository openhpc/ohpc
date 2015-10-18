/*

   Simple cosine and sine 1D arrays over time from
   a single process.
   Test: can we read back the timed 2D arrays?

*/

/* Include other header files */
#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <fcntl.h>
#include <stdint.h>

/* ADIOS include files */
#include "adios.h"

/* ------------------ Global variables (for main.c only) ------------------ */
// Simulation Variables
//---------------------
char basedatapath[256] ;        // data output files' starting path (without varname)
char baseimgpath[256] ;         // image output files' starting path (without varname)
char outpath1[256] ;         	// data output files' starting path (can be extended with subdirs, names, indexes)
char outpath2[256] ;         	// image output files' starting path (can be extended with subdirs, names, indexes)
char title[256];            	// Title of the (single) plot
char subtitle[256];         	// subtitle of the (single) plot
char xname[256];            	// force to plot this as X axis string
char varname [100];            	// variable name
char vardirpath[256];          	// variable path for a variable name
char txtpath[256];            	// text path for a text file to be registered
char datapath[256];            	// data path for a data file to be registered
char longvarname[256];

// Utility variable to create directories
int res0 = 0;

/* ------------------------------ Prototypes ------------------------------ */
// Print out raw data files (Here simple ASCII files, later BP files)
static int print_data_1D(float timestep, int npoints, float *x, float *y, int output_xml);

/* --------------------------------- Main --------------------------------- */
int main( int argc, char ** argv)
{
    char        filename [256];
    MPI_Comm    comm = MPI_COMM_WORLD;
    int         rank, size;
    /* ADIOS variables declarations for matching gwrite_schema.ch */
    int         adios_err;
    uint64_t    adios_groupsize, adios_totalsize;
    int64_t     adios_handle;
    float       tmax = 10.0;
    float       dt = 0.5;  // run from 0.0 increasing with 'dt' up to 'tmax'
    int         i;

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    adios_init ("local_array_time.xml", comm);
    strcpy(filename, "local_array_time.bp");


    // Declare and Initialize essential variables
    int num_points = 37;
    float angles[num_points];
    float cos_of_angles[num_points];
    float sin_of_angles[num_points];
    float pi;

    // Obtain pi once for all
    pi = 4.0*atan(1.0);

    // Initialize angles in degrees
    float angle_degree = 0;
    for (i=0; i<num_points; i++) {
        angles[i] = pi * angle_degree/180.0;
        angle_degree = angle_degree + 10.0;
    }

    //  Scan over time
    float timestep = 0.0;
    for (timestep = 0.0; timestep <= tmax; timestep = timestep + dt) {

        if (timestep == 0.0) {
            printf("\n\n\nopen file\n\n\n");
            adios_open (&adios_handle, "schema", filename, "w", comm);
        } else {
            adios_open (&adios_handle, "schema", filename, "a", comm);
        }

        for (i=0; i<num_points; i++) {
            cos_of_angles[i] = cos(angles[i]*timestep);
            sin_of_angles[i] = sin(angles[i]*timestep);
        }

        adios_groupsize = 4 + 4 \
                + 4*num_points \
                + 4*num_points;
        if (timestep == 0 && rank == 0) {
            adios_groupsize += 4 + 4 + 4*num_points;
        }
        adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);

        adios_write (adios_handle, "num_points", &num_points);
        adios_write (adios_handle, "t", &timestep);
        if (timestep == 0 && rank == 0) {
            adios_write (adios_handle, "tmax", &tmax);
            adios_write (adios_handle, "dt", &dt);
            adios_write (adios_handle, "angles", angles);
        }
        adios_write (adios_handle, "cos", cos_of_angles);
        adios_write (adios_handle, "sin", sin_of_angles);

        adios_close (adios_handle);

        // Write out raw data
        print_data_1D(timestep, num_points, angles, sin_of_angles, 0);
    }
   
    MPI_Barrier (comm);

    adios_finalize (rank);

    MPI_Finalize ();

    return 0;
}

/**
  Print out a 1D array into ascii or xlm files
  ascii format 1: "X Y"
 */
static int print_data_1D(float timestep, int npoints, float *x, float *y, int output_xml) {
    char fn[256];
    FILE *f;
    int i;
    char xstr[128], ystr[128];
    char *xname = "rad";
    char *yname = "sin(rad)";

    snprintf(fn, 256, "local_array_time.%g.txt", timestep);
    printf ("Write file %s\n", fn);

    if ((f = fopen(fn,"w")) == NULL) {
        fprintf(stderr, "Error at opening for writing file %s: %s\n",
                fn, strerror(errno));
        return 1;
    }

    if (output_xml == 1) {
        fprintf(f, "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n");
        fprintf(f, "<plot xname=\"%s\" yname=\"%s\">\n", xname, yname);
        for (i=0; i<npoints; i++) {
            sprintf( xstr, "%g", x[i] );
            sprintf( ystr, "%g", y[i] );
            fprintf(f,"  <point> <X>%s</X>  <Y>%s</Y> </point>\n", xstr, ystr);
        }
        fprintf(f, "</plot>\n");
    } else if (output_xml == 0){
        for (i=0; i<npoints; i++) {
            fprintf(f, "%g %g\n", x[i], y[i]);
        }
    }

    fclose(f);
    return 0;
}

