/*
 *  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: csnap.c 2728 2016-12-18 17:49:20Z wkliao $ */


/******************************************************************************

  This code writes one or two arrays, tt[k][j][i] (and smf[j][i], if
  'only_3d' is 0), into the file 'csnap.nc.' It then reads the field(s)
  from the file, and compares with the original field values.
 
  i=longitude, j=latitude, k=level
 
  To run: Set the global sizes, parallel decomposition and other I/O
  parameters below.
 
  By Woo-Sun Yang and Chris Ding
  NERSC, Lawrence Berkeley National Laboratory

 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h> /* srand(), rand() */
#include <string.h> /* strcpy() */
#include <unistd.h>
#include <math.h>  /* sqrt() */
#include <limits.h>
#include <float.h>
#include <mpi.h>
#include <pnetcdf.h>
#include <testutils.h>

#define ERR {if(err!=NC_NOERR){printf("Error at line=%d: %s\n", __LINE__, ncmpi_strerror(err));nerrs++;}}

/*** Field parameters ***/
static int verbose, nerrs;

const MPI_Offset totsiz_3d[3] = { 256, 256, 256 }; /* global sizes of 3D field */
MPI_Offset totsiz_2d[2];                           /* global sizes of 2D field */
MPI_Offset locsiz_3d[3];                           /* local sizes of 3D fields */
MPI_Offset locsiz_2d[2];                           /* local sizes of 2D fields */
MPI_Offset istart, jstart, kstart;                 /* offsets of 3D field */

const int random_fields = 0;                /* random fields? 1 or 0 */
const int only_3d       = 1;                /* I/O 3D field only? 1 or 0 */

int has_2d;                                 /* contains valid 2D data? */

const int nwrites = 5;                      /* number of write samples */
const int nreads  = 5;                      /* number of read samples */

const int fillmode = NC_NOFILL;             /* NC_FILL or NC_NOFILL; actually
                                               prefilling not supported */

/*** Parallel domain decomposition parameters ***/

MPI_Comm comm_cart;                         /* Cartesian communicator */
int mype;                                   /* rank in comm_cart */
int totpes;                                 /* total number of PEs */
int numpes[3] = {   0,   1,   1 };          /* number of PEs along axes;
                                               determined by MPI where
                                               a zero is specified */
int pe_coords[3];                           /* Cartesian PE coords */

/*** function prototypes ***/

void find_locnx(MPI_Offset nx, int mype, int totpes, MPI_Offset *locnx, MPI_Offset *xbegin);
void write_file(char *filename, double *t);
void read_file(char *filename, double *t);
void get_fields(double *tt, double *smf);
void compare_vec(double *a, double *b, int ndims, MPI_Offset *sizes, int corr_data);


int main(int argc, char *argv[]) {
  int isperiodic[3] = {0, 0, 0};
  int reorder = 0;
  double t[20], t_g[20];
  double file_size;
  double rates_l[4], rates_g[4];
  int i, rank;
  char filename[256];

  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&totpes);
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    if (argc > 2) {
        if (!rank) printf("Usage: %s [filename]\n",argv[0]);
        MPI_Finalize();
        return 0;
    }
    if (argc == 2) snprintf(filename, 256, "%s", argv[1]);
    else           strcpy(filename, "testfile.nc");
    MPI_Bcast(filename, 256, MPI_CHAR, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        char *cmd_str = (char*)malloc(strlen(argv[0]) + 256);
        sprintf(cmd_str, "*** TESTING C   %s for 3D array write/read ", argv[0]);
        printf("%-66s ------ ", cmd_str); fflush(stdout);
        free(cmd_str);
    }

  verbose = 0;
  nerrs = 0;

  MPI_Dims_create(totpes,3,numpes);
  MPI_Cart_create(MPI_COMM_WORLD,3,numpes,isperiodic,reorder,&comm_cart);
  MPI_Comm_rank(comm_cart,&mype);
  MPI_Cart_coords(comm_cart,mype,3,pe_coords);

/*
   Determine local sizes for tt (locsiz_3d) and smf (locsiz_2d).
   Also determine whether the current processor contains valid 2D data.
   Compute file_size in 1e6 Bytes
 */

  find_locnx(totsiz_3d[0],pe_coords[0],numpes[0],&locsiz_3d[0],&kstart);
  find_locnx(totsiz_3d[1],pe_coords[1],numpes[1],&locsiz_3d[1],&jstart);
  find_locnx(totsiz_3d[2],pe_coords[2],numpes[2],&locsiz_3d[2],&istart);

  totsiz_2d[0] = totsiz_3d[1];
  totsiz_2d[1] = totsiz_3d[2];

  locsiz_2d[0] = locsiz_3d[1];
  locsiz_2d[1] = locsiz_3d[2];

  has_2d = (! only_3d) && (pe_coords[0] == numpes[0] - 1);

  if (only_3d)
    file_size = (((double) totsiz_3d[0])*((double) totsiz_3d[1])
               * ((double) totsiz_3d[2])) * 1.0e-6 * sizeof(double);
  else
    file_size = (((double) totsiz_3d[0])*((double) totsiz_3d[1])
               * ((double) totsiz_3d[2])
               + ((double) totsiz_2d[0])*((double) totsiz_2d[1]))
               * 1.0e-6 * sizeof(double);

/* Print data decomposition information */

  if (mype == 0 && verbose)
    printf("mype  pe_coords    totsiz_3d         locsiz_3d       "
           "kstart,jstart,istart\n");

  MPI_Barrier(comm_cart);

  if (verbose)
  printf("%3d   %2d %2d %2d  %4lld %4lld %4lld    %4lld %4lld %4lld   %6lld %6lld %6lld\n",
         mype, pe_coords[0], pe_coords[1], pe_coords[2],
         totsiz_3d[0], totsiz_3d[1], totsiz_3d[2],
         locsiz_3d[0], locsiz_3d[1], locsiz_3d[2],
         kstart, jstart, istart);

/* Write and then read back */

  for (i=0; i < 20; t[i++] = DBL_MAX);   /* ready for timing */

  write_file(filename, &t[ 0]);
  read_file (filename, &t[10]);

/* Compute I/O rates */

  rates_l[0] = file_size / t[1];             /* write rate */
  rates_l[1] = file_size /(t[0] + t[1]);     /* effective write rate */
  rates_l[2] = file_size / t[11];            /* read rate */
  rates_l[3] = file_size /(t[10] + t[11]);   /* effective read rate */

  MPI_Allreduce(rates_l, rates_g, 4, MPI_DOUBLE, MPI_MIN, comm_cart);
  MPI_Allreduce(t, t_g, 20, MPI_DOUBLE, MPI_MAX, comm_cart);

  if (mype == 0 && verbose) {
     printf("File size: %10.3e MB\n", file_size);
     printf("    Write: %9.3f MB/s  (eff., %9.3f MB/s)\n",
            rates_g[0], rates_g[1]);
     printf("    Read : %9.3f MB/s  (eff., %9.3f MB/s)\n",
            rates_g[2], rates_g[3]);
     printf(" %c %10.3e %3d %10.3e %10.3e %8.3f %10.3e %10.3e %8.3f\n",
           ((fillmode == NC_FILL) ? 'f' : 'n'), file_size, totpes,
            t_g[0], t_g[1], rates_g[1], t_g[10], t_g[11], rates_g[3]);
  }

  MPI_Comm_free(&comm_cart);

    MPI_Offset malloc_size, sum_size;
    int err = ncmpi_inq_malloc_size(&malloc_size);
    if (err == NC_NOERR) {
        MPI_Reduce(&malloc_size, &sum_size, 1, MPI_OFFSET, MPI_SUM, 0, MPI_COMM_WORLD);
        if (mype == 0 && sum_size > 0)
            printf("heap memory allocated by PnetCDF internally has %lld bytes yet to be freed\n",
                   sum_size);
    }

    MPI_Allreduce(MPI_IN_PLACE, &nerrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (rank == 0) {
        if (nerrs) printf(FAIL_STR,nerrs);
        else       printf(PASS_STR);
    }

  MPI_Finalize();
  return 0;
}


void write_file(char *filename, double *t) {
  double *tt = NULL;
  double *smf = NULL;
  double t1, t2, t3;
  int dim_id[3];
  int lon_id, lat_id, lev_id;
  int err;
  int file_id;
  int t_id, smf_id;
  int ii;
  MPI_Offset start_3d[3];
  MPI_Offset count_3d[3];
  MPI_Offset start_2d[2];
  MPI_Offset count_2d[2];

  start_3d[0] = kstart;
  start_3d[1] = jstart;
  start_3d[2] = istart;
  count_3d[0] = locsiz_3d[0];
  count_3d[1] = locsiz_3d[1];
  count_3d[2] = locsiz_3d[2];
  start_2d[0] = jstart;
  start_2d[1] = istart;
  count_2d[0] = locsiz_2d[0];
  count_2d[1] = locsiz_2d[1];
  
  tt = (double*)malloc(locsiz_3d[0]*locsiz_3d[1]*locsiz_3d[2]*sizeof(double));

  if (has_2d)
    smf = (double*)malloc(locsiz_2d[0]*locsiz_2d[1]*sizeof(double));
  else
    smf = (double*)malloc(sizeof(double));

  for (ii = 1; ii <= nwrites; ii++) {

    if(mype == 0) unlink(filename);

    get_fields(tt, smf);
    MPI_Barrier(comm_cart);

    t1 = MPI_Wtime();

    err = ncmpi_create(comm_cart, filename, NC_CLOBBER, MPI_INFO_NULL,
                        &file_id); ERR

/*  err = nc_set_fill(file_id,fillmode,&old_fillmode); ERR */

    err = ncmpi_def_dim(file_id,"level",    (MPI_Offset) totsiz_3d[0],&lev_id); ERR
    err = ncmpi_def_dim(file_id,"latitude", (MPI_Offset) totsiz_3d[1],&lat_id); ERR
    err = ncmpi_def_dim(file_id,"longitude",(MPI_Offset) totsiz_3d[2],&lon_id); ERR

    dim_id[0] = lev_id; dim_id[1] = lat_id; dim_id[2] = lon_id;

    err = ncmpi_def_var(file_id,"t",NC_DOUBLE,3,dim_id,&t_id); ERR

    if (! only_3d) {
      err = ncmpi_def_var(file_id,"smf",NC_DOUBLE,2,&dim_id[1],&smf_id); ERR
    }

    err = ncmpi_enddef(file_id); ERR

    t2 = MPI_Wtime();

    err = ncmpi_put_vara_double_all(file_id,t_id,start_3d,count_3d,tt); ERR

    if (! only_3d) {
      err = ncmpi_begin_indep_data(file_id); ERR

      if (has_2d) {
        err = ncmpi_put_vara_double(file_id,smf_id,start_2d,count_2d,smf); ERR
      }

      err = ncmpi_end_indep_data(file_id); ERR
    }

    err = ncmpi_close(file_id); ERR

    MPI_Barrier(comm_cart);
    t3 = MPI_Wtime();

    if (t2 - t1 < t[0]) t[0] = t2 - t1;
    if (t3 - t2 < t[1]) t[1] = t3 - t2;
    if (mype == 0 && verbose) printf("write %d: %9.3e %9.3e\n", ii, t2-t1, t3-t2);
  }

  free(tt);
  free(smf);
}


void read_file(char *filename, double *t) {
  double *tt  = NULL;
  double *smf = NULL;
  double *buf = NULL;
  double t1, t2, t3;
  double dt1, dt2=0;
  int ncid;
  int vid_t, vid_smf;
  int i, j, k, ii, err;

  MPI_Offset start_3d[3];
  MPI_Offset count_3d[3];
  MPI_Offset start_2d[2];
  MPI_Offset count_2d[2];

  start_3d[0] = kstart;
  start_3d[1] = jstart;
  start_3d[2] = istart;
  count_3d[0] = locsiz_3d[0];
  count_3d[1] = locsiz_3d[1];
  count_3d[2] = locsiz_3d[2];
  start_2d[0] = jstart;
  start_2d[1] = istart;
  count_2d[0] = locsiz_2d[0];
  count_2d[1] = locsiz_2d[1];

  tt = (double*)malloc(locsiz_3d[0]*locsiz_3d[1]*locsiz_3d[2]*sizeof(double));

  if (has_2d)
    smf = (double*)malloc(locsiz_2d[0]*locsiz_2d[1]*sizeof(double));
  else
    smf = (double*)malloc(sizeof(double));

  buf = (double*)malloc(locsiz_3d[0]*locsiz_3d[1]*locsiz_3d[2]*sizeof(double));

  get_fields(tt, smf);

  for (ii = 1; ii <= nreads; ii++) {

    double *ptr = buf;
    for (k = 0; k < locsiz_3d[0]; k++)
      for (j = 0; j < locsiz_3d[1]; j++)
        for (i = 0; i < locsiz_3d[2]; i++)
          *ptr++ = 4.444;

    MPI_Barrier(comm_cart);
    t1 = MPI_Wtime();

    err = ncmpi_open(comm_cart, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR

    err = ncmpi_inq_varid(ncid,"t",&vid_t); ERR
    if (! only_3d) {
        err = ncmpi_inq_varid(ncid,"smf",&vid_smf); ERR
    }

    t2 = MPI_Wtime();

    err = ncmpi_get_vara_double_all(ncid,vid_t,start_3d,count_3d,buf); ERR

    dt1 = MPI_Wtime();
    if (ii == 1) compare_vec(tt,buf,3,locsiz_3d,1);
    dt1 = MPI_Wtime() - dt1;

    if (! only_3d) {
      err = ncmpi_begin_indep_data(ncid); ERR

      if (has_2d) {
          err = ncmpi_get_vara_double(ncid,vid_smf,start_2d,count_2d,buf); ERR
      }

      dt2 = MPI_Wtime();
      if (ii == 1) compare_vec(smf,buf,2,locsiz_2d,has_2d);
      dt2 = MPI_Wtime() - dt2;

      err = ncmpi_end_indep_data(ncid); ERR
    }

    err = ncmpi_close(ncid); ERR

    MPI_Barrier(comm_cart);
    t3 = MPI_Wtime();

    if (t2 - t1 < t[0]) t[0] = t2 - t1;
    if ((t3 - t2) - (dt1 + dt2) < t[1]) t[1] = (t3 - t2) - (dt1 + dt2);
    if (mype == 0 && verbose)
        printf(" read %d: %9.3e %9.3e\n", ii, t2-t1, (t3-t2)-(dt1+dt2));
  }

  free(tt);
  free(smf);
  free(buf);
}


void find_locnx(MPI_Offset nx, int mype, int totpes, MPI_Offset *locnx, MPI_Offset *xbegin) {
  MPI_Offset xremain;

  *locnx = nx / totpes;
  xremain = nx - totpes*(*locnx);
  if (mype < xremain) (*locnx)++;
  *xbegin = mype*(nx/totpes) + xremain;
  if (mype < xremain) *xbegin += mype - xremain;
}


void get_fields(double *tt, double *smf) {
  int i, j, k;

  if (random_fields) {
    unsigned int seed = (INT_MAX / totpes) * mype;
    srand(seed);

    for (k = 0; k < locsiz_3d[0]; k++)
      for (j = 0; j < locsiz_3d[1]; j++)
        for (i = 0; i < locsiz_3d[2]; i++) {
            double tmp = rand();
            *tt++ = tmp / (RAND_MAX + 1.);
        }

    if (has_2d)
      for (j = 0; j < locsiz_2d[0]; j++)
        for (i = 0; i < locsiz_2d[1]; i++) {
            double tmp = rand();
            *smf++ = tmp / (RAND_MAX + 1.);
        }
  }
  else {
    for (k = 0; k < locsiz_3d[0]; k++)
      for (j = 0; j < locsiz_3d[1]; j++)
        for (i = 0; i < locsiz_3d[2]; i++)
           *tt++ = (istart + i + 1 + totsiz_3d[2]*(jstart + j
                                   + totsiz_3d[1]*(kstart + k)))*1.e-3;

    if (has_2d)
      for (j = 0; j < locsiz_2d[0]; j++)
        for (i = 0; i < locsiz_2d[1]; i++)
           *smf++ = (istart + i + 1 + totsiz_2d[1]*(jstart + j))*1.e-2;
  }
}


void compare_vec(double *a, double *b, int ndims, MPI_Offset *sizes, int corr_data) {
  double diff, delta, delmax, delmin;
  double ws[5], wr[5];
  MPI_Offset totsiz;
  int i;

  if (corr_data) {
    totsiz = 1;
    for (i = 0; i < ndims; i++)
      totsiz = totsiz * sizes[i];

    ws[0] = 0.;           /*  diff    */
    ws[1] = 0.;           /*  sumsq   */
    ws[2] = totsiz;       /*  totsiz  */
    ws[3] = 0.;           /*  delmax  */
    ws[4] = DBL_MAX;      /*  delmin  */

    for (i = 0; i < totsiz; i++) {
      delta = (a[i] - b[i]) * (a[i] - b[i]);
      ws[0] = ws[0] + delta;
      ws[1] = ws[1] + a[i] * a[i];
      if (delta > ws[3]) ws[3] = delta;
      if (delta < ws[4]) ws[4] = delta;
    }
  }
  else {
    ws[0] = ws[1] = ws[2] = ws[3] = 0.;
    ws[4] = DBL_MAX;
  }

  MPI_Allreduce( ws,     wr,     3, MPI_DOUBLE, MPI_SUM, comm_cart);
  MPI_Allreduce(&ws[3], &delmax, 1, MPI_DOUBLE, MPI_MAX, comm_cart);
  MPI_Allreduce(&ws[4], &delmin, 1, MPI_DOUBLE, MPI_MIN, comm_cart);

  diff   = sqrt(wr[0]/wr[1]);           /*  Normalized error */
  delmax = sqrt(wr[2]*delmax/wr[1]);    /*  Normalized max difference */
  delmin = sqrt(wr[2]*delmin/wr[1]);    /*  Normalized min difference */

  if (mype == 0 && verbose)
      printf("diff, delmax, delmin = %9.3e %9.3e %9.3e\n", diff, delmax, delmin);
}
