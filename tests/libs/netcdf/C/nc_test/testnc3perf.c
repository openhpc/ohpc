/*********************************************************************
 *   Copyright 1989, University Corporation for Atmospheric Research
 *   See netcdf/README file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/nctime.c,v 1.12 1996/04/30 17:56:58 davis Exp $
 *********************************************************************/

/*
 * This is a standalone benchmark program for timing netCDF hyperslab accesses.
 * Once it is built, the benchmarks are run by invoking it with the shape of a
 * four-dimensional netCDF variable, e.g.
 *
 * 	nctime 10 20 30 40
 *
 * which will run timing benchmarks accessing 1-, 2-, 3-, and 4-dimensional
 * slabs from 10 by 20 by 30 by 40 variables of each type.  The first dimension
 * varies most slowly and is an unlimited (record) dimension.
 *
 * This program is especially useful for testing the effect of various compiler
 * optimization levels or platform-specific optimizations on the performance of
 * netCDF I/O.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/param.h>		/* for HZ */
#include <sys/times.h>
#include <assert.h>
#include <time.h>

#ifndef HZ
#ifdef CLK_TCK
#define HZ CLK_TCK
#else
#define HZ 60
#endif
#endif

#include "netcdf.h"

struct ncdim {			/* dimension */
    char *name;
    long size;
};

struct ncvar {			/* variable */
    char *name;
    nc_type type;
    int ndims;
    int *dims;
    int natts;
};


#define LEN_OF(array) ((sizeof array) / (sizeof array[0]))

/* Number of dimensions.  Changing this requires other changes as well. */
#define NDIMS   4

#define NVARS   6		/* number of variables, one for each type */

/* Any function that maps dimension values 1-1 to values is OK here */
#define VF(w)  1000*w[0]+100*w[1]+10*w[2]+w[3]

static int DEFAULTDIMS[NDIMS] = {10, 20, 30, 40};

/*
 * Fill typed array element with specified value, that is
 * 	
 * 	v[ii] = val;
 */
static void
val_stuff(type, v, ii, val)	/* v[ii] = val */
     nc_type type;		/* netcdf type of v, NC_BYTE, ..., NC_DOUBLE */
     void *v;			/* array of specified type */
     int ii;			/* it's v[ii] we want to store into */
     long val;			/* value to store */
{
    union gp {
	char cp[1];
	short sp[1];
	nclong lp[1];
	float fp[1];
	double dp[1];
    } *gp;

    gp = (union gp *) v;
    switch (type) {
      case NC_BYTE:
      case NC_CHAR:
	gp->cp[ii] = (char) val;
	break;
      case NC_SHORT:
	gp->sp[ii] = (short) val;
	break;
      case NC_LONG:
	gp->lp[ii] = (nclong) val;
	break;
      case NC_FLOAT:
	gp->fp[ii] = (float) val;
	break;
      case NC_DOUBLE:
	gp->dp[ii] = (double) val;
	break;
    }
}


/*
 * Compare typed array element with specified value, that is return
 *
 * 	(v[ii] != val)
 *
 * returns 0 if equal, 1 if not equal 
 */

static int
val_diff(type, v, ii, val)	/* v[ii] != val */
     nc_type type;		/* netcdf type of v, NC_BYTE, ..., NC_DOUBLE */
     void *v;			/* array of specified type */
     int ii;			/* it's v[ii] we want to compare */
     long val;			/* value to compare with */
{
    union gp {
	char cp[1];
	short sp[1];
	nclong lp[1];
	float fp[1];
	double dp[1];
    } *gp;

    gp = (union gp *) v;
    switch (type) {
      case NC_BYTE:
      case NC_CHAR:
	return (gp->cp[ii] != (char) val);
      case NC_SHORT:
	return (gp->sp[ii] != (short) val);
      case NC_LONG:
	return (gp->lp[ii] != (nclong) val);
      case NC_FLOAT:
	return (gp->fp[ii] != (float) val);
      case NC_DOUBLE:
	return (gp->dp[ii] != (double) val);
    }
    /* NOTREACHED */
    return 0;
}

/*
 * The following timing macros can be used by including the necessary
 * declarations with
 *
 *     TIMING_DECLS ;
 *
 * and surrounding sections of code to be timed with the "statements"
 *
 *     TIMING_START ;
 *     [code to be timed goes here]
 *     TIMING_END ;
 *
 * (The terminating semicolon is required for TIMING_DECLS and TIMING_END.)
 * The macros assume the user has stored a description of what is being timed
 * in the user-declared string time_mess, and has included <sys/times.h>
 */

#define TIMING_DECLS \
	long TMreps;		/* counts repetitions of timed code */ \
	long TMrepeats;		/* repetitions needed to exceed a second */ \
	clock_t TMus, TMsy;	/* user and system time in clock ticks */ \
	float TMelapsed;	/* elapsed time in seconds */ \
	struct tms TMru;
      
#define TIMING_START \
	TMrepeats = 1; \
	do {  /* loop enough times for at least 0.1 second elapsed time */ \
	    TMrepeats *= 2; \
	    times(&TMru); \
	    TMus = TMru.tms_utime; \
	    TMsy = TMru.tms_stime; \
	    for(TMreps=0;TMreps < TMrepeats;TMreps++) {
	
#define TIMING_END \
            } \
	    times(&TMru); \
	    TMus = TMru.tms_utime - TMus; \
	    TMsy = TMru.tms_stime - TMsy; \
	    TMelapsed= (float) (TMus+TMsy) / (float) HZ; \
	    if (TMreps < TMrepeats) break; \
	} while (TMelapsed < 0.1 ); \
	printf("time for %-20.20s %10.3f msec\n", \
	       time_mess, TMelapsed*1000./(TMreps+1))



/*
 * For each type of variable, put a four-dimensional hypercube of values
 * with a single call to ncvarput.  Then use ncvarget to retrieve a single
 * value, a vector of values along each of the four dimensions, a plane of
 * values along each of the six pairs of dimensions, a cube of values along
 * each of the four triples of dimensions, and all the values.
 */
void
test_slabs(ncid, sizes)
     int ncid;			/* handle of netcdf open and in data mode */
     int *sizes;		/* dimension sizes */
{
    char time_mess[100];

    struct ncdim dims[NDIMS];
    int dimids[NDIMS];		/* dimension ids */
    long corner[NDIMS], edge[NDIMS], point[NDIMS];

    static struct ncvar va[NVARS] = { /* variables of all types */
	{"byte_var", NC_BYTE, NDIMS, 0, 0},
	{"char_var", NC_CHAR, NDIMS, 0, 0},
	{"short_var", NC_SHORT, NDIMS, 0, 0},
	{"long_var", NC_LONG, NDIMS, 0, 0},
	{"float_var", NC_FLOAT, NDIMS, 0, 0},
	{"double_var", NC_DOUBLE, NDIMS, 0, 0},
    };
    void *v;

    int varid[NVARS], iv;			/* variable id */
    int idim, jdim, kdim, ldim;
    int iw, ix, iy, iz, ii, jj, kk;
    static char* dnames[] = {"w", "x", "y", "z", "u", "v", "a", "b", "c", "d"};

    assert(NDIMS <= LEN_OF(dnames));
    for (idim = 0; idim < NDIMS; idim++) {
	dims[idim].size = sizes[idim];
	dims[idim].name = dnames[idim];
    }
    
    /* back in define mode OK, now add dimensions */

    dimids[0] = ncdimdef(ncid, dims[0].name, NC_UNLIMITED);
    if (dimids[0] == -1) {
	ncclose(ncid);
	return;
    }
    for (idim = 1; idim < NDIMS; idim++) {
	dimids[idim] = ncdimdef(ncid, dims[idim].name, dims[idim].size);
	if (dimids[idim] == -1) {
	    ncclose(ncid);
	    return;
	}
    }

    /* define a multi-dimensional variable of each type */

    for (iv = 0; iv < NVARS; iv++) {
	va[iv].dims = (int *) malloc(sizeof(int) * (unsigned)va[iv].ndims);
	for (idim = 0; idim < va[iv].ndims; idim++)
	  va[iv].dims[idim] = dimids[idim];
	varid[iv] = ncvardef(ncid, va[iv].name, va[iv].type, va[iv].ndims,
			     va[iv].dims);
	if (varid[iv] == -1) {
	    ncclose(ncid); return;
	}
    }

    if (ncendef (ncid) == -1) {
	ncclose(ncid); return;
    }
    
    printf("Note: first ncvarput writes fill values for all variables.\n");

    for (iv = 0; iv < NVARS; iv++) { /* test each type of variable */
	TIMING_DECLS ;
	printf("\n----- %s(%d,%d,%d,%d)\n",
	       va[iv].name, sizes[0], sizes[1], sizes[2], sizes[3]);

	v = (void *) malloc((unsigned)sizes[0]*sizes[1]*sizes[2]*sizes[3]
		    * nctypelen(va[iv].type));

	/* fill it with values using a function of dimension indices */
	ii = 0;
	for (iw=0; iw < sizes[0]; iw++) {
	    corner[0] = iw;
	    for (ix=0; ix < sizes[1]; ix++) {
		corner[1] = ix;
		for (iy=0; iy < sizes[2]; iy++) {
		    corner[2] = iy;
		    for (iz=0; iz < sizes[3]; iz++) {
			corner[3] = iz;
			/* v[ii++] = VF(corner); */
			val_stuff(va[iv].type, v, ii, VF(corner));
			ii++;
		    }
		}
	    }
	}
	
	for (idim = 0; idim < NDIMS; idim++) {
	    corner[idim] = 0;
	    edge[idim] = dims[idim].size;
	}

	sprintf(time_mess,"ncvarput %ldx%ldx%ldx%ld",
		edge[0], edge[1], edge[2], edge[3]);

	TIMING_START ;
	/* ncvarput the whole variable */
	if (ncvarput(ncid, varid[iv], corner, edge, (void *) v) == -1) {
	    ncclose(ncid);
	    return;
	}
	TIMING_END ;

	/*
	 * For several combinations of fixed dimensions, get a slab and compare
	 * values to function values.
	 */

	/* get an interior point */
	for (idim=0; idim < NDIMS; idim++) {
	    corner[idim] = dims[idim].size/2;
	    edge[idim] = 1;
	    point[idim] = corner[idim];
	}
	
	sprintf(time_mess,"ncvarget %ldx%ldx%ldx%ld"
		,edge[0],edge[1],edge[2],edge[3]);
      
	TIMING_START ;
	if (ncvarget(ncid, varid[iv], corner, edge, (void *) v) == -1)
	    return;
	TIMING_END ;
	
	/* if (v[0] != VF(point)) */
	if (val_diff(va[iv].type, v, 0, VF(point)))
	  fprintf(stderr,"ncvarget got wrong value for point");
	
	/* get a vector in each direction */
	for (idim=0; idim < NDIMS; idim++) {
	    for (jdim=0; jdim < NDIMS; jdim++) {
		corner[jdim] = 0;
		edge[jdim] = 1;
		point[jdim] = corner[jdim];
	    }
	    corner[idim] = 0;		/* get vector along dimension idim */
	    edge[idim] = dims[idim].size;

	    sprintf(time_mess,"ncvarget %ldx%ldx%ldx%ld"
		    ,edge[0],edge[1],edge[2],edge[3]);

	    TIMING_START ;
	    if (ncvarget(ncid, varid[iv], corner, edge, (void *) v) == -1)
	      return;
	    TIMING_END ;

	    for (ii=corner[idim]; ii < edge[idim]; ii++) {
		point[idim] = ii;
		/* if (v[ii] != VF(point)) */
		if (val_diff(va[iv].type, v, ii, VF(point)))
		  fprintf(stderr,"ncvarget got wrong value for vector");
	    }
	}

	/* get a plane in each direction */
	for (idim=0; idim < NDIMS; idim++) {
	    for (jdim=idim+1; jdim < NDIMS; jdim++) {
		for (kdim=0; kdim < NDIMS; kdim++) { /* reset corners and edges */
		    corner[kdim] = 0;
		    edge[kdim] = 1;
		    point[kdim] = corner[kdim];
		}
		corner[idim] = 0;	/* plane along dimensions idim jdim */
		corner[jdim] = 0;
		edge[idim] = dims[idim].size;
		edge[jdim] = dims[jdim].size;
		
		sprintf(time_mess,"ncvarget %ldx%ldx%ldx%ld"
			,edge[0],edge[1],edge[2],edge[3]);

		TIMING_START ;
		if (ncvarget(ncid, varid[iv], corner, edge, (void *) v) == -1)
		  return;
		TIMING_END ;

		for (ii=corner[idim]; ii < edge[idim]; ii++) {
		    for (jj=corner[jdim]; jj < edge[jdim]; jj++) {
			point[idim] = ii;
			point[jdim] = jj;
			/* if (v[(ii)*edge[jdim]+jj] != VF(point)) { */
			if (val_diff(va[iv].type, v,
				     (ii)*(int)edge[jdim]+jj, VF(point))) {
			    fprintf(stderr,
				    "ncvarget got wrong value in plane");
			}
		    }
		}
	    }
	}
	
	/* get a cube in each direction */
	for (idim=0; idim < NDIMS; idim++) {
	    for (jdim=idim+1; jdim < NDIMS; jdim++) {
		for (kdim=jdim+1; kdim < NDIMS; kdim++) {
		    for (ldim=0; ldim < NDIMS; ldim++) { /* reset corners, edges */
			corner[ldim] = 0;
			edge[ldim] = 1;
			point[ldim] = corner[ldim];
		    }
		    corner[idim] = 0;	/* intr. cube along idim jdim kdim */
		    corner[jdim] = 0;
		    corner[kdim] = 0;
		    edge[idim] = dims[idim].size;
		    edge[jdim] = dims[jdim].size;
		    edge[kdim] = dims[kdim].size;
		
		    sprintf(time_mess,"ncvarget %ldx%ldx%ldx%ld"
			    ,edge[0],edge[1],edge[2],edge[3]);

		    TIMING_START ;
		    if (ncvarget(ncid, varid[iv], corner, edge,
				 (void *) v) == -1)
		      return;
		    TIMING_END ;

		    for (ii=corner[idim]; ii < edge[idim]; ii++) {
			for (jj=corner[jdim]; jj < edge[jdim]; jj++) {
			    for (kk=corner[kdim]; kk < edge[kdim]; kk++) {
				point[idim] = ii;
				point[jdim] = jj;
				point[kdim] = kk;
				/* if (v[((ii)*edge[jdim]+jj)*
				   edge[kdim]+kk] != VF(point)) { */
				if (val_diff(va[iv].type,v,
					     ((ii)*(int)edge[jdim]+jj)*
					     (int)edge[kdim]+kk,VF(point))) {
				    fprintf(stderr,
					    "ncvarget - bad value in cube");
				}
			    }
			}
		    }
		}
	    }
	}
	
	/* get one 4-D slab of data */
	for(idim = 0; idim < NDIMS; idim++) {
	    corner[idim] = 0;
	    edge[idim] = dims[idim].size;
	}
		
	sprintf(time_mess,"ncvarget %ldx%ldx%ldx%ld"
		,edge[0],edge[1],edge[2],edge[3]);

	TIMING_START ;
	if (ncvarget(ncid, varid[iv], corner, edge, (void *) v) == -1)
	  return;
	TIMING_END ;

	free(v);
    }
}

void
usage(argv)
     char **argv;
{
    int i;
    fprintf(stderr, "usage: %s ", argv[0]);
    for (i=0; i < NDIMS; i++)
      fprintf(stderr, "dim%d ", i);
    fprintf(stderr, "\n");
}


int
main(argc, argv)
     int argc;
     char **argv;
{
    int ncid;
    int i;
    int w[NDIMS];

    if (argc != NDIMS+1) {
        for (i = 0; i < NDIMS; i++)
          w[i] = DEFAULTDIMS[i];
    } else {
        for (i = 0; i < NDIMS; i++)
          w[i] = atoi(argv[i+1]);
    }

    ncid = nccreate("benchmark.nc",NC_CLOBBER);

    test_slabs(ncid, w);

    ncclose(ncid);
    return 0;
}
