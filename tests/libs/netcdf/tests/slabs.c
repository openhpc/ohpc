/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/slabs.c,v 1.13 2009/02/11 16:17:39 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "emalloc.h"
#include "add.h"		/* functions to update in-memory netcdf */
#include "error.h"
#include "tests.h"

#define LEN_OF(array) ((sizeof array) / (sizeof array[0]))
/* dimension sizes */
#define NDIMS   4		/* number of dimensions */
#define WSIZE	7		/* sizes of dimensions */
#define XSIZE	5
#define YSIZE   6
#define ZSIZE   4
/* Any function that maps dimension values 1-1 to values is OK here */
#define VF(w)  1000*w[0]+100*w[1]+10*w[2]+w[3]
#define NVARS   6		/* number of variables */


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
    static char pname[] = "val_stuff";

    switch (type) {
      case NC_BYTE:
      case NC_CHAR:
	 ((char *)v)[ii] = (char) val;
	break;
      case NC_SHORT:
	 ((short *)v)[ii] = (short) val;
	break;
      case NC_LONG:
	 ((nclong *)v)[ii] = (nclong) val;
	break;
      case NC_FLOAT:
	 ((float *)v)[ii] = (float) val;
	break;
      case NC_DOUBLE:
	 ((double *)v)[ii] = (double) val;
	break;
      default:
	error("%s: bad type, test program error", pname);
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
    static char pname[] = "val_diff";

    switch (type) {
      case NC_BYTE:
      case NC_CHAR:
	 return (((char *)v)[ii] != (char) val);
      case NC_SHORT:
	 return (((short *)v)[ii] != (short) val);
      case NC_LONG:
	 return (((nclong *)v)[ii] != (nclong) val);
      case NC_FLOAT:
	 return (((float *)v)[ii] != (float) val);
      case NC_DOUBLE:
	 return (((double *)v)[ii] != (double) val);
      default:
	error("%s: bad type, test program error", pname);
	return (-1);
    }
}


/*
 * For each type of variable, put a four-dimensional hypercube of values
 * with a single call to ncvarput.  Then use ncvarget to retrieve a single
 * interior value, an interior vector of values along each of the four
 * dimensions, an interior plane of values along each of the six pairs of
 * dimensions, and an interior cube of values along each of the four
 * triples of dimensions.  In each case, compare the retrieved values with
 * the written values.
 */

int
test_slabs(cdfid)
     int cdfid;			/* handle of netcdf open and in data mode */
{
    int nerrs = 0;
    static char pname[] = "test_slabs";
    static struct cdfdim dims[NDIMS] = {
	{"w", WSIZE},
	{"x", XSIZE},
	{"y", YSIZE},
	{"z", ZSIZE}
    };
    int dimids[NDIMS];		/* dimension ids */
    long corner[NDIMS], edge[NDIMS], point[NDIMS];

    static struct cdfvar va[NVARS] = { /* variables of all types */
	{"bytevar", NC_BYTE, NDIMS, ___, 0},
	{"charvar", NC_CHAR, NDIMS, ___, 0},
	{"shortvar", NC_SHORT, NDIMS, ___, 0},
	{"longvar", NC_LONG, NDIMS, ___, 0},
	{"floatvar", NC_FLOAT, NDIMS, ___, 0},
	{"doublevar", NC_DOUBLE, NDIMS, ___, 0},
    };
    void *v;

    int varid[NVARS], iv;			/* variable id */
    int idim, jdim, kdim, ldim;
    int iw, ix, iy, iz, ii, jj, kk;

    if (ncredef(cdfid) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(cdfid); return 1;
    }

    /* back in define mode OK, now add dimensions */

    for (idim = 0; idim < NDIMS; idim++) {
	dimids[idim] = ncdimdef(cdfid, dims[idim].name, dims[idim].size);
	if (dimids[idim] == -1) {
	    error("%s: ncdimdef failed", pname);
	    ncclose(cdfid);
	    return 1;
	}
	add_dim(&test, &dims[idim]);
    }

    /* define a multi-dimensional variable of each type */

    for (iv = 0; iv < NVARS; iv++) {
	va[iv].dims = (int *) emalloc(sizeof(int) * va[iv].ndims);
	for (idim = 0; idim < va[iv].ndims; idim++)
	  va[iv].dims[idim] = dimids[idim];
	varid[iv] = ncvardef(cdfid, va[iv].name, va[iv].type, va[iv].ndims,
			     va[iv].dims);
	if (varid[iv] == -1) {
	    error("%s: ncvardef failed", pname);
	    ncclose(cdfid); return 1;
	}
	add_var(&test, &va[iv]); /* keep in-memory netcdf in sync */
	free(va[iv].dims);
    }

    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return 1;
    }

    for (iv = 0; iv < NVARS; iv++) { /* test each type of variable */

	v = emalloc(WSIZE*XSIZE*YSIZE*ZSIZE * nctypelen(va[iv].type));

	/* fill it with values using a function of dimension indices */
	ii = 0;
	for (iw=0; iw < WSIZE; iw++) {
	    corner[0] = iw;
	    for (ix=0; ix < XSIZE; ix++) {
		corner[1] = ix;
		for (iy=0; iy < YSIZE; iy++) {
		    corner[2] = iy;
		    for (iz=0; iz < ZSIZE; iz++) {
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
      
	/* ncvarput the whole variable */
	if (ncvarput(cdfid, varid[iv], corner, edge, (void *) v) == -1) {
	    error("%s: ncvarput failed", pname);
	    nerrs++;
	}

	add_data(&test, varid[iv], corner, edge); /* keep test in sync */
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
	if (ncvarget(cdfid, varid[iv], corner, edge, (void *) v) == -1) {
	    error("%s: ncvarget of one point failed", pname);
	    nerrs++;
	}
	/* if (v[0] != VF(point)) */
	if (val_diff(va[iv].type, v, 0, VF(point))) {
	    error("%s: ncvarget got wrong value for point", pname);
	    nerrs++;
	}
	
	/* get an interior vector in each direction */
	for (idim=0; idim < NDIMS; idim++) {
	    for (jdim=0; jdim < NDIMS; jdim++) {
		corner[jdim] = dims[jdim].size/2;
		edge[jdim] = 1;
		point[jdim] = corner[jdim];
	    }
	    corner[idim] = 1;		/* get vector along dimension idim */
	    edge[idim] = dims[idim].size - 2;
	    if (ncvarget(cdfid, varid[iv], corner, edge, (void *) v) == -1) {
		error("%s: ncvarget of vector failed", pname);
		nerrs++;
	    }
	    for (ii=(int)corner[idim]; ii <= edge[idim]; ii++) {
		point[idim] = ii;
		/* if (v[ii-1] != VF(point)) */
		if (val_diff(va[iv].type, v, ii-1, VF(point))) {
		    error("%s: ncvarget got wrong value for vector", pname);
		    nerrs++;
		}
	    }
	}

	/* get an interior plane in each direction */
	for (idim=0; idim < NDIMS; idim++) {
	    for (jdim=idim+1; jdim < NDIMS; jdim++) {
		for (kdim=0; kdim < NDIMS; kdim++) { /* reset corners and edges */
		    corner[kdim] = dims[kdim].size/2;
		    edge[kdim] = 1;
		    point[kdim] = corner[kdim];
		}
		corner[idim] = 1;	/* interior plane along dimensions idim jdim */
		corner[jdim] = 1;
		edge[idim] = dims[idim].size - 2;
		edge[jdim] = dims[jdim].size - 2;
		if (ncvarget(cdfid, varid[iv], corner, edge, (void *) v) == -1) {
		    error("%s: ncvarget of plane failed", pname);
		    nerrs++;
		}
		for (ii=(int)corner[idim]; ii <= edge[idim]; ii++) {
		    for (jj=(int)corner[jdim]; jj <= edge[jdim]; jj++) {
			point[idim] = ii;
			point[jdim] = jj;
			/* if (v[(ii-1)*edge[jdim]+jj-1] != VF(point)) { */
			if (val_diff(va[iv].type, v,
				     (ii-1)*(int)edge[jdim]+jj-1, VF(point))) {
			    error("%s: ncvarget got wrong value in plane", pname);
			    error("idim=%d,jdim=%d,ii=%d,jj=%d",
				  idim,
				  jdim,
				  ii,
				  jj);
			    nerrs++;
			}
		    }
		}
	    }
	}
	
	/* get an interior cube in each direction */
	for (idim=0; idim < NDIMS; idim++) {
	    for (jdim=idim+1; jdim < NDIMS; jdim++) {
		for (kdim=jdim+1; kdim < NDIMS; kdim++) {
		    for (ldim=0; ldim < NDIMS; ldim++) { /* reset corners, edges */
			corner[ldim] = dims[ldim].size/2;
			edge[ldim] = 1;
			point[ldim] = corner[ldim];
		    }
		    corner[idim] = 1;	/* intr. cube along idim jdim kdim */
		    corner[jdim] = 1;
		    corner[kdim] = 1;
		    edge[idim] = dims[idim].size - 2;
		    edge[jdim] = dims[jdim].size - 2;
		    edge[kdim] = dims[kdim].size - 2;
		    if (ncvarget(cdfid, varid[iv], corner, edge, (void *) v) == -1) {
			error("%s: ncvarget of cube failed", pname);
			nerrs++;
		    }
		    for (ii=(int)corner[idim]; ii <= edge[idim]; ii++) {
			for (jj=(int)corner[jdim]; jj <= edge[jdim]; jj++) {
			    for (kk=(int)corner[kdim]; kk <= edge[kdim]; kk++) {
				point[idim] = ii;
				point[jdim] = jj;
				point[kdim] = kk;
				/* if (v[((ii-1)*edge[jdim]+jj-1)*
				   edge[kdim]+kk-1] != VF(point)) { */
				if (val_diff(va[iv].type,v,
					     ((ii-1)*(int)edge[jdim]+jj-1)*
					     (int)edge[kdim]+kk-1,VF(point))) {
				    error("%s: ncvarget got wrong value in cube", pname);
				    error("idim=%d,jdim=%d,kdim=%d,ii=%d,jj=%d,kk=%d",
					  idim,
					  jdim,
					  kdim,
					  ii,
					  jj,
					  kk);
				    nerrs++;
				}
			    }
			}
		    }
		}
	    }
	}
	free(v);
    }
    return nerrs;
}

