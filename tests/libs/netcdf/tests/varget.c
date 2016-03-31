/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/varget.c,v 1.14 2006/10/31 16:22:03 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "error.h"
#include "tests.h"
#include "emalloc.h"


/*
 * Test ncvarget
 *    check that proper call worked after ncvarput
 *    try with negative coords, edges, check error
 *    try with too-large coords, edges, check error
 *    try with bad variable handle, check error
 *    try in define mode, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncvarget(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarget";
    int cdfid;			/* netcdf id */
    int iv;			/* variable id */
    struct cdfhc {		/* a hypercube with generic values */
	long cor[MAX_NC_DIMS];	/* netcdf coordinates for lower corner */
	long edg[MAX_NC_DIMS];	/* netcdf edge lengths to upper corner */
	void *vals;		/* pointer to block of values */
    } hc;			/* test hypercube */

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened in data mode, try putting and getting hypercubes of each type */
    nerrs += test_varputget (cdfid);

    /* try putting hypercube and getting various interior slabs */
    nerrs += test_slabs (cdfid);
    
    /* find a variable with at least one dimension */
    iv = 0;
    while (test.vars[iv].ndims <= 0 && iv < test.nvars)
      iv++;
    if (iv < test.nvars) {	/* iv is varid of variable with dimensions */
	long tmp;
	/* set coords */
	int id;			/* dimension id */
	for (id = 0; id < test.vars[iv].ndims; id++) {
	    hc.cor[id] = 0;
	    hc.edg[id] = 1;
	}
	/* get space for vals */
	hc.vals = emalloc(nctypelen(test.vars[iv].type) + 8);

	id = test.vars[iv].ndims - 1;
	tmp = hc.cor[id];
	hc.cor[id] = -1;	/* try negative coordinate, should fail */
	if(ncvarget (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarget should fail for negative corner", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.cor[id] = tmp;
	tmp = hc.edg[id];
	hc.edg[id] = -1;	/* try negative edge, should fail */
	if(ncvarget (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarget should fail for negative edge", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.edg[id] = tmp;

	{ 
		long mqv = test.vars[iv].ndims -1 ;
		int dim = test.vars[iv].dims[mqv] ;

	tmp = hc.cor[mqv];
	hc.cor[mqv] = test.dims[dim].size; /* try big coordinate, should fail */
	if(ncvarget (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarget should fail for too-high coordinate", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.cor[mqv] = tmp;
	tmp = hc.edg[mqv];
	hc.edg[mqv] = test.dims[dim].size + 1; /* try big edge, should fail */
	if(ncvarget (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarget should fail for too-high edge", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.edg[mqv] = tmp;
	} /* mqv block */

	if (ncredef(cdfid) == -1) {
	    error("%s: ncredef failed", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	/* try in define mode, should fail */
	if(ncvarget (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarget should fail in define mode",
		  pname);
	    ncclose(cdfid); return ++nerrs;
	}
	if (ncendef (cdfid) == -1) {
	    error("%s: ncendef failed", pname);
	    ncclose(cdfid); return ++nerrs;
	}
    }
    /* try with bad variable handle, should fail */
    if(ncvarget (cdfid, -1, hc.cor, hc.edg, hc.vals) != -1) {
	error("%s: ncvarget should fail for bad variable handle", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* try with bad netCDF handle, should fail */
    if(ncvarget (cdfid, 0, hc.cor, hc.edg, hc.vals) != -1) {
	error("%s: ncvarget failed to report bad netcdf handle", pname);
	nerrs++;
    }
    free (hc.vals);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}
