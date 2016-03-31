/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/varput.c,v 1.14 2006/10/31 16:22:05 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "emalloc.h"
#include "val.h"
#include "error.h"
#include "tests.h"


/*
 * Test ncvarput
 *    check that proper call worked with ncvarget
 *    try with negative coords, edges, check error
 *    try with too-large coords, edges, check error
 *    try with bad variable handle, check error
 *    try in define mode, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncvarput(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarput";
    int cdfid;			/* netcdf id */
    int iv;			/* variable id */
    struct cdfhc {		/* a hypercube with generic values */
	long cor[MAX_NC_DIMS];	/* netcdf coordinates for lower corner */
	long edg[MAX_NC_DIMS];	/* netcdf edge lengths to upper corner */
	void *vals;		/* pointer to block of values */
    } hc;			/* test hypercube */
    long tmp;
    int id;			/* dimension id */

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    /* handle case where struct netcdf test is uninitialised */
    hc.cor[0] = 0 ;
    hc.edg[0] = 1 ;
    hc.vals = 0 ;

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }

    /* opened in data mode, try putting and getting hypercubes of each type */
    nerrs += test_varputget (cdfid);	/* prints messages for discrepencies */
    
    /* find a variable with at least one dimension */
    iv = 0;
    while (test.vars[iv].ndims <= 0 && iv < test.nvars)
      iv++;
    if (iv < test.nvars) {	/* iv is first varid of var with dimensions */
	/* set coords */
	for (id = 0; id < test.vars[iv].ndims; id++) {
	    hc.cor[id] = 0;
	    hc.edg[id] = 1;
	}
	/* fill in vals with value of appropriate type */
	hc.vals = emalloc(nctypelen(test.vars[iv].type));
	val_fill(test.vars[iv].type, 1, hc.vals);

	id = test.vars[iv].ndims - 1;
	tmp = hc.cor[id];
	hc.cor[id] = -1;	/* try negative coordinate, should fail */
	if(ncvarput (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarput should fail for negative corner", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.cor[id] = tmp;
	tmp = hc.edg[id];
	hc.edg[id] = -1;	/* try negative edge, should fail */
	if(ncvarput (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarput should fail for negative edge", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.edg[id] = tmp;
	{ 
		long mqv = test.vars[iv].ndims -1 ;
		int dim = test.vars[iv].dims[mqv] ;

		tmp = hc.cor[mqv];
		hc.cor[mqv] = test.dims[dim].size; /* try big coordinate, should fail */
		if(ncvarput (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
		    error("%s: ncvarput should fail for too-high coordinate", pname);
		    ncclose(cdfid); return ++nerrs;
		}
		hc.cor[mqv] = tmp;
	
		tmp = hc.edg[mqv];
		hc.edg[mqv] = test.dims[dim].size + 1; /* try big edge, should fail */
		if(ncvarput (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
		    error("%s: ncvarput should fail for too-high edge", pname);
		    ncclose(cdfid); return ++nerrs;
		}
		hc.edg[mqv] = tmp;
	}

	if (ncredef(cdfid) == -1) {
	    error("%s: ncredef failed", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	/* try in define mode, should fail */
	if(ncvarput (cdfid, iv, hc.cor, hc.edg, hc.vals) != -1) {
	    error("%s: ncvarput should fail in define mode", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	if (ncendef (cdfid) == -1) {
	    error("%s: ncendef failed", pname);
	    ncclose(cdfid); return ++nerrs;
	}
    }
    else
	error("\"struct netcdf test\" uninitialized, no tests performed") ;
    /* try with bad variable handle, should fail */
    if(ncvarput (cdfid, -1, hc.cor, hc.edg, hc.vals) != -1) {
	error("%s: ncvarput should fail for bad variable handle", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* try with bad netCDF handle, should fail */
    if(ncvarput (cdfid, 0, hc.cor, hc.edg, hc.vals) != -1) {
	error("%s: ncvarput failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if(hc.vals) free (hc.vals);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}
