/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: varputg.c,v 1.10 2006/10/31 16:22:06 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "val.h"
#include "error.h"
#include "tests.h"
#include "emalloc.h"

/*
 * Test ncvarputg
 *    check that proper call worked with ncvargetg
 *    try with negative coords, edges, check error
 *    try with too-large coords, edges, check error
 *    try with non-positive strides, check error
 *    try with bad variable handle, check error
 *    try in define mode, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncvarputg(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarputg";
    int cdfid;			/* netcdf id */
    int iv;			/* variable id */
    struct cdfhc {		/* a hypercube with generic values */
	long cor[MAX_NC_DIMS];	/* netcdf coordinates for lower corner */
	long edg[MAX_NC_DIMS];	/* netcdf edge lengths to upper corner */
	void *vals;		/* pointer to block of values */
    } hc;			/* test hypercube */
    long tmp;
    int id;			/* dimension id */
    long strides[MAX_NC_DIMS];	/* external, I/O strides */

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }

    /* opened in data mode, try putting and getting hypercubes of each type.
     * prints out messages for discrepencies */
    nerrs += test_varputgetg (cdfid);

    /* find a variable with at least one dimension */
    iv = 0;
    while (test.vars[iv].ndims <= 0 && iv < test.nvars)
      iv++;
    if (iv < test.nvars) {	/* iv is first varid of var with dimensions */

	/* set coords and strides */
	for (id = 0; id < test.vars[iv].ndims; id++) {
	    hc.cor[id] = 0;
	    hc.edg[id] = 1;
	    strides[id] = 1;
	}

	/* fill in vals with value of appropriate type */
	hc.vals = emalloc(nctypelen(test.vars[iv].type));
	val_fill(test.vars[iv].type, 1, hc.vals);

#	define TEST_FAILS(varid) \
	    (ncvarputg(cdfid, varid, hc.cor, hc.edg, \
		       strides, (long*)NULL, hc.vals) != -1)

	id = test.vars[iv].ndims - 1;
	tmp = hc.cor[id];
	hc.cor[id] = -1;	/* try negative coordinate, should fail */
	if (TEST_FAILS(iv)) {
	    error("%s: ncvarputg should fail for negative corner", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.cor[id] = tmp;
	tmp = hc.edg[id];
	hc.edg[id] = -1;	/* try negative edge, should fail */
	if (TEST_FAILS(iv)) {
	    error("%s: ncvarputg should fail for negative edge", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.edg[id] = tmp;
	tmp = hc.cor[id];
	hc.cor[id] = test.dims[id].size; /* try big coordinate, should fail */
	if (TEST_FAILS(iv)) {
	    error("%s: ncvarputg should fail for too-high coordinate", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.cor[id] = tmp;
	tmp = hc.edg[id];
	hc.edg[id] = test.dims[id].size + 1; /* try big edge, should fail */
	if (TEST_FAILS(iv)) {
	    error("%s: ncvarputg should fail for too-high edge", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	hc.edg[id] = tmp;
	tmp = strides[id];
	strides[id] = -1; /* try non-positive stride, * should fail */
	if (TEST_FAILS(iv)) {
	    error("%s: ncvarputg should fail for non-positive stride", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	strides[id] = tmp;

	if (ncredef(cdfid) == -1) {
	    error("%s: ncredef failed", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	/* try in define mode, should fail */
	if (TEST_FAILS(iv)) {
	    error("%s: ncvarputg should fail in define mode", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	if (ncendef (cdfid) == -1) {
	    error("%s: ncendef failed", pname);
	    ncclose(cdfid); return ++nerrs;
	}
    }
    /* try with bad variable handle, should fail */
    if (TEST_FAILS(-1)) {
	error("%s: ncvarputg should fail for bad variable handle", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* try with bad netCDF handle, should fail */
    if (TEST_FAILS(0)) {
	error("%s: ncvarputg failed to report bad netcdf handle", pname);
	nerrs++;
    }
    free (hc.vals);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}
