/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/dimtests.c,v 1.14 2006/10/31 16:21:54 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "emalloc.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "add.h"		/* functions to update in-memory netcdf */
#include "error.h"
#include "tests.h"

/* 
 * Test ncdimdef
 *    try in data mode, check error
 *    check that returned id is one more than previous id
 *    try adding same dimension twice, check error
 *    try with illegal sizes, check error
 *    make sure unlimited size works, shows up in ncinquire(...,*xtendim)
 *    try to define a second unlimited dimension, check error
 */
int
test_ncdimdef(path)
     const char *path;		/* name of writable netcdf to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncdimdef";
    int cdfid;			/* netcdf id */
    static struct cdfdim mm =	/* dimension */
      {"mm", 1};		/* 1 should be a valid dimension size */
    static struct cdfdim nn =	/* dimension */
      {"bogus", ___};		/* used for testing invalid dimension sizes */
    static struct cdfdim rec =	/* dimension */
      {"rec", NC_UNLIMITED};
    int ndims;			/* number of dimensions */
    int nvars;			/* number of variables */
    int natts;			/* number of attributes */
    int xdimid;			/* id of unlimited dimension, or -1 if none */
    int dimid;			/* dimension id */

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened, defining a dimension should fail in data mode */
    if (ncdimdef(cdfid, mm.name, mm.size) != -1) {
	error("%s: ncdimdef should have failed in data mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* enter define mode */
    if (ncredef(cdfid) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in define mode OK, add a dimension */
    if ((dimid = ncdimdef(cdfid, mm.name, mm.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    add_dim(&test, &mm);	/* keep in-memory netcdf in sync */
    /* check that dim id returned is one more than previous dim id */
    if (dimid != test.ndims - 1) {
	error("%s: ncdimdef returned %d for dim id, expected %d",
	      pname, dimid, test.ndims-1);
	ncclose(cdfid); return ++nerrs;
    }

    /* try adding same dimension again, this should fail */
    if (ncdimdef(cdfid, mm.name, mm.size) != -1) {
	error("%s: ncdimdef should not have allowed redefinition", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try adding dimension with negative size, this should fail */
    if (ncdimdef(cdfid, nn.name, (long) -10) != -1) {
	error("%s: ncdimdef should not allow negative size dimension", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* if there is not already an unlimited size dimension, try adding one */
    if (ncinquire(cdfid, &ndims, &nvars, &natts, &xdimid) == -1) {
	error("%s: ncinquire failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (xdimid == -1) {
	if (ncdimdef(cdfid, rec.name, rec.size) == -1) {
	    error("%s: ncdimdef failed on NC_UNLIMITED dimension", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	add_dim(&test, &rec);
    }
    /* try adding another unlimited dimension, which should fail */
    if (ncdimdef(cdfid, "rec2", rec.size) != -1) {
	error("%s: ncdimdef should not allow second NC_UNLIMITED dimension",
	      pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    if (ncdimdef(cdfid, "rec2", rec.size) != -1) {
	error("%s: ncdimdef should fail on bad netCDF id", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/*
 * Test ncdimid
 *    check return with defined dimension in both modes
 *    try with undefined dimension, check error
 *    check return with unlimited size dimension
 *    try with bad handle, check error
 */
int
test_ncdimid(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncdimid";
    int cdfid;			/* netcdf id */
    int nn_dim;			/* dimension id */
    static struct cdfdim nn =	/* dimension */
      {"nn", 1};		/* 1 should be a valid dimension size */

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened, enter define mode */
    if (ncredef(cdfid) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in define mode OK, add a dimension */
    if ((nn_dim = ncdimdef(cdfid, nn.name, nn.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    add_dim(&test, &nn);	/* keep in-memory netcdf in sync */
    /* check id returned for name matches id returned from definition */
    if (ncdimid(cdfid, nn.name) != nn_dim) {
	error("%s: ncdimid returned wrong value in define mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in data mode, check returned id for dimension just added */
    if (ncdimid(cdfid, nn.name) != nn_dim) {
	error("%s: ncdimid returned wrong value in data mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try with undefined dimension, should fail */
    if (ncdimid(cdfid, "easter-bunny") != -1) {
	error("%s: ncdimid with bogus name should have failed ", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try with unlimited dimension, assumed to be "rec" from earlier calls */
    if (ncdimid(cdfid, "rec") != test.xdimid) {
	error("%s: ncdimid returned bad value for record dimension", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* try on bad handle, should fail */
    if (ncdimid(cdfid, nn.name) != -1) {
	error("%s: ncdimid failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/*
 * Test ncdiminq
 *    try in both modes
 *    check returned name and size against defined name and size	
 *    try with bad dimension handle, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncdiminq(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncdiminq";
    int cdfid;			/* netcdf id */
    int dimid;			/* dimension id */
    struct cdfdim dim;		/* dimension */

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened, in data mode */
    dim.name = (char *) emalloc(MAX_NC_NAME);
    for (dimid = 0 ; dimid < test.ndims; dimid++) { /* loop on all dim ids */
	if (ncdiminq(cdfid, dimid, dim.name, &dim.size) == -1) {
	    error("%s: ncdiminq in data mode failed on dim id %d",
		  pname, dimid);
	    ncclose(cdfid); return ++nerrs;
	}
	/* compare returned with expected values */
	if (strcmp(dim.name, test.dims[dimid].name) != 0) {
	    error("%s: ncdiminq (data mode), name %s, expected %s for id = %d",
		pname, dim.name, test.dims[dimid].name, dimid);
	    nerrs++;
	}
	if (dim.size != test.dims[dimid].size) {
	    error("%s: ncdiminq (data mode), size %d, expected %d for id = %d",
		pname, dim.size, test.dims[dimid].size, dimid);
	    nerrs++;
	}
    }
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in define mode, compare returned with expected values again */
    for (dimid = 0 ; dimid < test.ndims; dimid++) { /* loop on all dim ids */
	if (ncdiminq(cdfid, dimid, dim.name, &dim.size) == -1) {
	    error("%s: ncdiminq in define mode failed on dim id %d",
		  pname, dimid);
	    ncclose(cdfid); return ++nerrs;
	}
	/* compare returned with expected values */
	if (strcmp(dim.name, test.dims[dimid].name) != 0) {
	    error("%s: ncdiminq (define), name %s, expected %s for id = %d",
		pname, dim.name, test.dims[dimid].name, dimid);
	    nerrs++;
	}
	if (dim.size != test.dims[dimid].size) {
	    error("%s: ncdiminq (define), size %d, expected %d for id = %d",
		pname, dim.size, test.dims[dimid].size, dimid);
	    nerrs++;
	}
    }
    /* try with bad dimension handles, check for failure */
    if (ncdiminq(cdfid, -1, dim.name, &dim.size) != -1 ||
	ncdiminq(cdfid, test.ndims, dim.name, &dim.size) != -1) {
	error("%s: ncdiminq should have failed on bad dimension ids",
	      pname, dimid);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* should fail, since bad handle */
    if (test.ndims >= 1) {	/* if any dimensions have been defined */
	if (ncdiminq (cdfid, 0, dim.name, &dim.size) != -1) {
	    error("%s: ncdiminq failed to report bad netcdf handle ", pname);
	    nerrs++;
	}
    }
    free(dim.name);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}    
    
/*
 * Test ncdimrename
 *    check that proper rename worked with ncdiminq
 *    try renaming to existing dimension name, check error
 *    try with bad dimension handle, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncdimrename(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncdimrename";
    int cdfid;			/* netcdf id */
    int pp_dim;			/* dimension id */
    static struct cdfdim pp =	/* dimension */
      {"pp", 7};
    static char newname[MAX_NC_NAME] = /* dimension name */
      "new_name";
    struct cdfdim dim;		/* dimension */
    static struct cdfdim qq =	/* dimension */
      {"qq", 10};

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened */
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in define mode, add two dimensions */
    if ((pp_dim = ncdimdef(cdfid, pp.name, pp.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    add_dim(&test, &pp);	/* keep in-memory netcdf in sync */
    if (ncdimdef(cdfid, qq.name, qq.size) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    add_dim(&test, &qq);	/* keep in-memory netcdf in sync */
    /* rename first dimension */
    if (ncdimrename(cdfid, pp_dim, newname) == -1) {
	error("%s: ncdimrename failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* check new name with ncdiminq */
    dim.name = (char *) emalloc(MAX_NC_NAME);
    if (ncdiminq(cdfid, pp_dim, dim.name, &dim.size) == -1) {
	error("%s: ncdiminq failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (strcmp(dim.name,pp.name) == 0) {
	error("%s: ncdimrename failed to change name", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (strcmp(dim.name,newname) != 0) {
	error("%s: ncdimrename changed name to %s instead of %s",
	      pname, dim.name, newname);
	ncclose(cdfid); return ++nerrs;
    }
    test.dims[pp_dim].name = (char *) erealloc((void *)test.dims[pp_dim].name,
					      strlen(newname)+1);
    (void) strcpy(test.dims[pp_dim].name, newname); /* keep test consistent */
    /* try to rename first dimension same as second, should fail */
    if (ncdimrename(cdfid, pp_dim, qq.name) != -1) {
	error("%s: ncdimrename should have failed with used name", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try with bad dimension handles, check for failure */
    if (ncdimrename(cdfid, -1, dim.name) != -1 ||
	ncdimrename(cdfid, test.ndims, dim.name) != -1) {
	error("%s: ncdimrename should have failed on bad dimension ids",
	      pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }

    /* in data mode, rename to shorter name */
    if (ncdimrename(cdfid, pp_dim, "p") == -1) {
	error("%s: ncdimrename to shorter name failed in data mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    test.dims[pp_dim].name = (char *) erealloc((void *)test.dims[pp_dim].name,
					      strlen("p")+1);
    (void) strcpy(test.dims[pp_dim].name, "p"); /* keep test consistent */
    /* Check with ncdimid */
    if (pp_dim != ncdimid(cdfid, "p")) {
	error("%s: lookup by name in data mode failed after ncdimrename",
	      pname);	
	return ++nerrs;
    }
    /* in data mode, restore old name */
    if (ncdimrename(cdfid, pp_dim, pp.name) == -1) {
	error("%s: ncdimrename failed in data mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    test.dims[pp_dim].name = (char *) erealloc((void *)test.dims[pp_dim].name,
					      strlen(pp.name)+1);
    (void) strcpy(test.dims[pp_dim].name, pp.name); /* keep test consistent */
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* should fail, since bad handle */
    if (ncdimrename (cdfid, 0, dim.name) != -1) {
	error("%s: ncdimrename failed to report bad netcdf handle ", pname);
	nerrs++;
    }
    free (dim.name);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}
