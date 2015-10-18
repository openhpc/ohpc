/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/vartests.c,v 1.19 2006/10/31 16:20:49 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "emalloc.h"
#include "add.h"		/* functions to update in-memory netcdf */
#include "error.h"
#include "tests.h"

#define LEN_OF(array) ((sizeof array) / (sizeof array[0]))


/*
 * Test ncvarid
 *    check that proper variable handle returned in both modes
 *    try with undefined name, check error
 *    try with bad handle, check error
 */
int
test_ncvarid(path)
     const char *path;		/* name of writable netcdf file to open */
{
    static char pname[] = "test_ncvarid";
    int cdfid;			/* netcdf id */
    int id;
    int varid;			/* variable id */
    static struct cdfvar xx =	/* variable */
      {"xx", NC_FLOAT, 1, ___, 0};
    int nerrs = 0;

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
    /* in define mode, add a variable */
    xx.dims = (int *) emalloc(sizeof(int) * xx.ndims);
    for (id = 0; id < xx.ndims; id++)
      xx.dims[id] = id;
    if ((varid = ncvardef(cdfid,
			   xx.name, xx.type, xx.ndims, xx.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    add_var(&test, &xx);	/* keep in-memory netcdf in sync */

    /* check id returned for name matches id returned from definition */
    if (ncvarid(cdfid, xx.name) != varid) {
	error("%s: ncvarid returned wrong value in define mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in data mode, check returned id for variable just added */
    if (ncvarid(cdfid, xx.name) != varid) {
	error("%s: ncvarid returned wrong value in data mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try with undefined variable, should fail */
    if (ncvarid(cdfid, "santa-claus") != -1) {
	error("%s: ncvarid with bogus name should have failed ", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* try on bad handle, should fail */
    if (ncvarid(cdfid, xx.name) != -1) {
	error("%s: ncvarid failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    free(xx.dims);
    return nerrs;
}


/*
 * Test ncvarinq
 *    try in both modes
 *    check returned values against defined values
 *    try with bad variable handle, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncvarinq(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarinq";
    int cdfid;			/* netcdf id */
    int varid;			/* variable id */
    struct cdfvar var;		/* variable */
    int idim;

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened, in data mode */
    var.dims = (int *) emalloc(sizeof(int) * MAX_VAR_DIMS);
    var.name = (char *) emalloc(MAX_NC_NAME);
    for (varid = 0 ; varid < test.nvars; varid++) { /* loop on all var ids */
	if (ncvarinq(cdfid, varid, var.name, &var.type,
		      &var.ndims, var.dims, &var.natts) == -1) {
	    error("%s: ncvarinq in data mode failed on var id %d",
		  pname, varid);
	    ncclose(cdfid); return ++nerrs;
	}
	/* compare returned with expected values */
	if (strcmp(var.name, test.vars[varid].name) != 0) {
	    error("%s: ncvarinq (data mode), name %s, expected %s for id = %d",
		  pname, var.name, test.vars[varid].name, varid);
	    nerrs++;
	}
	if (var.type != test.vars[varid].type) {
	    error("%s: ncvarinq (data mode), type %d, expected %d for id = %d",
		  pname, var.type, test.vars[varid].type, varid);
	    nerrs++;
	}
	if (var.ndims != test.vars[varid].ndims) {
	    error("%s: ncvarinq (data mode), ndims %d, expected %d for id = %d",
		  pname, var.ndims, test.vars[varid].ndims, varid);
	    nerrs++;
	}
	else {			/* if ndims OK, compare dims */
	    for (idim = 0; idim < var.ndims; idim++)
	      if (var.dims[idim] != test.vars[varid].dims[idim]) {
		  error("%s: ncvarinq (data mode), dims[%d]=%d, expected %d",
			pname, idim, var.dims[idim],
			test.vars[varid].dims[idim]);
		  nerrs++;
	      }
	}
    }
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in define mode, compare returned with expected values again */
    for (varid = 0 ; varid < test.nvars; varid++) { /* loop on all var ids */
	if (ncvarinq(cdfid, varid, var.name, &var.type,
		      &var.ndims, var.dims, &var.natts) == -1) {
	    error("%s: ncvarinq in data mode failed on var id %d",
		  pname, varid);
	    ncclose(cdfid); return ++nerrs;
	}
	if (strcmp(var.name, test.vars[varid].name) != 0) {
	    error("%s: ncvarinq (define mode), name %s, expected %s for id = %d",
		  pname, var.name, test.vars[varid].name, varid);
	    nerrs++;
	}
	if (var.type != test.vars[varid].type) {
	    error("%s: ncvarinq (define mode), type %d, expected %d for id = %d",
		  pname, var.type, test.vars[varid].type, varid);
	    nerrs++;
	}
	if (var.ndims != test.vars[varid].ndims) {
	    error("%s: ncvarinq (define mode), ndims %d, expected %d for id = %d",
		  pname, var.ndims, test.vars[varid].ndims, varid);
	    nerrs++;
	}
	else {			/* if ndims OK, compare dims */
	    for (idim = 0; idim < var.ndims; idim++)
	      if (var.dims[idim] != test.vars[varid].dims[idim]) {
		  error("%s: ncvarinq (define mode), dims[%d]=%d, expected %d",
			pname, idim, var.dims[idim],
			test.vars[varid].dims[idim]);
		  nerrs++;
	      }
	}
    }
    /* try with bad variable handles, check for failure */
    if (ncvarinq(cdfid, -1, var.name, &var.type,
		      &var.ndims, var.dims, &var.natts) != -1 ||
	ncvarinq(cdfid, test.nvars, var.name, &var.type,
		      &var.ndims, var.dims, &var.natts) != -1) {
	error("%s: ncvarinq should have failed on bad variable ids",
	      pname, varid);
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
    if (test.nvars >= 1) {	/* if any variables have been defined */
	if (ncvarinq(cdfid, varid, var.name, &var.type,
		      &var.ndims, var.dims, &var.natts) != -1) {
	    error("%s: ncvarinq failed to report bad netcdf handle ", pname);
	    nerrs++;
	}
    }
    free(var.dims);
    free(var.name);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


struct cdfelm {			/* coordinates and generic value */
    long coords[MAX_NC_DIMS];
    union generic {
	char by;
	char ch;
	short sh;
	nclong lo;
	float fl;
	double db;
    } val;
};


/* 
 * Test both ncvarput1 and ncvarget1 with all types of data
 *    use points in "lower-left", "middle", and "upper-right"
 *    for each existing variable, put values of its type at each point
 *    get values and compare with put values
 */
static int
test_varputget1(cdfid)
     int cdfid;			/* handle of netcdf open and in data mode */
{
    int nerrs = 0;
    static char pname[] = "test_varputget1";
    int id, ie, iv;
    int ne = 3;			/* number of test points */
    struct cdfelm elm[3];	/* coordinates and values of test points */
    static long edges[] = {1};
    void *voidp;
    void *tmpp;
    char chval;
    short shval;
    nclong loval;
    float flval;
    double dbval;

    for (iv = 0; iv < test.nvars; iv++)	{ /* for each var in netcdf */
	for (id = 0; id < test.vars[iv].ndims; id++) { /* set corners */
	    int dsize;		/* max dimension size, used for u-r corner */
	    /* "lower-left" corner */
	    elm[0].coords[id] = 0;
	    /* if unlimited dimension, choose record 3 for max, arbitrarily */
	    dsize = (int) test.dims[test.vars[iv].dims[id]].size;
	    if (dsize == NC_UNLIMITED)
	      dsize = 3;
	    /* middle */
	    elm[1].coords[id] = dsize / 2;
	    /* "upper-right" corner */
	    elm[2].coords[id] = dsize - 1;
	}
	for (ie = 0; ie < ne; ie++) { /* for each of ne points */
	    switch (test.vars[iv].type) { /* get values of right type to put */
	      case NC_BYTE:
	      case NC_CHAR:
		elm[ie].val.by = (char) (ie+1);
		voidp = (void *) &elm[ie].val.by;
		tmpp = (void *) &chval;
		break;
	      case NC_SHORT:
		elm[ie].val.sh = (short) (ie-1);
		voidp = (void *) &elm[ie].val.sh;
		tmpp = (void *) &shval;
		break;
	      case NC_LONG:
		elm[ie].val.lo = (nclong) (ie-3);
		voidp = (void *) &elm[ie].val.lo;
		tmpp = (void *) &loval;
		break;
	      case NC_FLOAT:
		elm[ie].val.fl = (float) (ie+1);
		voidp = (void *) &elm[ie].val.fl;
		tmpp = (void *) &flval;
		break;
	      case NC_DOUBLE:
		elm[ie].val.db = (double) (ie-1);
		voidp = (void *) &elm[ie].val.db;
		tmpp = (void *) &dbval;
		break;
	      default:
		error("%s: bad type, test program error", pname);
	    }
	    if(ncvarput1 (cdfid, iv, elm[ie].coords, voidp) == -1) {
		error("%s: ncvarput1 failed for point %d, variable %s",
		      pname, ie, test.vars[iv].name);
		ncclose(cdfid); return 1;
	    }
	    add_data(&test, iv, elm[ie].coords, edges); /* keep test in sync */

	    if(ncvarget1 (cdfid, iv, elm[ie].coords, tmpp) == -1) {
		error("%s: ncvarget1 failed for point %d, variable %s",
		      pname, ie, test.vars[iv].name);
		ncclose(cdfid); return 1;
	    }
	    switch (test.vars[iv].type) { /* compare values of right type */
	      case NC_BYTE:
	      case NC_CHAR:
		if (elm[ie].val.by != chval) {
		    error("%s: ncvarget1 returned char %d, expected %d",
			  pname, chval, elm[ie].val.by);
		    nerrs++;
		}
		break;
	      case NC_SHORT:
		if (elm[ie].val.sh != shval) {
		    error("%s: ncvarget1 returned short %d, expected %d",
			  pname, shval, elm[ie].val.sh);
		    nerrs++;
		}
		break;
	      case NC_LONG:
		if (elm[ie].val.lo != loval) {
		    error("%s: ncvarget1 returned long %ld, expected %ld",
			  pname, (long)loval, (long)elm[ie].val.lo);
		    nerrs++;
		}
		break;
	      case NC_FLOAT:
		if (elm[ie].val.fl != flval) {
		    error("%s: ncvarget1 returned float %g, expected %g",
			  pname, flval, elm[ie].val.fl);
		    nerrs++;
		}
		break;
	      case NC_DOUBLE:
		if (elm[ie].val.db != dbval) {
		    error("%s: ncvarget1 returned double %g, expected %g",
			  pname, dbval, elm[ie].val.db);
		    nerrs++;
		}
		break;
	      default:
		error("%s: bad type, test program error", pname);
	    }
	}
    }
   
    return nerrs;
}


/*
 * Test ncvarput1
 *    check that proper call worked with ncvarget1
 *    try with negative coords, check error
 *    try with too-large coords, check error
 *    try with bad variable handle, check error
 *    try in define mode, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncvarput1(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarput1";
    int cdfid;			/* netcdf id */
    int iv;			/* variable id */
    struct cdfelm elm;		/* coordinates and value of test point */

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened in data mode, try putting and getting values of each type */
    nerrs += test_varputget1 (cdfid);	/* tests ncvarput1 and ncvarget1 */

    /* find a variable with at least one dimension */
    iv = 0;
    while (test.vars[iv].ndims <= 0 && iv < test.nvars)
      iv++;
    if (iv < test.nvars) {	/* iv is varid of variable with dimensions */
	/* set coords */
	int id;			/* dimension id */
	for (id = 0; id < test.vars[iv].ndims; id++)
	  elm.coords[id] = 0;
	/* try invalid coordinates, should fail */
	elm.coords[test.vars[iv].ndims/2] = -1;
	if(ncvarput1 (cdfid, iv, elm.coords, (void *) &elm.val) != -1) {
	    error("%s: ncvarput1 should fail for negative coordinate", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	elm.coords[test.vars[iv].ndims/2] =
	  test.dims[test.vars[iv].dims[test.vars[iv].ndims/2]].size;
	if(ncvarput1 (cdfid, iv, elm.coords, (void *) &elm.val) != -1) {
	    error("%s: ncvarput1 should fail for too-high coordinate", pname);
	    ncclose(cdfid); return ++nerrs;
	}
    }
    /* try with bad variable handle, should fail */
    if(ncvarput1 (cdfid, -1, elm.coords, (void *) &elm.val) != -1 ||
       ncvarput1 (cdfid, test.nvars, elm.coords, (void *) &elm.val) != -1) {
	error("%s: ncvarput1 should fail for bad variable handle", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try in define mode, should fail */
    if (test.nvars > 0)
      if(ncvarput1 (cdfid, 0, elm.coords, (void *) &elm.val) != -1) {
	  error("%s: ncvarput1 should fail in define mode", pname);
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
    /* try with bad netCDF handle, should fail */
    if(ncvarput1 (cdfid, 0, elm.coords, (void *) &elm.val) != -1) {
	error("%s: ncvarput1 failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/*
 * Test ncvarget1
 *    check that proper call worked after ncvarput1
 *    try with negative coords, check error
 *    try with too-large coords, check error
 *    try with bad variable handle, check error
 *    try in define mode, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncvarget1(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarget1";
    int cdfid;			/* netcdf id */
    int iv;			/* variable id */
    struct cdfelm elm;		/* coordinates and value of test point */

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened in data mode, try putting and getting values of each type */
    nerrs += test_varputget1 (cdfid);	/* tests ncvarput1 and ncvarget1 */

    /* find a variable with at least one dimension */
    iv = 0;
    while (test.vars[iv].ndims <= 0 && iv < test.nvars)
      iv++;
    if (iv < test.nvars) {	/* iv is varid of variable with dimensions */
	/* set coords */
	int id;			/* dimension id */
	for (id = 0; id < test.vars[iv].ndims; id++)
	  elm.coords[id] = 0;
	/* try invalid coordinates, should fail */
	elm.coords[test.vars[iv].ndims/2] = -1;
	if(ncvarget1 (cdfid, iv, elm.coords, (void *) &elm.val) != -1) {
	    error("%s: ncvarget1 should fail for negative coordinate", pname);
	    ncclose(cdfid); return ++nerrs;
	}
	elm.coords[test.vars[iv].ndims/2] =
	  test.dims[test.vars[iv].dims[test.vars[iv].ndims/2]].size;
	if(ncvarget1 (cdfid, iv, elm.coords, (void *) &elm.val) != -1) {
	    error("%s: ncvarget1 should fail for too-high coordinate", pname);
	    ncclose(cdfid); return ++nerrs;
	}
    }
    /* try with bad variable handle, should fail */
    if(ncvarget1 (cdfid, -1, elm.coords, (void *) &elm.val) != -1 ||
       ncvarget1 (cdfid, test.nvars, elm.coords, (void *) &elm.val) != -1) {
	error("%s: ncvarget1 should fail for bad variable handle", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try in define mode, should fail */
    if (test.nvars > 0)
      if(ncvarget1 (cdfid, 0, elm.coords, (void *) &elm.val) != -1) {
	  error("%s: ncvarget1 should fail in define mode",
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
    /* try with bad netCDF handle, should fail */
    if(ncvarget1 (cdfid, 0, elm.coords, (void *) &elm.val) != -1) {
	error("%s: ncvarget1 failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/*
 * Test ncvarrename
 *    check that proper rename worked with ncvarinq
 *    try with bad netCDF handle, check error
 *    try in data mode, check error
 *    try with bad variable handle, check error
 *    try renaming to existing variable name, check error
 */
int
test_ncvarrename(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarrename";
    int cdfid;			/* netcdf id */
    int id;			/* dimension id */
    int yy_id;			/* variable id */
    static struct cdfvar yy =	/* variable */
      {"old_name", NC_SHORT, 1, ___, 0};
    static char newname[] = "yyy"; /* variable name */
    static char shortname[] = "yy"; /* variable name */
    struct cdfvar var;		/* variable */
    static struct cdfvar zz =	/* variable */
      {"zz", NC_BYTE, 2, ___, 0};

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
    /* in define mode, add two variables */
    yy.dims = (int *) emalloc(sizeof(int) * yy.ndims);
    for (id = 0; id < yy.ndims; id++)
      yy.dims[id] = id;
    if ((yy_id = ncvardef(cdfid,
			   yy.name, yy.type, yy.ndims, yy.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    add_var(&test, &yy);	/* keep in-memory netcdf in sync */
    zz.dims = (int *) emalloc(sizeof(int) * zz.ndims);
    for (id = 0; id < zz.ndims; id++)
      zz.dims[id] = id;
    if (ncvardef(cdfid, zz.name, zz.type, zz.ndims, zz.dims) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    add_var(&test, &zz);	/* keep in-memory netcdf in sync */

    /* rename first variable */
    if (ncvarrename(cdfid, yy_id, newname) == -1) {
	error("%s: ncvarrename failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* check new name with ncvarid, ncvarinq */
    if (yy_id != ncvarid(cdfid, newname)) {
        error("%s: lookup by name failed after ncvarrename", pname);
    }
    var.dims = (int *) emalloc(sizeof(int) * MAX_VAR_DIMS);
    var.name = (char *) emalloc(MAX_NC_NAME);
    if (ncvarinq(cdfid, yy_id, var.name,
		  &var.type, &var.ndims, var.dims, &var.natts) == -1) {
	error("%s: ncvarinq failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (strcmp(var.name,yy.name) == 0) {
	error("%s: ncvarrename failed to change name", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (strcmp(var.name,newname) != 0) {
	error("%s: ncvarrename changed name to %s instead of %s",
	      pname, var.name, newname);
	ncclose(cdfid); return ++nerrs;
    }
    (void) strcpy(test.vars[yy_id].name, newname); /* keep test consistent */
    /* try to rename second variable same as first, should fail */
    if (ncvarrename(cdfid, yy_id, zz.name) != -1) {
	error("%s: ncvarrename should have failed with used name", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* try with bad variable handles, check for failure */
    if (ncvarrename(cdfid, -1, var.name) != -1 ||
	ncvarrename(cdfid, test.nvars, var.name) != -1) {
	error("%s: ncvarrename should have failed on bad variable ids",
	      pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return ++nerrs;
    }
    /* in data mode */
    if (ncvarrename(cdfid, yy_id, "a_longer_name") != -1) {
	error("%s: ncvarrename to longer should fail in data mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    if (ncvarrename(cdfid, yy_id, shortname) == -1) {
	error("%s: ncvarrename to shorter should succeed in data mode", pname);
	ncclose(cdfid); return ++nerrs;
    }
    (void) strcpy(test.vars[yy_id].name, shortname); /* keep test consistent */
    /* check new name with ncvarid, ncvarinq */
    if (yy_id != ncvarid(cdfid, shortname)) {
        error("%s: lookup by name in data mode failed after ncvarrename",
	      pname);
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* should fail, since bad handle */
    if (ncvarrename (cdfid, 0, var.name) != -1) {
	error("%s: ncvarrename failed to report bad netcdf handle ", pname);
	nerrs++;
    }
    free(yy.dims);
    free(zz.dims);
    free(var.name);
    free(var.dims);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}
