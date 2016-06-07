/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/cdftests.c,v 1.23 2009/02/14 14:11:28 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "emalloc.h"
#include "testcdf.h"		/* defines in-memory test netcdf structure */
#include "add.h"		/* functions to update in-memory netcdf */
#include "error.h"
#include "tests.h"

#define LEN_OF(array) ((sizeof array) / (sizeof array[0]))


/* 
 * Test nccreate
 *    create a netcdf with no data, close it, test that it can be opened
 *    try again with NC_CLOBBER mode, check that no errors occurred
 *    try again with NC_NOCLOBBER mode, check error return
 * On exit, netcdf files are closed.
 * Uses: nccreate, ncendef, ncclose, ncopen.
 */
int
test_nccreate(path)
     const char *path;		/* name of netCDF file to create */
{
    int nerrs = 0;
    static char pname[] = "test_nccreate";
    int ncid;

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((ncid = nccreate(path, NC_CLOBBER)) == -1) {
	error("%s: nccreate failed to NC_CLOBBER", pname);
	return 1;
    }
    /* in define mode already, so ncredef should fail  */
    if (ncredef(ncid) != -1) {
	error("%s: ncredef should fail after nccreate", pname);
	nerrs++;
    }
    /* created OK */
    if (ncendef (ncid) == -1) {
	error("%s: ncendef failed", pname);
	nerrs++;
    }
    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	nerrs++;
    }
    if ((ncid = ncopen(path, NC_NOWRITE)) == -1) {
	error("%s: ncopen of newly created netcdf failed", pname);
	return ++nerrs;
    }
    /* opened OK */
    if (ncclose (ncid) == -1) {
	error("%s: second ncclose failed", pname);
	nerrs++;
    }
    /* this call should fail, since we're using NC_NOCLOBBER mode */
    if (nccreate(path, NC_NOCLOBBER) != -1) {
	error("%s: nccreate failed to honor NC_NOCLOBBER mode", pname);
	nerrs++;
    }

    /* Initialize in-memory netcdf to empty */
    add_reset(&test);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/* 
 * Test ncopen
 *    try to open a non-existent netCDF, check error return
 *    open a file that is not a netCDF file, check error return
 *    open a netCDF with NC_WRITE mode, write something, close it
 *    open a netCDF with NC_NOWRITE mode, write something and check error
 *    try to open a netcdf twice, check whether returned netcdf ids different
 * On exit, netcdf files are closed.
 * Uses: ncopen, ncredef, ncattput, ncendef, ncclose.
 */
#define DATA_LEN 32    
#define TEMP_FILE_NAME "temp.tmp"
int
test_ncopen(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncopen";
    int ncid0, ncid1;
    static char title_val[] = "test netcdf";
    static char xpath[] = "tooth-fairy.nc"; /* must not exist */
    static struct cdfatt title = /* attribute */
      {NC_GLOBAL, "title", NC_CHAR, LEN_OF (title_val), (void *) title_val};
    FILE *temp;
    char dummy_data[DATA_LEN];
    int i;

    /* Initialize to keep valgrind happy. */
    for (i = 0; i < DATA_LEN; i++)
       dummy_data[i] = 0;

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    /* Open a nonexistent file */
    if(ncopen(xpath, NC_NOWRITE) != -1) {
	error("%s: ncopen should fail opening nonexistent file",
	      pname);
	return ++nerrs;
    }
    if (ncerr != NC_SYSERR) {
	error("%s: ncopen of nonexistent file should set ncerr to %d",
	      pname, NC_SYSERR);
    }
    /*
     * Open a non-netCDF file.  Don't use "Makefile.in" because that
     * name is munged to something else by PC/NFS and, consequently,
     * won't exist in a cross-mounted directory. Also don't use a
     * source file, becase that will break building in another
     * directory, and consequently, make dist. An object file is not
     * safe, because sometimes it's a .o and sometimes a .obj. So just
     * create a file!
     */
    if (!(temp = fopen(TEMP_FILE_NAME, "w+")))
    {
       error("could not create temp file");
       return ++nerrs;
    }
    if (fwrite(dummy_data, 1, DATA_LEN, temp) != DATA_LEN)
    {
       error("could not write to temp file");
       return ++nerrs;
    }
    if (fclose(temp))
    {
       error("could not close temp file");
       return ++nerrs;
    }
    
    if(ncopen(TEMP_FILE_NAME, NC_NOWRITE) != -1) {
	error("%s: ncopen should fail opening non-netCDF file",
	      pname);
	return ++nerrs;
    }
    if(ncerr != NC_ENOTNC) {
	error("%s: ncopen of non-netCDF file should set ncerr to %d",
	      pname, NC_ENOTNC);
	return ++nerrs;
    }


    if ((ncid0 = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed with NC_WRITE mode", pname);
	return ++nerrs;
    }

    /* opened */
    if (ncredef(ncid0) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(ncid0); return ++nerrs;
    }
    /* in define mode */
    if (ncattput(ncid0, NC_GLOBAL, "title", NC_CHAR, title.len, title.val)
	== -1) {
	error("%s: ncattput failed", pname);
	ncclose(ncid0); return ++nerrs;
    }
    add_att(&test, NC_GLOBAL, &title); /* keep in-memory netcdf updated */
    if (ncendef (ncid0) == -1) {
	error("%s: ncendef failed after ncattput", pname);
	ncclose(ncid0); return ++nerrs;
    }
    if (ncclose (ncid0) == -1) {
	error("%s: ncclose failed in NC_WRITE mode", pname);
	return ++nerrs;
    }

    if ((ncid0 = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed with NC_NOWRITE mode", pname);
	return ++nerrs;
    }
    if ((ncid1 = ncopen(path, NC_NOWRITE)) == -1) {
#ifndef vms
	error("%s: second ncopen failed", pname);
	nerrs++;
#else
	fprintf(stderr,"Doesn't support shared access on vms\n") ;
#endif
    }
    else
    {
       /* this should fail, since in NC_NOWRITE mode */
       if (ncredef(ncid1) != -1) {
	  error("%s: cdredef should fail after NC_NOWRITE open", pname);
	  ncclose(ncid1); return ++nerrs;
       }
       /* second open OK */
       if (ncid0 == ncid1) {
	  error("%s: ncopen should return new ncid on second open",
		pname);
	  nerrs++;
       }
       if (ncclose (ncid1) == -1) {
	  error("%s: ncclose failed to close after second open", pname);
	  nerrs++;
       }
    }
    if (ncclose (ncid0) == -1) {
	error("%s: ncclose failed in NC_NOWRITE mode", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/*
 * Test ncredef
 *    open a netCDF, enter define mode, add dimension, variable, attribute
 *    try ncredef from within define mode, check error
 *    leave define mode and close, releasing netcdf handle
 *    try ncredef with old handle, check error
 * On exit netcdf files are closed.
 * Uses: ncopen, ncredef, ncdimdef, ncvardef, ncattput, ncclose 
 */
int
test_ncredef(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncredef";
    int ncid;			/* netcdf id */
    int ii_dim;			/* dimension id */
    static struct cdfdim ii =	/* dimension */
      {"ii", 4};
    int aa_id;			/* variable id */
    static struct cdfvar aa =	/* variable */
      {"aa", NC_LONG, 1, ___, 0};
    static char units_val[] = "furlongs";
    static struct cdfatt aa_units = /* attribute */
      {___, "units", NC_CHAR, LEN_OF(units_val), (void *)units_val};

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened OK, enter define mode */
    if (ncredef(ncid) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* in define mode OK, add a dimension */
    if ((ii_dim = ncdimdef(ncid, ii.name, ii.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    add_dim(&test, &ii);	/* keep in-memory netcdf in sync */

    /* dimension added OK, add a variable */
    aa.dims = (int *)emalloc(sizeof(int) * aa.ndims);
    aa.dims[0] = ii_dim;
    if ((aa_id = ncvardef(ncid, aa.name, aa.type,
			   aa.ndims, aa.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    add_var(&test, &aa);	/* keep in-memory netcdf in sync */

    /* variable added OK, add a variable attribute */
    aa_units.var = aa_id;
    if (ncattput(ncid, aa_units.var, aa_units.name,
		  aa_units.type, aa_units.len, (void *) aa_units.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    add_att(&test, aa_id, &aa_units); /* keep in-memory netcdf in sync */

    if (ncredef(ncid) != -1) {
	error("%s: cdredef in define mode should have failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    if (ncendef (ncid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    if (ncredef(ncid) != -1) {
	error("%s: ncredef failed to report bad netcdf handle", pname);
	nerrs++;
    }
    free (aa.dims);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/* 
 * Test ncendef
 *    check return from proper cdfendif after define mode
 *    try ncendef when in data mode, check error
 *    try ncendef with bad handle, check error
 *  On exit netcdf files are closed.
 * Uses: ncopen, ncredef, ncdimdef, ncvardef, ncattput, ncendef, ncclose
 */
int
test_ncendef(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncendef";
    int ncid;			/* netcdf id */
    int jj_dim, kk_dim;		/* dimension ids */
    int bb_id;			/* variable id */
    static struct cdfdim kk =	/* dimension */
      {"kk", 3};
    static struct cdfdim jj =	/* dimension */
      {"jj", 3};
    static struct cdfvar bb =	/* variable */
      {"bb", NC_LONG, 2, ___, 0};
    static float bb_rangev[2] = {0., 100.}; /* attribute vector */
    static struct cdfatt bb_range = /* attribute */
      {___, "valid_range", NC_FLOAT, LEN_OF(bb_rangev), (void *)bb_rangev};

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened */
    if (ncredef(ncid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* in define mode, add dimensions */
    if ((jj_dim = ncdimdef(ncid, jj.name, jj.size)) == -1 ||
	(kk_dim = ncdimdef(ncid, kk.name, kk.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    add_dim(&test, &jj);	/* keep in-memory netcdf in sync */
    add_dim(&test, &kk);	/* keep in-memory netcdf in sync */
    
    /* dimensions added OK, add a variable */
    bb.dims = (int *) emalloc(sizeof(int) * bb.ndims);
    bb.dims[0] = kk_dim;
    bb.dims[1] = jj_dim;
    if ((bb_id = ncvardef(ncid, bb.name, bb.type,
			   bb.ndims, bb.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    add_var(&test, &bb);	/* keep in-memory netcdf in sync */
    
    /* variable added OK, add a variable attribute */
    if (ncattput(ncid, bb_id, bb_range.name, bb_range.type, bb_range.len,
		  (void *) bb_range.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    add_att(&test, bb_id, &bb_range); /* keep in-memory netcdf in sync */
    
    if (ncendef (ncid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* in data mode */
    if (ncendef (ncid) != -1) { /* should fail in data mode */
	error("%s: ncendef in data mode should have failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* should fail on a bad handle */
    if (ncendef (ncid) != -1) {
	error("ncendef failed to report bad netcdf handle");
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    free(bb.dims);

    return nerrs;
}


/* 
 * Test ncclose
 *    try on open netCDF
 *    try in define mode and data mode
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
int
test_ncclose(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncclose";
    int ncid;			/* netcdf id */

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened */
    if (ncredef(ncid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* in define mode */
    if (ncclose (ncid) == -1) {
	error("%s: ncclose in define mode failed", pname);
	nerrs++;
    }

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* in data mode */
    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	nerrs++;
    }
    if (ncclose (ncid) != -1) { /* should fail, since ncid is a bad handle */
	error("%s: ncclose failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/* 
 * Test ncinquire
 *    try in data mode, check returned values
 *    try in define mode, after adding an unlimited dimension, variable
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
int
test_ncinquire(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncinquire";
    int ncid;			/* netcdf id */
    int ndims;			/* number of dimensions */
    int nvars;			/* number of variables */
    int ngatts;			/* number of global attributes */
    int xdimid;			/* id of unlimited dimension */
    int rec_dim;		/* dimension id */
    static struct cdfdim rec =	/* dimension */
      {"rec", NC_UNLIMITED};
    static struct cdfdim dims[] = { /* dimensions */
	{"i1", 5},{"i2", 3},{"i3", 7}
    };
    int id, nd = LEN_OF(dims);	/* number of dimensions */
    int dimids[LEN_OF(dims)];
    int cc_id;			/* variable id */
    static struct cdfvar cc[] =	{ /* record variables of various sizes */
	{"cc", NC_LONG, 1, ___, 0},
	{"cd", NC_SHORT, 2, ___, 0},
	{"ce", NC_FLOAT, 3, ___, 0}
    };
    int iv;
    int nv = LEN_OF(cc);	/* number of record variables */
    static char units_val[] = "moles";
    static struct cdfatt cc_units = /* attribute */
      {___, "units", NC_CHAR, LEN_OF(units_val), (void *)units_val};

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened, in data mode */
    if (ncinquire(ncid, &ndims, &nvars, &ngatts, &xdimid) == -1) {
	error("%s: ncinquire in data mode failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* compare returned with expected values */
    if (ndims != test.ndims) {
	error("%s: ndims returned as %d, expected %d",
	    pname, ndims, test.ndims);
	nerrs++;
    }
    if (nvars != test.nvars) {
	error("%s: nvars returned as %d, expected %d",
	    pname, nvars, test.nvars);
	nerrs++;
    }
    if (ngatts != test.ngatts) {
	error("%s: ngatts returned as %d, expected %d",
	    pname, ngatts, test.ngatts);
	nerrs++;
    }
    if (xdimid != test.xdimid) {
	error("%s: xdimid returned as %d, expected %d",
	    pname, xdimid, test.xdimid);
	nerrs++;
    }

    if (ncredef(ncid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* add dimensions */
    for (id = 0; id < nd; id++) {
	if ((dimids[id] = ncdimdef(ncid, dims[id].name, dims[id].size))
	    == -1) {
	    error("%s: ncdimdef failed on normal dimension", pname);
	    ncclose(ncid); return ++nerrs;
	}
	add_dim(&test, &dims[id]);
    }

    /* add an unlimited dimension */
    if ((rec_dim = ncdimdef(ncid, rec.name, rec.size)) == -1) {
	error("%s: ncdimdef failed on NC_UNLIMITED dimension", pname);
	ncclose(ncid); return ++nerrs;
    }
    add_dim(&test, &rec);

    /* add some record variables */
    for (iv = 0; iv < nv; iv++) {
	cc[iv].dims = (int *) emalloc(sizeof(int) * cc[iv].ndims);
	cc[iv].dims[0] = rec_dim; /* first dimension unlimited */
	for (id = 1; id < cc[iv].ndims; id++)
	  cc[iv].dims[id] = dimids[id];
	if ((cc_id = ncvardef(ncid, cc[iv].name, cc[iv].type,
			       cc[iv].ndims, cc[iv].dims)) == -1) {
	    error("%s: ncvardef failed", pname);
	    ncclose(ncid); return ++nerrs;
	}
	add_var(&test, &cc[iv]);

	/* add a variable attribute */
	if (ncattput(ncid, cc_id, cc_units.name, cc_units.type,
		      cc_units.len, (void *) cc_units.val) == -1) {
	    error("%s: ncattput failed", pname);
	    ncclose(ncid); return ++nerrs;
	}
	add_att(&test, cc_id, &cc_units);
	free(cc[iv].dims);
    }
    /* try calling from define mode, compare returned values to expected */
    if (ncinquire(ncid, &ndims, &nvars, &ngatts, &xdimid) == -1) {
	error("%s: ncinquire in define mode failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* compare returned with expected values */
    if (ndims != test.ndims) {
	error("%s: ndims returned as %d, expected %d",
	    pname, ndims, test.ndims);
	nerrs++;
    }
    if (nvars != test.nvars) {
	error("%s: nvars returned as %d, expected %d",
	    pname, nvars, test.nvars);
	nerrs++;
    }
    if (ngatts != test.ngatts) {
	error("%s: ngatts returned as %d, expected %d",
	    pname, ngatts, test.ngatts);
	nerrs++;
    }
    if (xdimid != test.xdimid) {
	error("%s: xdimid returned as %d, expected %d",
	    pname, xdimid, test.xdimid);
	nerrs++;
    }

    if (ncendef (ncid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(ncid); return ++nerrs;
    }

    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    /* should fail, since bad handle */
    if (ncinquire (ncid, &ndims, &nvars, &ngatts, &xdimid) != -1) {
	error("%s: ncinquire failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/*
 * Test ncsync
 *    try in define mode, check error
 *    try writing with one handle, reading with another on same netCDF
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
int
test_ncsync(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncsync";
    int ncid0, ncid1;		/* netcdf ids */
    int ll_dim;			/* dimension id */
    static struct cdfdim ll =	/* dimension */
      {"ll", 3};
    int dd_id;			/* variable id */
    static struct cdfvar dd =	/* variable */
      {"dd", NC_SHORT, 1, ___, 0};
    static short dd_fill_valv[] = {-999};
    static struct cdfatt dd_fill_val = /* attribute */
      {___, "fill_value", NC_SHORT, LEN_OF(dd_fill_valv), (void *) dd_fill_valv};

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((ncid0 = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen in NC_WRITE mode failed", pname);
	return ++nerrs;
    }

    /* opened */
    if (ncredef(ncid0) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(ncid0); return ++nerrs;
    }
    /* in define mode, add a dimension, variable, and attribute */
    if ((ll_dim = ncdimdef(ncid0, ll.name, ll.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(ncid0);
	return ++nerrs;
    }
    add_dim(&test, &ll);

    dd.dims = (int *) emalloc(sizeof(int) * dd.ndims);
    dd.dims[0] = ll_dim;
    if ((dd_id=ncvardef(ncid0, dd.name, dd.type, dd.ndims, dd.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(ncid0);
	return ++nerrs;
    }
    add_var(&test, &dd);

    if (ncattput(ncid0, dd_id, dd_fill_val.name, dd_fill_val.type,
		  dd_fill_val.len, (void *) dd_fill_val.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(ncid0);
	return ++nerrs;
    }
    add_att(&test, dd_id, &dd_fill_val);

    if (ncsync (ncid0) != -1) {
	error("%s: ncsync in define mode should fail", pname);
	nerrs++;
    }

    if (ncendef (ncid0) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(ncid0); return ++nerrs;
    }
    /* in data mode */
    if (ncsync (ncid0) == -1) {
	error("%s: ncsync in data mode failed", pname);
	nerrs++;
    }

    /* put some data into a variable */
    {
	static long dd_start[] = {0};
	static long dd_edges[] = {2};
	static short dd_vals[] = {1, 2};
	short got_vals[2];

	if (ncvarput(ncid0,dd_id,dd_start,dd_edges,(void *)dd_vals) == -1) {
	    error("%s: ncvarput failed", pname);
	    ncclose(ncid0);
	    return ++nerrs;
	}
	add_data(&test,dd_id,dd_start,dd_edges); /* keep test in sync */
	if (ncsync (ncid0) == -1) {
	    error("%s: ncsync after putting data failed", pname);
	    nerrs++;
	}
	if ((ncid1 = ncopen(path, NC_NOWRITE)) == -1) {
#ifndef vms
	    error("%s: second ncopen failed", pname);
	    nerrs++;
#else
	    fprintf(stderr,"Doesn't support shared access on vms\n") ;
#endif
	} else {
		if (ncid0 == ncid1) {
		    error("%s: second ncopen should return distinct handle",
			  pname);
		    nerrs++;
		}	/* read data just put after a sync, should succeed */
		if (ncvarget(ncid1,dd_id,dd_start,dd_edges,(void *)got_vals)
		    == -1) {
		    error("%s: ncvarget failed", pname);
		    nerrs++;
		}
		if (dd_vals[0] != got_vals[0] || dd_vals[1] != got_vals[1]) {
		    error("%s: ncvarget succeeded but data values wrong",
			  pname);
		}
   		if (ncclose (ncid1) == -1) {
		    error("%s: ncclose failed", pname);
		    nerrs++;
		}
	}
    }
    if (ncclose (ncid0) == -1) {
	error("%s: ncclose failed", pname);
	nerrs++;
    }
    if (ncsync (ncid0) != -1) { /* should fail, since ncid0 is bad handle */
	error("%s: ncsync failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    /* Free resources. */
    free(dd.dims);

    return nerrs;
}


/* 
 * Test ncabort
 *    try in define mode, check that file was deleted
 *    try after writing variable
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
int
test_ncabort(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncabort";
    static char fpath[] = "ufo.nc";
    static short attv[] = {3};
    static struct cdfatt att = /* attribute */
      {___, "temp", NC_SHORT, LEN_OF(attv), (void *) attv};
    int ncid;			/* netcdf id */

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }
    /* opened */
    if (ncredef(ncid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* in define mode, add a new global attribute */
    if (ncattput(ncid, NC_GLOBAL, att.name, att.type, att.len, att.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(ncid); return ++nerrs;
    }

    /* abort in define mode, should restore to state before define mode */
    if (ncabort(ncid) == -1) {
	error("%s: ncabort in define mode failed", pname);
	ncclose(ncid); return ++nerrs;
    }
    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen after ncabort failed", pname);
	return ++nerrs;
    }
    /* check that new global attribute was not added */
    if (ncattinq(ncid, NC_GLOBAL, att.name, &att.type, &att.len) != -1) {
	error("%s: ncabort should have restored state before ncredef", pname);
	ncclose(ncid); return ++nerrs;
    }
    /* in data mode not being created, should just close */
    if (ncabort(ncid) == -1) {
	error("%s: ncabort in define mode failed", pname);
	return ++nerrs;
    }
    if ((ncid = nccreate(fpath, NC_CLOBBER)) == -1) {
	error("%s: nccreate failed to NC_CLOBBER", pname);
	return ++nerrs;
    }
    /* in define mode being created, should delete */
    if (ncabort(ncid) == -1) {
	error("%s: ncabort after nccreate failed", pname);
	return ++nerrs;
    }
    /* check with ncopen that file doesn't exist */
    if (ncopen(fpath, NC_NOWRITE) != -1) {
	error("%s: ncabort deleted file, but ncopen found it", pname);
	return ++nerrs;
    }
    if (ncabort(ncid) != -1) {	/* should fail, ncid is bad handle */
	error("%s: ncclose failed to report bad netcdf handle", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}
