/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/rec.c,v 1.11 2006/10/31 16:21:58 ed Exp $
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
 * Returns number of record variables in an open netCDF file, and an array of
 * the record variable ids, if the array parameter is non-null.  Returns -1 on
 * error.
 */
static int
numrecvars(ncid, recvarids)
     int ncid;
     int *recvarids;
{
    int ndims, iv, nvars;
    int nrecvars;
    int recdimid;
    int dimids[MAX_NC_DIMS];

    if (ncinquire(ncid, 0, &nvars, 0, &recdimid) == -1)
      return -1;
    if (recdimid == -1)
      return 0;
    nrecvars = 0;
    for (iv = 0; iv < nvars; iv++) {
	if (ncvarinq(ncid, iv, 0, 0, &ndims, dimids, 0) == -1)
	  return -1;
	if (ndims > 0 && dimids[0] == recdimid) {
	    if (recvarids)
	      recvarids[nrecvars] = iv;
	    nrecvars++;
	}
    }
    return nrecvars;
}


/*
 * Returns record size (in bytes) of the record variable with a specified
 * variable id.  Returns 0 if not a record variable.  Returns -1 on error.
 */
static long
ncrecsize(ncid,vid)
     int ncid;
     int vid;
{
    int recdimid;
    nc_type type;
    int ndims;
    int dimids[MAX_NC_DIMS];
    int id;
    long size;

    if (ncinquire(ncid, 0, 0, 0, &recdimid) == -1)
      return -1;
    if (ncvarinq(ncid, vid, 0, &type, &ndims, dimids, 0) == -1)
      return -1;
    if (ndims == 0 || dimids[0] != recdimid)
      return 0;
    size = nctypelen(type);
    for (id = 1; id < ndims; id++) {
	long len;
	(void) ncdiminq(ncid, dimids[id], 0, &len);
	size *= len;
    }
    return size;
}


/*
 * Retrieves the number of record variables, the record variable ids, and the
 * record size of each record variable.  If any pointer to info to be returned
 * is null, the associated information is not returned.  Returns -1 on error.
 * This is the same as the ncrecinq() in the library, except that can handle
 * errors better.
 */
static int
recinq(ncid, nrecvars, recvarids, recsizes)
     int ncid;
     int *nrecvars;
     int *recvarids;
     long *recsizes;
{
    int iv;
    int rvarids[MAX_NC_VARS];
    int nrvars = numrecvars(ncid, rvarids);

    if (nrvars == -1)
      return -1;

    if (nrecvars)
      *nrecvars = nrvars;
    if (recvarids)
      for (iv = 0; iv < nrvars; iv++)
	recvarids[iv] = rvarids[iv];
    if (recsizes)
      for (iv = 0; iv < nrvars; iv++)
	recsizes[iv] = ncrecsize(ncid, rvarids[iv]);
    return 0;
}


/*
 * Test ncrecinq
 *    try in both data and define modes
 *    check returned values against independently computed values
 *    try with bad netCDF handle, check error
 */
int
test_ncrecinq(path)
     const char *path;		/* name of netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncrecinq";
    int ncid;
    int nrvars;			/* number of record variables */
    int rvarids[MAX_NC_VARS];	/* id of each record variable */
    long rvarsizes[MAX_NC_VARS]; /* record size of each record variable */
    int tnrvars;		/* true number of record variables */
    int trvarids[MAX_NC_VARS];	/* true id of each record variable */
    long trvarsizes[MAX_NC_VARS]; /* true rec size of each record variable */
    int iv;

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }

    /* First compute independently what ncrecinq should return */
    if (recinq(ncid, &tnrvars, trvarids, trvarsizes) == -1) {
	error("%s: recinq failed", pname);
	ncclose(ncid);
	return ++nerrs;
    }

    /* check that ncrecinq() returns correct information in data mode */
    if (ncrecinq(ncid, &nrvars, rvarids, rvarsizes) == -1) {
	error("%s: ncrecinq failed", pname);
	ncclose(ncid);
	return ++nerrs;
    }

    if (nrvars != tnrvars) {
	error("ncrecinq returned wrong number of rec vars, %d != %d",
	      nrvars, tnrvars);
	nerrs++;
    }

    for (iv = 0; iv < nrvars; iv++) {
	if (rvarids[iv] != trvarids[iv]) {
	    error("ncrecinq returned wrong record id for var %d",
		  trvarids[iv]);
	    nerrs++;
	}
	if (rvarsizes[iv] != trvarsizes[iv]) {
	    error("ncrecinq returned wrong record size for var %d",
		  trvarids[iv]);
	    nerrs++;
	}
    }

    if (ncredef(ncid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(ncid);
	return ++nerrs;
    }
    /* check that ncrecinq() returns correct information in define mode too */
    if (ncrecinq(ncid, &nrvars, rvarids, rvarsizes) == -1) {
	error("%s: ncrecinq failed in define mode", pname);
	ncclose(ncid);
	return ++nerrs;
    }
    if (nrvars != tnrvars) {
	error("define mode, ncrecinq returned wrong num of rec vars, %d != %d",
	      nrvars, tnrvars);
	nerrs++;
    }
    for (iv = 0; iv < nrvars; iv++) {
	if (rvarids[iv] != trvarids[iv]) {
	    error("define mode, ncrecinq returned wrong record id for var %d",
		  trvarids[iv]);
	    nerrs++;
	}
	if (rvarsizes[iv] != trvarsizes[iv]) {
	    error("define mode, ncrecinq returned wrong rec size for var %d",
		  trvarids[iv]);
	    nerrs++;
	}
    }

    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }

    if (ncrecinq(ncid, &nrvars, rvarids, rvarsizes) != -1) {
	error("%s: ncrecinq failed to report bad handle", pname);
	nerrs++;
    }
    
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
    return nerrs;
}


/*
 * Retrieves the dimension sizes of a variable with a specified variable id in
 * an open netCDF file.  Returns -1 on error.
 */
static int
dimsizes(ncid, varid, sizes)
     int ncid;
     int varid;
     long *sizes;
{
    int ndims;
    int id;
    int dimids[MAX_NC_DIMS];

    if (ncvarinq(ncid, varid, 0, 0, &ndims, dimids, 0) == -1)
      return -1;
    if (ndims == 0 || sizes == 0)
      return 0;
    for (id = 0; id < ndims; id++)
      (void) ncdiminq(ncid, dimids[id], 0, &sizes[id]);
    return 0;
}


/*
 * Write one record's worth of data, except don't write to variables for which
 * the address of the data to be written is NULL.  Return -1 on error.  This is
 * the same as the ncrecput() in the library, except that can handle errors
 * better.
 */
static int
recput(ncid, recnum, datap)
     int ncid;
     long recnum;
     void **datap;
{
    int iv;
    int rvids[MAX_NC_VARS];
    int nrvars = numrecvars(ncid, rvids);
    long start[MAX_NC_DIMS];
    long edges[MAX_NC_DIMS];

    if (nrvars == -1)
      return -1;

    start[0] = recnum;
    for (iv = 1; iv < nrvars; iv++)
	start[iv] = 0;

    for (iv = 0; iv < nrvars; iv++) {
	if (datap[iv] != 0) {
	    (void) dimsizes(ncid, rvids[iv], edges);
	    edges[0] = 1;		/* only 1 record's worth */
	    if (ncvarput(ncid, rvids[iv], start, edges, datap[iv]) == -1)
	      return -1;
	}
    }    
    return 0;
}


/*
 * Read one record's worth of data, except don't read from variables for which
 * the address of the data to be read is null.  Return -1 on error.  This is
 * the same as the ncrecget() in the library, except that can handle errors
 * better.
 */
static int
recget(ncid, recnum, datap)
     int ncid;
     long recnum;
     void **datap;
{
    int iv;
    int rvids[MAX_NC_VARS];
    int nrvars = numrecvars(ncid, rvids);
    long start[MAX_NC_DIMS];
    long edges[MAX_NC_DIMS];

    if (nrvars == -1)
      return -1;

    start[0] = recnum;
    for (iv = 1; iv < nrvars; iv++)
	start[iv] = 0;

    for (iv = 0; iv < nrvars; iv++) {
	if (datap[iv] != 0) {
	    (void) dimsizes(ncid, rvids[iv], edges);
	    edges[0] = 1;		/* only 1 record's worth */
	    if (ncvarget(ncid, rvids[iv], start, edges, datap[iv]) == -1)
	      return -1;
	}
    }    
    return 0;
}


/*
 * Test ncrecput
 *    check that proper call works putting all recoerd variables
 *    try putting only a proper subset of variables
 *    try putting the empty subset of variables
 *    try in define mode, check error
 *    try with bad netCDF handle, check error
 */
int
test_ncrecput(path)
     const char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncrecput";
    int nrvars;			/* number of record variables */
    int rvarids[MAX_NC_VARS];	/* id of each record variable */
    long rvarsizes[MAX_NC_VARS]; /* record size of each record variable */
    int ncid;			/* netcdf id */
    void *datap[MAX_NC_VARS];	/* array of address pointers for rec vars */
    void *datar[MAX_NC_VARS];	/* pointers for comparison data */
    long recnum = 1;		/* we'll write the second record */
    int iv;
    long recsize[MAX_NC_VARS];	/* record size in data elements */
    nc_type vartype[MAX_NC_VARS];
    void *zeros[MAX_NC_VARS];

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }

    if (ncrecinq(ncid, &nrvars, rvarids, rvarsizes) == -1) {
	error("%s: ncrecinq failed", pname);
	ncclose(ncid);
	return ++nerrs;
    }

    /* get a block of data of the right type for each record variable */
    for (iv = 0; iv < nrvars; iv++) {
	datap[iv] = emalloc(rvarsizes[iv]);
	datar[iv] = emalloc(rvarsizes[iv]); /* for comparison values */
	if (ncvarinq(ncid, rvarids[iv], 0, &vartype[iv], 0, 0, 0) == -1) {
	    error("%s: ncvarinq failed", pname);
	    ncclose(ncid);
	    return ++nerrs;
	}
	recsize[iv] = rvarsizes[iv]/nctypelen(vartype[iv]);
	/* Fill data blocks with 0,1,2,3,... */
	val_fill(vartype[iv], recsize[iv], datap[iv]);
	/* Zero out comparison data */
	val_fill_zero(vartype[iv], recsize[iv], datar[iv]);
    }

    /* Zero data in recnum record, before trying to put non-zero data */
    if (recput(ncid, recnum, datar) == -1) {
	error("%s: recput failed", pname);
	ncclose(ncid);
	return ++nerrs;
    }

    /* opened in data mode, try putting a complete record */
    if (ncrecput(ncid, recnum, datap) == -1) {
	error("%s: ncrecput failed on complete record", pname);
	nerrs++;
    }

    /* Check that right values were put */
    if (recget(ncid, recnum, datar) == -1) {
	error("%s: recget failed", pname);
	nerrs++;
    }
    for (iv = 0; iv < nrvars; iv++) {
	if (val_cmp(vartype[iv], recsize[iv], datap[iv], datar[iv]) != 0) {
	    error("%s: bad values written by recput", pname);
	    nerrs++;
	}
	val_fill_zero(vartype[iv], recsize[iv], datap[iv]);
	val_fill_zero(vartype[iv], recsize[iv], datar[iv]);
	zeros[iv] = 0;
    }
    
    if (nrvars > 0) {
	void *datap0 = datap[0];

	/* put a partial record, everything but first record variable */
	datap[0] = 0;
	val_fill(vartype[0], recsize[0], datar[0]);
	if (ncrecput(ncid, recnum, datap) == -1) {
	    error("%s: ncrecput failed on partial record", pname);
	    nerrs++;
	}

	/* Check right values were put, first record variable undisturbed */
	datap[0] = datap0;
	if (recget(ncid, recnum, datap) == -1) {
	    error("%s: recget failed after partial record put", pname);
	    nerrs++;
	}
	for (iv = 0; iv < nrvars; iv++) {
	    if (val_cmp(vartype[iv], recsize[iv], datap[iv], datar[iv]) != 0) {
		error("%s: bad values written by partial recput", pname);
		nerrs++;
	    }
	}
    }

    /* Put an empty record, check that values remain undisturbed */
    if (ncrecput(ncid, recnum, zeros) == -1) {
	error("%s: ncrecput failed on empty record", pname);
	nerrs++;
    }
    if (recget(ncid, recnum, datap) == -1) {
	error("%s: recget failed after empty record put", pname);
	nerrs++;
    }
    for (iv = 0; iv < nrvars; iv++) {
	if (val_cmp(vartype[iv], recsize[iv], datap[iv], datar[iv]) != 0) {
	    error("%s: bad values written by empty recput", pname);
	    nerrs++;
	}
    }

    /* try in define mode, check error */
    if (ncredef(ncid) == -1) {
	error("%s: ncredef failed", pname);
 	ncclose(ncid);
	return ++nerrs;
    }

    if (ncrecput(ncid, recnum, datap) != -1) {
	error("%s: ncrecput should fail in define mode", pname);
	nerrs++;
    }

    /* try with bad netCDF handle, check error */
    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    if (ncrecput(ncid, recnum, datap) != -1) {
	error("%s: ncrecput failed to report bad handle", pname);
	nerrs++;
    }
    for (iv = 0; iv < nrvars; iv++) {
	free(datap[iv]);
	free(datar[iv]);
    }

    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}


/*
 * Test ncrecget
 *    check that proper call works getting all record variables
 *    try getting only a proper subset of variables
 *    try getting the empty subset of variables
 *    try with bad netCDF handle, check error
 */
int
test_ncrecget(path)
     const char *path;		/* name of netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncrecget";
    int nrvars;			/* number of record variables */
    int rvarids[MAX_NC_VARS];	/* id of each record variable */
    long rvarsizes[MAX_NC_VARS]; /* record size of each record variable */
    int ncid;			/* netcdf id */
    void *datap[MAX_NC_VARS];	/* array of address pointers for rec vars */
    void *datar[MAX_NC_VARS];	/* pointers for comparison data */
    long recnum = 1;		/* we'll write the second record */
    int iv;
    long recsize[MAX_NC_VARS];	/* record size in data elements */
    nc_type vartype[MAX_NC_VARS];
    void *zeros[MAX_NC_VARS];

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((ncid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return ++nerrs;
    }

    if (ncrecinq(ncid, &nrvars, rvarids, rvarsizes) == -1) {
	error("%s: ncrecinq failed", pname);
	ncclose(ncid);
	return ++nerrs;
    }

    /* get a block of data of the right type for each record variable */
    for (iv = 0; iv < nrvars; iv++) {
	datap[iv] = emalloc(rvarsizes[iv]);
	datar[iv] = emalloc(rvarsizes[iv]); /* for comparison values */
	if (ncvarinq(ncid, rvarids[iv], 0, &vartype[iv], 0, 0, 0) == -1) {
	    error("%s: ncvarinq failed", pname);
	    ncclose(ncid);
	    return ++nerrs;
	}
	recsize[iv] = rvarsizes[iv]/nctypelen(vartype[iv]);
	/* Fill data blocks with 0,1,2,3,... */
	val_fill(vartype[iv], recsize[iv], datap[iv]);
	/* Zero out comparison data */
	val_fill_zero(vartype[iv], recsize[iv], datar[iv]);
    }

    if (recput(ncid, recnum, datap) == -1) {
	error("%s: recput failed", pname);
	ncclose(ncid);
	return ++nerrs;
    }

    /* opened in data mode, try getting a complete record */
    if (recget(ncid, recnum, datap) == -1) {
	error("%s: recget failed on complete record", pname);
	nerrs++;
    }
    if (ncrecget(ncid, recnum, datar) == -1) {
	error("%s: ncrecget failed on complete record", pname);
	nerrs++;
    }

    for (iv = 0; iv < nrvars; iv++) {
	if (val_cmp(vartype[iv], recsize[iv], datap[iv], datar[iv]) != 0) {
	    error("%s: bad values written by recget", pname);
	    nerrs++;
	}
	val_fill_zero(vartype[iv], recsize[iv], datap[iv]);
	val_fill_zero(vartype[iv], recsize[iv], datar[iv]);
	zeros[iv] = 0;
    }
    
    if (nrvars > 0) {
	void *datap0 = datap[0];
	void *datar0 = datar[0];

	/* get a partial record, everything but first record variable */
	datap[0] = 0;
	if (ncrecget(ncid, recnum, datap) == -1) {
	    error("%s: ncrecget failed on partial record", pname);
	    nerrs++;
	}
	datar[0] = 0;
	if (recget(ncid, recnum, datar) == -1) {
	    error("%s: recget failed on partial record", pname);
	    nerrs++;
	}
	/* Check right values were got, first record variable undisturbed */
	datap[0] = datap0;
	datar[0] = datar0;
	for (iv = 0; iv < nrvars; iv++) {
	    if (val_cmp(vartype[iv], recsize[iv], datap[iv], datar[iv]) != 0) {
		error("%s: bad values read by partial recget", pname);
		nerrs++;
	    }
	}
    }

    /* Get an empty record */
    if (ncrecget(ncid, recnum, zeros) == -1) {
	error("%s: ncrecget failed on empty record", pname);
	nerrs++;
    }

    /* try with bad netCDF handle, check error */
    if (ncclose (ncid) == -1) {
	error("%s: ncclose failed", pname);
	return ++nerrs;
    }
    if (ncrecget(ncid, recnum, datap) != -1) {
	error("%s: ncrecget failed to report bad handle", pname);
	nerrs++;
    }
    for (iv = 0; iv < nrvars; iv++) {
	free(datap[iv]);
	free(datar[iv]);
    }

    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
    
    return nerrs;
}

