/*********************************************************************
 *   Copyright 1996, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: test_read.c,v 1.32 2008/04/04 20:23:51 dmh Exp $
 *********************************************************************/

#include "tests.h"

/* 
 * Test nc_strerror.
 *    Try on a bad error status.
 *    Test for each defined error status.
 */
void
test_nc_strerror(void)
{
    int i;
    const char *message;

    static const struct {
	int status;
	const char *msg;
    } ncerrs[] = {
	{NC_NOERR, "No error"},
	{NC_EBADID, "NetCDF: Not a valid ID"},
	{NC_ENFILE, "NetCDF: Too many files open"},
	{NC_EEXIST, "NetCDF: File exists && NC_NOCLOBBER"},
	{NC_EINVAL, "NetCDF: Invalid argument"},
	{NC_EPERM, "NetCDF: Write to read only"},
	{NC_ENOTINDEFINE, "NetCDF: Operation not allowed in data mode"},
	{NC_EINDEFINE, "NetCDF: Operation not allowed in define mode"},
	{NC_EINVALCOORDS, "NetCDF: Index exceeds dimension bound"},
	{NC_EMAXDIMS, "NetCDF: NC_MAX_DIMS exceeded"},
	{NC_ENAMEINUSE, "NetCDF: String match to name in use"},
	{NC_ENOTATT, "NetCDF: Attribute not found"},
	{NC_EMAXATTS, "NetCDF: NC_MAX_ATTRS exceeded"},
	{NC_EBADTYPE, "NetCDF: Not a valid data type or _FillValue type mismatch"},
	{NC_EBADDIM, "NetCDF: Invalid dimension ID or name"},
	{NC_EUNLIMPOS, "NetCDF: NC_UNLIMITED in the wrong index"},
	{NC_EMAXVARS, "NetCDF: NC_MAX_VARS exceeded"},
	{NC_ENOTVAR, "NetCDF: Variable not found"},
	{NC_EGLOBAL, "NetCDF: Action prohibited on NC_GLOBAL varid"},
	{NC_ENOTNC, "NetCDF: Unknown file format"},
	{NC_ESTS, "NetCDF: In Fortran, string too short"},
	{NC_EMAXNAME, "NetCDF: NC_MAX_NAME exceeded"},
	{NC_EUNLIMIT, "NetCDF: NC_UNLIMITED size already in use"},
	{NC_ENORECVARS, "NetCDF: nc_rec op when there are no record vars"},
	{NC_ECHAR, "NetCDF: Attempt to convert between text & numbers"},
	{NC_EEDGE, "NetCDF: Start+count exceeds dimension bound"},
	{NC_ESTRIDE, "NetCDF: Illegal stride"},
	{NC_EBADNAME, "NetCDF: Name contains illegal characters"},
	{NC_ERANGE, "NetCDF: Numeric conversion not representable"},
	{NC_ENOMEM, "NetCDF: Memory allocation (malloc) failure"},
	{NC_EVARSIZE, "NetCDF: One or more variable sizes violate format constraints"},
	{NC_EDIMSIZE, "NetCDF: Invalid dimension size"}
    };

    /* Try on a bad error status */
    /* Dmh: allow trailing extra info */
    message = nc_strerror(-666);/* should fail */
    IF (strcmp(message, "Unknown Error") != 0)
	error("nc_strerror on bad error status returned: %s", message);

    /* Try on each legitimate error status */
    /* Dmh: allow trailing extra info */
    for (i=0; i<LEN_OF(ncerrs); i++) {
	const char *message = nc_strerror(ncerrs[i].status);
	IF (strcmp(message, ncerrs[i].msg) != 0)
	    error("nc_strerror(%d) should return `%s', not `%s'",
		  ncerrs[i].status, ncerrs[i].msg, message);
    }
}


/* 
 * Test nc_open.
 * If in read-only section of tests,
 *    Try to open a non-existent netCDF file, check error return.
 *    Open a file that is not a netCDF file, check error return.
 *    Open a netCDF file with a bad mode argument, check error return.
 *    Open a netCDF file with NC_NOWRITE mode, try to write, check error.
 *    Try to open a netcdf twice, check whether returned netcdf ids different.
 * If in writable section of tests,
 *    Open a netCDF file with NC_WRITE mode, write something, close it.
 * On exit, any open netCDF files are closed.
 */
void
test_nc_open(void)
{
    int err;
    int ncid;
    int ncid2;
    
    /* Try to open a nonexistent file */
    err = nc_open("tooth-fairy.nc", NC_NOWRITE, &ncid);/* should fail */
    IF (err == NC_NOERR)
	error("nc_open of nonexistent file should have failed");
#ifndef USE_PARALLEL
    IF (! NC_ISSYSERR(err))
	error("nc_open of nonexistent file should have returned system error");
#endif

    /* Open a file that is not a netCDF file.  But need a portable
     * test that also works for cross-compiles ... */
    /* err = nc_open("nc_test.o", NC_NOWRITE, &ncid);/\* should fail *\/ */
    /* IF (err != NC_ENOTNC) */
    /* 	error("nc_open of non-netCDF file: status = %d", err); */

    /* Open a netCDF file in read-only mode, check that write fails */
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    err = nc_redef(ncid);	/* should fail */
    IF (err != NC_EPERM)
	error("nc_redef of read-only file should fail");
    /* Opened OK, see if can open again and get a different netCDF ID */
    err = nc_open(testfile, NC_NOWRITE, &ncid2);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    else {
	(void) nc_close(ncid2);
    }
    IF (ncid2 == ncid)
	error("netCDF IDs for first and second nc_open calls should differ");

    err = nc_create(scratch, NC_NOCLOBBER, &ncid2);
    IF (err) 
       error("nc_create: %s", nc_strerror(err));
    else 
       (void) nc_close(ncid2);
    err = nc_open(scratch, NC_WRITE, &ncid2);
    IF (err) 
       error("nc_open: %s", nc_strerror(err));
    else 
       (void) nc_close(ncid2);
    err = remove(scratch);
    IF (err) 
       error("remove of %s failed", scratch);

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


/* 
 * Test nc_close.
 *    Try to close a netCDF file twice, check whether second close fails.
 *    Try on bad handle, check error return.
 *    Try in define mode and data mode.
 */
void
test_nc_close(void)
{
    int ncid;
    int err = nc_open(testfile, NC_NOWRITE, &ncid);

    IF (err)
	error("nc_open: %s", nc_strerror(err));

    /* Close a netCDF file twice, second time should fail */
    err = nc_close(ncid);
    IF (err)
	error("nc_close failed: %s", nc_strerror(err));
    err = nc_close(ncid);
    IF (err != NC_EBADID)
	error("nc_close of closed file should have failed");
    
    /* Try with a bad netCDF ID */
    err = nc_close(BAD_ID);/* should fail */
    IF (err != NC_EBADID)
	error("nc_close with bad netCDF ID returned wrong error (%d)", err);

    /* Close in data mode */
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    err = nc_close(ncid);
    IF (err)
	error("nc_close in data mode failed: %s", nc_strerror(err));

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) 
       error("nc_create: %s", nc_strerror(err));
    err = nc_close(ncid);
    IF (err)
       error("nc_close in define mode: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
       error("remove of %s failed", scratch);
}


/* 
 * Test nc_inq.
 *    Try on bad handle, check error return.
 *    Try in data mode, check returned values.
 *    Try asking for subsets of info.
 * If in writable section of tests,
 *    Try in define mode, after adding an unlimited dimension, variable.
 * On exit, any open netCDF files are closed.
 */
void
test_nc_inq(void)
{
    int ncid;
    int ndims;			/* number of dimensions */
    int nvars;			/* number of variables */
    int ngatts;			/* number of global attributes */
    int recdim;			/* id of unlimited dimension */
    int err = nc_open(testfile, NC_NOWRITE, &ncid);

    IF (err)
	error("nc_open: %s", nc_strerror(err));
    
    /* Try on bad handle */
    err = nc_inq(BAD_ID, 0, 0, 0, 0);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);
    
    err = nc_inq(ncid, &ndims, &nvars, &ngatts, &recdim);
    IF (err)
	error("nc_inq: %s", nc_strerror(err));
    else IF (ndims != NDIMS)
	error("nc_inq: wrong number of dimensions returned, %d", ndims);
    else IF (nvars != NVARS)
	error("nc_inq: wrong number of variables returned, %d", nvars);
    else IF (ngatts != NGATTS)
	error("nc_inq: wrong number of global atts returned, %d", ngatts);
    else IF (recdim != RECDIM)
	error("nc_inq: wrong record dimension ID returned, %d", recdim);
    
    /* Inguire for no info (useless, but should still work) */
    err = nc_inq(ncid, 0, 0, 0, 0);
    IF (err)
	error("nc_inq for no info failed: %s", nc_strerror(err));

    /* Inguire for subsets of info */
    ngatts = NGATTS - 1;	/* wipe out previous correct value */
    err = nc_inq(ncid, 0, 0, &ngatts, 0);
    IF (err)
	error("nc_inq for one item failed: %s", nc_strerror(err));
    else IF (ngatts != NGATTS)
	error("nc_inq subset: wrong number of global atts returned, %d", ngatts);
    ndims = NDIMS - 1;
    nvars = NVARS - 1;
    err = nc_inq(ncid, &ndims, &nvars, 0, 0);
    IF (err)
	error("nc_inq for two items failed: %s", nc_strerror(err));
    else IF (ndims != NDIMS)
	error("nc_inq subset: wrong number of dimensions returned, %d", ndims);
    else IF (nvars != NVARS)
	error("nc_inq subset: wrong number of variables returned, %d", nvars);

    {		/* tests using netCDF scratch file */
	int ncid2;		/* for scratch netCDF dataset */

        err = nc_create(scratch, NC_NOCLOBBER, &ncid2);
        IF (err) {
            error("nc_create: %s", nc_strerror(err));
	} else {		/* add dim, var, gatt, check inq */
	    int ndims0;
	    int nvars0;
	    int ngatts0;
	    int recdim0;
	    err = nc_enddef(ncid2); /* enter data mode */
	    IF (err)
		error("nc_enddef: %s", nc_strerror(err));
	    err = nc_inq(ncid2, &ndims0, &nvars0, &ngatts0, &recdim0);
	    IF (err)
		error("nc_inq: %s", nc_strerror(err));
	    err = nc_redef(ncid2); /* enter define mode */
	    IF (err)
		error("nc_redef: %s", nc_strerror(err));
	    /* Check that inquire still works in define mode */
	    err = nc_inq(ncid2, &ndims, &nvars, &ngatts, &recdim);
	    IF (err)
		error("nc_inq in define mode: %s", nc_strerror(err));
	    else IF (ndims != ndims0)
		error("nc_inq in define mode: ndims wrong, %d", ndims);
	    else IF (nvars != nvars0)
		error("nc_inq in define mode: nvars wrong, %d", nvars);
	    else IF (ngatts != ngatts0)
		error("nc_inq in define mode: ngatts wrong, %d", ngatts);
	    else IF (recdim != recdim0)
		error("nc_inq in define mode: recdim wrong, %d", recdim);

	    {
		int did, vid;
		/* Add dim, var, global att */
		err = nc_def_dim(ncid2, "inqd", 1L, &did);
		IF (err)
		    error("nc_def_dim: %s", nc_strerror(err));
		err = nc_def_var(ncid2, "inqv", NC_FLOAT, 0, 0, &vid);
		IF (err)
		    error("nc_def_var: %s", nc_strerror(err));
	    }
	    err = nc_put_att_text(ncid2, NC_GLOBAL, "inqa", 1+strlen("stuff"),
				   "stuff");
	    IF (err)
		error("nc_put_att_text: %s", nc_strerror(err));

	    /* Make sure nc_inq sees the additions while in define mode */
	    err = nc_inq(ncid2, &ndims, &nvars, &ngatts, &recdim);
	    IF (err)
		error("nc_inq in define mode: %s", nc_strerror(err));
	    else IF (ndims != ndims0 + 1)
		error("nc_inq in define mode: ndims wrong, %d", ndims);
	    else IF (nvars != nvars0 + 1)
		error("nc_inq in define mode: nvars wrong, %d", nvars);
	    else IF (ngatts != ngatts0 + 1)
		error("nc_inq in define mode: ngatts wrong, %d", ngatts);
	    err = nc_enddef(ncid2);
	    IF (err)
		error("nc_enddef: %s", nc_strerror(err));

	    /* Make sure nc_inq stills sees additions in data mode */
	    err = nc_inq(ncid2, &ndims, &nvars, &ngatts, &recdim);
	    IF (err)
		error("nc_inq failed in data mode: %s", nc_strerror(err));
	    else IF (ndims != ndims0 + 1)
		error("nc_inq in define mode: ndims wrong, %d", ndims);
	    else IF (nvars != nvars0 + 1)
		error("nc_inq in define mode: nvars wrong, %d", nvars);
	    else IF (ngatts != ngatts0 + 1)
		error("nc_inq in define mode: ngatts wrong, %d", ngatts);
	    (void) nc_close(ncid2);
	    err = remove(scratch);
	    IF (err)
		error("remove of %s failed", scratch);
	}
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_natts(void)
{
    int ncid;
    int ngatts;			/* number of global attributes */
    int err;

    err = nc_inq_natts(BAD_ID, &ngatts);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    err = nc_inq_natts(ncid, &ngatts);
    IF (err)
	error("nc_inq_natts: %s", nc_strerror(err));
    else IF (ngatts != NGATTS)
	error("nc_inq_natts: wrong number of global atts returned, %d", ngatts);
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_ndims(void)
{
    int ncid;
    int ndims;
    int err;

    err = nc_inq_ndims(BAD_ID, &ndims);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    err = nc_inq_ndims(ncid, &ndims);
    IF (err)
	error("nc_inq_ndims: %s", nc_strerror(err));
    else IF (ndims != NDIMS)
	error("nc_inq_ndims: wrong number returned, %d", ndims);
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_nvars(void)
{
    int ncid;
    int nvars;
    int err;

    err = nc_inq_nvars(BAD_ID, &nvars);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    err = nc_inq_nvars(ncid, &nvars);
    IF (err)
	error("nc_inq_nvars: %s", nc_strerror(err));
    else IF (nvars != NVARS)
	error("nc_inq_nvars: wrong number returned, %d", nvars);
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_unlimdim(void)
{
    int ncid;
    int unlimdim;
    int err;

    err = nc_inq_unlimdim(BAD_ID, &unlimdim);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    err = nc_inq_unlimdim(ncid, &unlimdim);
    IF (err)
	error("nc_inq_unlimdim: %s", nc_strerror(err));
    else IF (unlimdim != RECDIM)
	error("nc_inq_unlimdim: wrong number returned, %d", unlimdim);
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_dimid(void)
{
    int ncid;
    int dimid;
    int i;
    int err;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    err = nc_inq_dimid(ncid, "noSuch", &dimid);
    IF (err != NC_EBADDIM)
	error("bad dim name: status = %d", err);
    for (i = 0; i < NDIMS; i++) {
	err = nc_inq_dimid(BAD_ID, dim_name[i], &dimid);
	IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_dimid(ncid, dim_name[i], &dimid);
	IF (err)
	    error("nc_inq_dimid: %s", nc_strerror(err));
	else IF (dimid != i)
	    error("expected %d, got %d", i, dimid);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_dim(void)
{
    int ncid;
    int i;
    int err;
    char name[NC_MAX_NAME];
    size_t length;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NDIMS; i++) {
	err = nc_inq_dim(BAD_ID, i, name, &length);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_dim(ncid, BAD_DIMID, name, &length);
        IF (err != NC_EBADDIM)
	    error("bad dimid: status = %d", err);
	err = nc_inq_dim(ncid, i, 0, 0);
	IF (err)
	    error("nc_inq_dim: %s", nc_strerror(err));
	err = nc_inq_dim(ncid, i, name, &length);
	IF (err)
	    error("nc_inq_dim: %s", nc_strerror(err));
	else IF (strcmp(dim_name[i],name)) 
	    error("name expected: %s, got: %s",dim_name[i],name);
	else IF (dim_len[i] != length)
	    error("size expected: %d, got: %d",dim_len[i],length);
	err = nc_inq_dim(ncid, i, name, 0);
        IF (err)
	    error("nc_inq_dim: %s", nc_strerror(err));
	else IF (strcmp(dim_name[i],name)) 
	    error("name expected: %s, got: %s",dim_name[i],name);
	err = nc_inq_dim(ncid, i, 0, &length);
        IF (err)
	    error("nc_inq_dim: %s", nc_strerror(err));
	else IF (dim_len[i] != length)
	    error("size expected: %d, got: %d",dim_len[i],length);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_dimlen(void)
{
    int ncid;
    int i;
    int err;
    size_t length;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NDIMS; i++) {
	err = nc_inq_dimlen(BAD_ID, i, &length);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_dimlen(ncid, BAD_DIMID, &length);
        IF (err != NC_EBADDIM)
	    error("bad dimid: status = %d", err);
	err = nc_inq_dimlen(ncid, i, &length);
	IF (err)
	    error("nc_inq_dimlen: %s", nc_strerror(err));
	else IF (dim_len[i] != length)
	    error("size expected: %d, got: %d",dim_len[i],length);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_dimname(void)
{
    int ncid;
    int i;
    int err;
    char name[NC_MAX_NAME];

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NDIMS; i++) {
	err = nc_inq_dimname(BAD_ID, i, name);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_dimname(ncid, BAD_DIMID, name);
        IF (err != NC_EBADDIM)
	    error("bad dimid: status = %d", err);
	err = nc_inq_dimname(ncid, i, name);
	IF (err)
	    error("nc_inq_dimname: %s", nc_strerror(err));
	else IF (strcmp(dim_name[i],name)) 
	    error("name expected: %s, got: %s",dim_name[i],name);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_varid(void)
{
    int ncid;
    int varid;
    int i;
    int err;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));

    err = nc_inq_varid(ncid, "noSuch", &varid);
    IF (err != NC_ENOTVAR)
	error("bad ncid: status = %d", err);

    for (i = 0; i < NVARS; i++) {
	err = nc_inq_varid(BAD_ID, var_name[i], &varid);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_varid(ncid, var_name[i], &varid);
        IF (err)
	    error("nc_inq_varid: %s", nc_strerror(err));
	else IF (varid != i)
	    error("expected %d, got %d", i, varid);
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_var(void)
{
    int ncid;
    int i;
    int err;
    char name[NC_MAX_NAME];
    nc_type datatype;
    int ndims;
    int dimids[MAX_RANK];
    int natts;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	err = nc_inq_var(BAD_ID, i, name, &datatype, &ndims, dimids, &natts);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_var(ncid,BAD_VARID,name,&datatype,&ndims,dimids,&natts);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	err = nc_inq_var(ncid, i, 0, 0, 0, 0, 0);
	IF (err)
	    error("nc_inq_var: %s", nc_strerror(err));
	err = nc_inq_var(ncid, i, name, &datatype, &ndims, dimids, &natts);
	IF (err)
	    error("nc_inq_var: %s", nc_strerror(err));
	else IF (strcmp(var_name[i],name)) 
	    error("name expected: %s, got: %s",var_name[i],name);
	else IF (var_type[i] != datatype)
	    error("type expected: %d, got: %d",var_type[i],datatype);
	else IF (var_rank[i] != ndims)
	    error("ndims expected: %d, got: %d",var_rank[i],ndims);
	else IF (!int_vec_eq(var_dimid[i],dimids,ndims))
	    error("unexpected dimid");
	else IF (var_natts[i] != natts)
	    error("natts expected: %d, got: %d",var_natts[i],natts);
	err = nc_inq_var(ncid, i, name, 0, 0, 0, 0);
        IF (err)
	    error("nc_inq_var: %s", nc_strerror(err));
	else IF (strcmp(var_name[i],name)) 
	    error("name expected: %s, got: %s",var_name[i],name);
	err = nc_inq_var(ncid, i, 0, &datatype, 0, 0, 0);
        IF (err)
	    error("nc_inq_var: %s", nc_strerror(err));
        else IF (var_type[i] != datatype)
            error("type expected: %d, got: %d",var_type[i],datatype);
	err = nc_inq_var(ncid, i, 0, 0, &ndims, 0, 0);
        IF (err)
	    error("nc_inq_var: %s", nc_strerror(err));
        else IF (var_rank[i] != ndims)
            error("ndims expected: %d, got: %d",var_rank[i],ndims);
	err = nc_inq_var(ncid, i, 0, 0, 0, dimids, 0);
        IF (err)
	    error("nc_inq_var: %s", nc_strerror(err));
        else IF (!int_vec_eq(var_dimid[i],dimids,ndims))
            error("unexpected dimid");
	err = nc_inq_var(ncid, i, 0, 0, 0, 0, &natts);
        IF (err)
	    error("nc_inq_var: %s", nc_strerror(err));
        else IF (var_natts[i] != natts)
            error("natts expected: %d, got: %d",var_natts[i],natts);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_vardimid(void)
{
    int ncid;
    int i;
    int err;
    int dimids[MAX_RANK];

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	err = nc_inq_vardimid(BAD_ID, i, dimids);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_vardimid(ncid, BAD_VARID, dimids);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	err = nc_inq_vardimid(ncid, i, dimids);
	IF (err)
	    error("nc_inq_vardimid: %s", nc_strerror(err));
	else IF (!int_vec_eq(var_dimid[i], dimids, var_rank[i]))
	    error("unexpected dimid");
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_varname(void)
{
    int ncid;
    int i;
    int err;
    char name[NC_MAX_NAME];

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	err = nc_inq_varname(BAD_ID, i, name);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_varname(ncid, BAD_VARID, name);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	err = nc_inq_varname(ncid, i, name);
	IF (err)
	    error("nc_inq_varname: %s", nc_strerror(err));
	else IF (strcmp(var_name[i],name)) 
	    error("name expected: %s, got: %s",var_name[i],name);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_varnatts(void)
{
    int ncid;
    int i;
    int err;
    int natts;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = -1; i < NVARS; i++) {
	err = nc_inq_varnatts(BAD_ID, i, &natts);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_varnatts(ncid, BAD_VARID, &natts);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	err = nc_inq_varnatts(ncid, VARID(i), &natts);
	IF (err)
	    error("nc_inq_varnatts: %s", nc_strerror(err));
        else IF (NATTS(i) != natts)
            error("natts expected: %d, got: %d",NATTS(i),natts);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_varndims(void)
{
    int ncid;
    int i;
    int err;
    int ndims;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	err = nc_inq_varndims(BAD_ID, i, &ndims);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_varndims(ncid, BAD_VARID, &ndims);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	err = nc_inq_varndims(ncid, i, &ndims);
	IF (err)
	    error("nc_inq_varndims: %s", nc_strerror(err));
        else IF (var_rank[i] != ndims)
            error("ndims expected: %d, got: %d",var_rank[i],ndims);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_vartype(void)
{
    int ncid;
    int i;
    int err;
    nc_type datatype;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	err = nc_inq_vartype(BAD_ID, i, &datatype);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
	err = nc_inq_vartype(ncid, BAD_VARID, &datatype);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	err = nc_inq_vartype(ncid, i, &datatype);
	IF (err)
	    error("nc_inq_vartype: %s", nc_strerror(err));
        else IF (var_type[i] != datatype)
            error("type expected: %d, got: %d", var_type[i], datatype);
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


/*
 * Test nc_put_var1
 */
void
test_nc_get_var1(void)
{
    int ncid;
    int i;
    int j;
    int err;
    size_t index[MAX_RANK];
    double expect;
    int nok = 0;		/* count of valid comparisons */
    double buf[1];		/* (void *) buffer */
    double value;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	for (j = 0; j < var_rank[i]; j++)
	    index[j] = 0;
        err = nc_get_var1(BAD_ID, i, index, buf);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
        err = nc_get_var1(ncid, BAD_VARID, index, buf);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	for (j = 0; j < var_rank[i]; j++) {
	    index[j] = var_shape[i][j];
	    err = nc_get_var1(ncid, i, index, buf);
            IF (err != NC_EINVALCOORDS)
                error("bad index: status = %d", err);
	    index[j] = 0;
	}
        for (j = 0; j < var_nels[i]; j++) {
	    err = toMixedBase(j, var_rank[i], var_shape[i], index);
	    IF (err)
		error("error in toMixedBase 2");
	    expect = hash( var_type[i], var_rank[i], index );
            if (var_rank[i] == 0 && i%2 )
		err = nc_get_var1(ncid, i, NULL, buf);
	    else
		err = nc_get_var1(ncid, i, index, buf);
	    IF (err)
		error("%s", nc_strerror(err));
	    err = nc2dbl( var_type[i], buf, &value );
	    IF (err)
		error("error in nc2dbl");
	    if (inRange(expect,var_type[i])) {
		IF (!equal(value,expect,var_type[i],NCT_DOUBLE)) {
		    error("expected: %G, got: %G", expect, value);
		} else {
		    nok++;
		}
	    }
        }
    }
    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
    print_nok(nok);
}

/*
 * Test nc_get_vara
 * Choose a random point dividing each dim into 2 parts
 * Get 2^rank (nslabs) slabs so defined
 * Each get overwrites buffer, so check after each get.
 */
void
test_nc_get_vara(void)
{
    int ncid;
    int d;
    int i;
    int j;
    int k;
    int err;
    int nels;
    int nslabs;
    int nok = 0;      /* count of valid comparisons */
    size_t start[MAX_RANK];
    size_t edge[MAX_RANK];
    size_t index[MAX_RANK];
    size_t mid[MAX_RANK];
    double buf[MAX_NELS];	/* (void *) buffer */
    char *p;			/* (void *) pointer */
    double expect;
    double got;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
        assert(var_rank[i] <= MAX_RANK);
        assert(var_nels[i] <= MAX_NELS);
        for (j = 0; j < var_rank[i]; j++) {
            start[j] = 0;
            edge[j] = 1;
        }
        err = nc_get_vara(BAD_ID, i, start, edge, buf);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
        err = nc_get_vara(ncid, BAD_VARID, start, edge, buf);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	for (j = 0; j < var_rank[i]; j++) {
	    start[j] = var_shape[i][j];
	    err = nc_get_vara(ncid, i, start, edge, buf);
            IF (err != NC_EINVALCOORDS)
                error("bad index: status = %d", err);
	    start[j] = 0;
	    edge[j] = var_shape[i][j] + 1;
	    err = nc_get_vara(ncid, i, start, edge, buf);
            IF (err != NC_EEDGE)
		error("bad edge: status = %d", err);
	    edge[j] = 1;
	}
            /* Choose a random point dividing each dim into 2 parts */
            /* get 2^rank (nslabs) slabs so defined */
        nslabs = 1;
        for (j = 0; j < var_rank[i]; j++) {
            mid[j] = roll( var_shape[i][j] );
            nslabs *= 2;
        }
            /* bits of k determine whether to get lower or upper part of dim */
        for (k = 0; k < nslabs; k++) {
            nels = 1;
            for (j = 0; j < var_rank[i]; j++) {
                if ((k >> j) & 1) {
                    start[j] = 0;
                    edge[j] = mid[j];
                }else{
                    start[j] = mid[j];
                    edge[j] = var_shape[i][j] - mid[j];
                }
                nels *= edge[j];
            }
            if (var_rank[i] == 0 && i%2 )
		err = nc_get_vara(ncid, i, NULL, NULL, buf);
	    else
		err = nc_get_vara(ncid, i, start, edge, buf);
	    IF (err) {
		error("%s", nc_strerror(err));
	    } else {
		for (j = 0; j < nels; j++) {
                    p = (char *) buf;
                    p += j * nctypelen(var_type[i]);
		    err = nc2dbl( var_type[i], p, & got );
		    IF (err)
			error("error in nc2dbl");
		    err = toMixedBase(j, var_rank[i], edge, index);
		    IF (err)
			error("error in toMixedBase 1");
		    for (d = 0; d < var_rank[i]; d++)
			index[d] += start[d];
		    expect = hash(var_type[i], var_rank[i], index);
		    if (inRange(expect,var_type[i])) {
			IF (!equal(got,expect,var_type[i],NCT_DOUBLE)) {
			    error("value read not that expected");
			    if (verbose) {
				error("\n");
				error("varid: %d, ", i);
				error("var_name: %s, ", var_name[i]);
				error("element number: %d ", j);
				error("expect: %g", expect);
				error("got: %g", got);
			    }
			} else {
			    nok++;
			}
		    }
		}
	    }
	}
    }
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    print_nok(nok);
}


/*
 * Test nc_get_vars
 * Choose a random point dividing each dim into 2 parts
 * Get 2^rank (nslabs) slabs so defined
 * Each get overwrites buffer, so check after each get.
 */
void
test_nc_get_vars(void)
{
    int ncid;
    int d;
    int i;
    int j;
    int k;
    int m;
    int err;
    int nels;
    int nslabs;
    int nstarts;	/* number of different starts */
    int nok = 0;	/* total count of valid comparisons */
    int n;		/* count of valid comparisons within var */
    size_t start[MAX_RANK];
    size_t edge[MAX_RANK];
    size_t index[MAX_RANK];
    size_t index2[MAX_RANK];
    size_t mid[MAX_RANK];
    size_t count[MAX_RANK];
    size_t sstride[MAX_RANK];
    ptrdiff_t stride[MAX_RANK];
    double buf[MAX_NELS];     /* (void *) buffer */
    char *p;			/* (void *) pointer */
    double expect;
    double got;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
        assert(var_rank[i] <= MAX_RANK);
        assert(var_nels[i] <= MAX_NELS);
        for (j = 0; j < var_rank[i]; j++) {
            start[j] = 0;
            edge[j] = 1;
            stride[j] = 1;
        }
        err = nc_get_vars(BAD_ID, i, start, edge, stride, buf);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
        err = nc_get_vars(ncid, BAD_VARID, start, edge, stride, buf);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	for (j = 0; j < var_rank[i]; j++) {
	    start[j] = var_shape[i][j];
	    err = nc_get_vars(ncid, i, start, edge, stride, buf);
            IF (err != NC_EINVALCOORDS)
                error("bad index: status = %d", err);
	    start[j] = 0;
	    edge[j] = var_shape[i][j] + 1;
	    err = nc_get_vars(ncid, i, start, edge, stride, buf);
            IF (err != NC_EEDGE)
		error("bad edge: status = %d", err);
	    edge[j] = 1;
	    stride[j] = 0;
	    err = nc_get_vars(ncid, i, start, edge, stride, buf);
            IF (err != NC_ESTRIDE)
		error("bad stride: status = %d", err);
	    stride[j] = 1;
	}
            /* Choose a random point dividing each dim into 2 parts */
            /* get 2^rank (nslabs) slabs so defined */
        nslabs = 1;
        for (j = 0; j < var_rank[i]; j++) {
            mid[j] = roll( var_shape[i][j] );
            nslabs *= 2;
        }
            /* bits of k determine whether to get lower or upper part of dim */
	    /* choose random stride from 1 to edge */
	n = 0;
        for (k = 0; k < nslabs; k++) {
            nstarts = 1;
            for (j = 0; j < var_rank[i]; j++) {
                if ((k >> j) & 1) {
                    start[j] = 0;
                    edge[j] = mid[j];
                }else{
                    start[j] = mid[j];
                    edge[j] = var_shape[i][j] - mid[j];
                }
		sstride[j] = stride[j] = edge[j] > 0 ? 1+roll(edge[j]) : 1;
                nstarts *= stride[j];
            }
	    for (m = 0; m < nstarts; m++) {
		err = toMixedBase(m, var_rank[i], sstride, index);
		IF (err)
		    error("error in toMixedBase");
		nels = 1;
		for (j = 0; j < var_rank[i]; j++) {
		    count[j] = 1 + (edge[j] - index[j] - 1) / stride[j];
		    nels *= count[j];
		    index[j] += start[j];
		}
			/* Random choice of forward or backward */
/* TODO
		if ( roll(2) ) {
		    for (j = 0; j < var_rank[i]; j++) {
			index[j] += (count[j] - 1) * stride[j];
			stride[j] = -stride[j];
		    }
		}
 */
		if (var_rank[i] == 0 && i%2 )
		    err = nc_get_vars(ncid, i, NULL, NULL, NULL, buf);
		else
		    err = nc_get_vars(ncid, i, index, count, stride, buf);
		IF (err) {
		    error("%s", nc_strerror(err));
		} else {
		    for (j = 0; j < nels; j++) {
			p = (char *) buf;
			p += j * nctypelen(var_type[i]);
			err = nc2dbl( var_type[i], p, & got );
			IF (err)
			    error("error in nc2dbl");
			err = toMixedBase(j, var_rank[i], count, index2);
			IF (err)
			    error("error in toMixedBase 1");
			for (d = 0; d < var_rank[i]; d++)
			    index2[d] = index[d] + index2[d] * stride[d];
			expect = hash(var_type[i], var_rank[i], index2);
			if (inRange(expect,var_type[i])) {
			    IF (!equal(got,expect,var_type[i],NCT_DOUBLE)) {
				error("value read not that expected");
				if (verbose) {
				    error("\n");
				    error("varid: %d, ", i);
				    error("var_name: %s, ", var_name[i]);
				    error("element number: %d ", j);
				    error("expect: %g, ", expect);
				    error("got: %g ", got);
				}
			    } else {
				nok++;
			    }
			}
			n++;
		    }
		}
	    }
	}
	IF (n != var_nels[i]) {
	    error("count != nels");
	    if (verbose) {
		error("\n");
		error("varid: %d, ", i);
		error("var_name: %s, ", var_name[i]);
		error("count: %d, ", n);
		error("nels: %d ", var_nels[i]);
	    }
	}
    }
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    print_nok(nok);
}


/*
 * Test nc_get_varm
 * Choose a random point dividing each dim into 2 parts
 * Get 2^rank (nslabs) slabs so defined
 * Choose random stride from 1 to edge
 * Buffer should end up being bit image of external variable.
 * So all gets for a variable store in different elements of buffer
 */
void
test_nc_get_varm(void)
{
    int ncid;
    int i;
    int j;
    int k;
    int m;
    int err;
    int nslabs;
    int nstarts;	/* number of different starts */
    int nok = 0;	/* total count of valid comparisons */
    size_t start[MAX_RANK];
    size_t edge[MAX_RANK];
    size_t index[MAX_RANK];
    size_t mid[MAX_RANK];
    size_t count[MAX_RANK];
    size_t sstride[MAX_RANK];
    ptrdiff_t stride[MAX_RANK];
    ptrdiff_t imap[MAX_RANK];
    ptrdiff_t imap2[MAX_RANK];
    double buf[MAX_NELS];	/* (void *) buffer */
    char *p;			/* (void *) pointer */
    double expect;
    double got;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
	error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
        assert(var_rank[i] <= MAX_RANK);
        assert(var_nels[i] <= MAX_NELS);
        for (j = 0; j < var_rank[i]; j++) {
            start[j] = 0;
            edge[j] = 1;
            stride[j] = 1;
        }
        if (var_rank[i] > 0) {
            j = var_rank[i] - 1;
            imap[j] = nctypelen(var_type[i]);
            for (; j > 0; j--)
                imap[j-1] = imap[j] * var_shape[i][j];
        }
        err = nc_get_varm(BAD_ID, i, start, edge, stride, imap, buf);
        IF (err != NC_EBADID)
	    error("bad ncid: status = %d", err);
        err = nc_get_varm(ncid, BAD_VARID, start, edge, stride, imap, buf);
        IF (err != NC_ENOTVAR)
	    error("bad var id: status = %d", err);
	for (j = 0; j < var_rank[i]; j++) {
	    start[j] = var_shape[i][j];
	    err = nc_get_varm(ncid, i, start, edge, stride, imap, buf);
            IF (err != NC_EINVALCOORDS)
                error("bad index: status = %d", err);
	    start[j] = 0;
	    edge[j] = var_shape[i][j] + 1;
	    err = nc_get_varm(ncid, i, start, edge, stride, imap, buf);
            IF (err != NC_EEDGE)
		error("bad edge: status = %d", err);
	    edge[j] = 1;
	    stride[j] = 0;
	    err = nc_get_varm(ncid, i, start, edge, stride, imap, buf);
            IF (err != NC_ESTRIDE)
		error("bad stride: status = %d", err);
	    stride[j] = 1;
	}
            /* Choose a random point dividing each dim into 2 parts */
            /* get 2^rank (nslabs) slabs so defined */
        nslabs = 1;
        for (j = 0; j < var_rank[i]; j++) {
            mid[j] = roll( var_shape[i][j] );
            nslabs *= 2;
        }
            /* bits of k determine whether to get lower or upper part of dim */
	    /* choose random stride from 1 to edge */
        for (k = 0; k < nslabs; k++) {
            nstarts = 1;
            for (j = 0; j < var_rank[i]; j++) {
                if ((k >> j) & 1) {
                    start[j] = 0;
                    edge[j] = mid[j];
                }else{
                    start[j] = mid[j];
                    edge[j] = var_shape[i][j] - mid[j];
                }
		sstride[j] = stride[j] = edge[j] > 0 ? 1+roll(edge[j]) : 1;
		imap2[j] = imap[j] * sstride[j];
                nstarts *= stride[j];
            }
	    for (m = 0; m < nstarts; m++) {
		if (var_rank[i] == 0 && i%2 ) {
		    err = nc_get_varm(ncid, i, NULL, NULL, NULL, NULL, buf);
		} else {
		    err = toMixedBase(m, var_rank[i], sstride, index);
		    IF (err)
			error("error in toMixedBase");
		    for (j = 0; j < var_rank[i]; j++) {
			count[j] = 1 + (edge[j] - index[j] - 1) / stride[j];
			index[j] += start[j];
		    }
			    /* Random choice of forward or backward */
/* TODO
		    if ( roll(2) ) {
			for (j = 0; j < var_rank[i]; j++) {
			    index[j] += (count[j] - 1) * stride[j];
			    stride[j] = -stride[j];
			}
		    }
 */
		    j = fromMixedBase(var_rank[i], index, var_shape[i]);
		    p = (char *) buf + j * nctypelen(var_type[i]);
		    err = nc_get_varm(ncid, i, index, count, stride, imap2, p);
		}
		IF (err)
		    error("%s", nc_strerror(err));
	    }
	}
        p = (char *) buf;
	for (j = 0; j < var_nels[i]; j++) {
            err = toMixedBase(j, var_rank[i], var_shape[i], index);
            IF (err)
                error("error in toMixedBase");
            expect = hash( var_type[i], var_rank[i], index);
	    err = nc2dbl( var_type[i], p, & got );
	    IF (err)
		error("error in nc2dbl");
	    if (inRange(expect,var_type[i])) {
		IF (!equal(got,expect,var_type[i],NCT_DOUBLE)) {
		    error("value read not that expected");
		    if (verbose) {
			error("\n");
			error("varid: %d, ", i);
			error("var_name: %s, ", var_name[i]);
			error("element number: %d ", j);
			error("expect: %g, ", expect);
			error("got: %g ", got);
		    }
		} else {
		    nok++;
		}
	    }
            p += nctypelen(var_type[i]);
	}
    }
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    print_nok(nok);
}


void
test_nc_get_att(void)
{
    int ncid;
    int i;
    int j;
    size_t k;
    int err;
    double buf[MAX_NELS];	/* (void *) buffer */
    char *p;			/* (void *) pointer */
    double expect;
    double got;
    int nok = 0;      /* count of valid comparisons */

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err) 
	error("nc_open: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
        for (j = 0; j < NATTS(i); j++) {
	    err = nc_get_att(BAD_ID, i, ATT_NAME(i,j), buf);
	    IF (err != NC_EBADID) 
		error("bad ncid: status = %d", err);
	    err = nc_get_att(ncid, BAD_VARID, ATT_NAME(i,j), buf);
	    IF (err != NC_ENOTVAR) 
		error("bad var id: status = %d", err);
	    err = nc_get_att(ncid, i, "noSuch", buf);
	    IF (err != NC_ENOTATT) 
		error("Bad attribute name: status = %d", err);
	    err = nc_get_att(ncid, i, ATT_NAME(i,j), buf);
	    IF (err) {
		error("%s", nc_strerror(err));
	    } else {
		for (k = 0; k < ATT_LEN(i,j); k++) {
		    expect = hash(ATT_TYPE(i,j), -1, &k );
		    p = (char *) buf;
		    p += k * nctypelen(ATT_TYPE(i,j));
		    err = nc2dbl( ATT_TYPE(i,j), p, &got );
		    IF (err)
			error("error in nc2dbl");
		    if (inRange(expect,ATT_TYPE(i,j))) {
			IF (!equal(got,expect,ATT_TYPE(i,j),NCT_DOUBLE)) {
			    error("value read not that expected");
			    if (verbose) {
				error("\n");
				error("varid: %d, ", i);
				error("var_name: %s, ",
					i >= 0 ? var_name[i] : "Global");
				error("att_name: %s, ", ATT_NAME(i,j));
				error("element number: %d\n", k);
				error("expect: %-23.16e\n", expect);
				error("   got: %-23.16e", got);
			    }
			} else {
			    nok++;
			}
		    }
                }
	    }
	}
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
    print_nok(nok);
}


void
test_nc_inq_att(void)
{
    int ncid;
    int i;
    int j;
    int err;
    nc_type t;
    size_t n;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err) 
	error("nc_open: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
        for (j = 0; j < NATTS(i); j++) {
	    err = nc_inq_att(BAD_ID, i, ATT_NAME(i,j), &t, &n);
	    IF (err != NC_EBADID) 
		error("bad ncid: status = %d", err);
	    err = nc_inq_att(ncid, BAD_VARID, ATT_NAME(i,j), &t, &n);
	    IF (err != NC_ENOTVAR) 
		error("bad var id: status = %d", err);
	    err = nc_inq_att(ncid, i, "noSuch", &t, &n);
	    IF (err != NC_ENOTATT) 
		error("Bad attribute name: status = %d", err);
	    err = nc_inq_att(ncid, i, ATT_NAME(i,j), &t, &n);
	    IF (err) {
		error("%s", nc_strerror(err));
	    } else {
		IF (t != ATT_TYPE(i,j))
		    error("type not that expected");
		IF (n != ATT_LEN(i,j)) 
		    error("length not that expected");
	    }
	}
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_attlen(void)
{
    int ncid;
    int i;
    int j;
    int err;
    size_t len;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
	err = nc_inq_attlen(ncid, i, "noSuch", &len);
	IF (err != NC_ENOTATT)
	    error("Bad attribute name: status = %d", err);
        for (j = 0; j < NATTS(i); j++) {
            err = nc_inq_attlen(BAD_ID, i, ATT_NAME(i,j), &len);
            IF (err != NC_EBADID)
                error("bad ncid: status = %d", err);
            err = nc_inq_attlen(ncid, BAD_VARID, ATT_NAME(i,j), &len);
            IF (err != NC_ENOTVAR)
                error("bad varid: status = %d", err);
            err = nc_inq_attlen(ncid, i, ATT_NAME(i,j), &len);
            IF (err) {
                error("%s", nc_strerror(err));
            } else {
		IF (len != ATT_LEN(i,j))
		    error("len not that expected");
            }
        }
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_atttype(void)
{
    int ncid;
    int i;
    int j;
    int err;
    nc_type datatype;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
	err = nc_inq_atttype(ncid, i, "noSuch", &datatype);
	IF (err != NC_ENOTATT)
	    error("Bad attribute name: status = %d", err);
        for (j = 0; j < NATTS(i); j++) {
            err = nc_inq_atttype(BAD_ID, i, ATT_NAME(i,j), &datatype);
            IF (err != NC_EBADID)
                error("bad ncid: status = %d", err);
            err = nc_inq_atttype(ncid, BAD_VARID, ATT_NAME(i,j), &datatype);
            IF (err != NC_ENOTVAR)
                error("bad varid: status = %d", err);
            err = nc_inq_atttype(ncid, i, ATT_NAME(i,j), &datatype);
            IF (err) {
                error("%s", nc_strerror(err));
            } else {
		IF (datatype != ATT_TYPE(i,j))
		    error("type not that expected");
            }
        }
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_attname(void)
{
    int ncid;
    int i;
    int j;
    int err;
    char name[NC_MAX_NAME];

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
	err = nc_inq_attname(ncid, i, BAD_ATTNUM, name);
	IF (err != NC_ENOTATT)
	    error("Bad attribute number: status = %d", err);
	err = nc_inq_attname(ncid, i, NATTS(i), name);
	IF (err != NC_ENOTATT)
	    error("Bad attribute number: status = %d", err);
        for (j = 0; j < NATTS(i); j++) {
            err = nc_inq_attname(BAD_ID, i, j, name);
            IF (err != NC_EBADID)
                error("bad ncid: status = %d", err);
            err = nc_inq_attname(ncid, BAD_VARID, j, name);
            IF (err != NC_ENOTVAR)
                error("bad var id: status = %d", err);
            err = nc_inq_attname(ncid, i, j, name);
            IF (err) {
                error("%s", nc_strerror(err));
            } else {
		IF (strcmp(ATT_NAME(i,j), name) != 0)
		    error("name not that expected");
            }
        }
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}


void
test_nc_inq_attid(void)
{
    int ncid;
    int i;
    int j;
    int err;
    int attnum;

    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
	err = nc_inq_attid(ncid, i, "noSuch", &attnum);
	IF (err != NC_ENOTATT)
	    error("Bad attribute name: status = %d", err);
        for (j = 0; j < NATTS(i); j++) {
            err = nc_inq_attid(BAD_ID, i, ATT_NAME(i,j), &attnum);
            IF (err != NC_EBADID)
                error("bad ncid: status = %d", err);
            err = nc_inq_attid(ncid, BAD_VARID, ATT_NAME(i,j), &attnum);
            IF (err != NC_ENOTVAR)
                error("bad varid: status = %d", err);
            err = nc_inq_attid(ncid, i, ATT_NAME(i,j), &attnum);
            IF (err) {
                error("%s", nc_strerror(err));
            } else {
		IF (attnum != j)
		    error("attnum not that expected");
            }
        }
    }

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
}

