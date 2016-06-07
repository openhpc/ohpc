/*********************************************************************
 *   Copyright 1996, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: test_write.c,v 1.33 2006/10/31 16:23:13 ed Exp $
 *********************************************************************/

#include "tests.h"
#include "math.h"
#include <stdio.h> /* For FILE stuff. */

/*
 * Test nc_create
 *    For mode in NC_NOCLOBBER, NC_CLOBBER do:
 *       create netcdf file 'scratch.nc' with no data, close it
 *       test that it can be opened, do nc_inq to check nvars = 0, etc.
 *    Try again in NC_NOCLOBBER mode, check error return
 * On exit, delete this file
 */
void
test_nc_create(void)
{
    int clobber;		/* 0 for NC_NOCLOBBER, 1 for NC_CLOBBER */
    int err;
    int ncid;
    int ndims;                  /* number of dimensions */
    int nvars;                  /* number of variables */
    int ngatts;                 /* number of global attributes */
    int recdim;                 /* id of unlimited dimension */

    for (clobber = 0; clobber < 2; clobber++) {
	err = nc_create(scratch, clobber ? NC_CLOBBER : NC_NOCLOBBER, &ncid);
	IF (err)
	    error("nc_create: %s", nc_strerror(err));
	err = nc_close(ncid);
	IF (err)
	    error("nc_close: %s", nc_strerror(err));
	err = nc_open(scratch, NC_NOWRITE, &ncid);
	IF (err)
	    error("nc_open: %s", nc_strerror(err));
	err = nc_inq(ncid, &ndims, &nvars, &ngatts, &recdim);
	IF (err)
	    error("nc_inq: %s", nc_strerror(err));
	else IF (ndims != 0)
	    error("nc_inq: wrong number of dimensions returned, %d", ndims);
	else IF (nvars != 0)
	    error("nc_inq: wrong number of variables returned, %d", nvars);
	else IF (ngatts != 0)
	    error("nc_inq: wrong number of global atts returned, %d", ngatts);
	else IF (recdim != -1)
	    error("nc_inq: wrong record dimension ID returned, %d", recdim);
	err = nc_close(ncid);
	IF (err)
	    error("nc_close: %s", nc_strerror(err));
    }

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err != NC_EEXIST)
	error("attempt to overwrite file: status = %d", err);
    err = remove(scratch);
    IF (err)
	error("remove of %s failed", scratch);
}


/*
 * Test nc_redef 
 * (In fact also tests nc_enddef - called from test_nc_enddef)
 *    BAD_ID
 *    attempt redef (error) & enddef on read-only file
 *    create file, define dims & vars. 
 *    attempt put var (error)
 *    attempt redef (error) & enddef.
 *    put vars
 *    attempt def new dims (error)
 *    redef
 *    def new dims, vars.
 *    put atts
 *    enddef
 *    put vars
 *    close
 *    check file: vars & atts
 *    check reopening with NC_WRITE and adding new dims, atts, vars
 */
void
test_nc_redef(void)
{
    int ncid;                   /* netcdf id */
    /* used to force effective test of ncio->move() in redef */
    size_t sizehint = 8192;
    int dimid;         /* dimension id */
    int varid;         /* variable id */
    int varid1;        /* variable id */
    int err;
    const char * title = "Not funny";
    double var;
    char name[NC_MAX_NAME];
    size_t length;
    int fmt_variant1, fmt_variant2;

	/* BAD_ID tests */
    err = nc_redef(BAD_ID);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);
    err = nc_enddef(BAD_ID);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);

	/* read-only tests */
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    err = nc_redef(ncid);
    IF (err != NC_EPERM)
	error("nc_redef in NC_NOWRITE mode: status = %d", err);
    err = nc_enddef(ncid);
    IF (err != NC_ENOTINDEFINE)
	error("nc_redef in NC_NOWRITE mode: status = %d", err);
    err = nc_close(ncid);
    IF (err) 
	error("nc_close: %s", nc_strerror(err));

	/* tests using scratch file */
    err = nc__create(scratch, NC_NOCLOBBER, 0, &sizehint, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    /* limit for ncio implementations which have infinite chunksize */
    if(sizehint > 32768)
	sizehint = 16384;
    def_dims(ncid);
    def_vars(ncid);
    put_atts(ncid);
    err = nc_inq_varid(ncid, "d", &varid);
    IF (err) 
	error("nc_inq_varid: %s", nc_strerror(err));
    var = 1.0;
    err = nc_put_var1_double(ncid, varid, NULL, &var);
    IF (err != NC_EINDEFINE)
        error("nc_put_var... in define mode: status = %d", err);
    err = nc_redef(ncid);
    IF (err != NC_EINDEFINE)
        error("nc_redef in define mode: status = %d", err);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    put_vars(ncid);
    err = nc_def_dim(ncid, "abc", sizehint, &dimid);
    IF (err != NC_ENOTINDEFINE)
        error("nc_def_dim in define mode: status = %d", err);
    err = nc_redef(ncid);
    IF (err)
        error("nc_redef: %s", nc_strerror(err));
#if 0
    err = nc_set_fill(ncid, NC_NOFILL, NULL);
    IF (err)
        error("nc_set_fill: %s", nc_strerror(err));
#endif
    err = nc_def_dim(ncid, "abc", sizehint, &dimid);
    IF (err)
        error("nc_def_dim: %s", nc_strerror(err));
    err = nc_def_var(ncid, "abcScalar", NC_INT, 0, NULL, &varid);
    IF (err)
        error("nc_def_var: %s", nc_strerror(err));
    err = nc_def_var(ncid, "abc", NC_INT, 1, &dimid, &varid1);
    IF (err)
        error("nc_def_var: %s", nc_strerror(err));
    {
	int dimids[NDIMS +1];
	int ii = 0;
	for(ii = 0; ii < NDIMS; ii++)
		dimids[ii] = ii;
	dimids[NDIMS] = dimid;
    	err = nc_def_var(ncid, "abcRec", NC_INT, NDIMS, dimids, &varid1);
    	IF (err)
        	error("nc_def_var: %s", nc_strerror(err));
    }
    err = nc_put_att_text(ncid, NC_GLOBAL, "title", 1+strlen(title), title);
    IF (err)
        error("nc_put_att_text: %s", nc_strerror(err));
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    var = 1.0;
    err = nc_put_var1_double(ncid, varid, NULL, &var);
    IF (err)
        error("nc_put_var1_double: %s", nc_strerror(err));
    err = nc_inq_format(ncid, &fmt_variant1);
    IF (err)
	error("nc_inq_format: %s", nc_strerror(err));
    err = nc_close(ncid);
    IF (err) 
	error("nc_close: %s", nc_strerror(err));

    /* check scratch file written as expected */
    check_file(scratch);  /* checks all except "abc" stuff added above */

    IF ((err = nc_open(scratch, NC_NOWRITE, &ncid)))
        error("nc_open: %s", nc_strerror(err));
    IF ((err = nc_inq_dim(ncid, dimid, name, &length))) 
	error("nc_inq_dim: %s", nc_strerror(err));
    IF (strcmp(name, "abc") != 0) 
	error("Unexpected dim name");
    IF (length != sizehint) 
	error("Unexpected dim length");
    IF ((err = nc_get_var1_double(ncid, varid, NULL, &var)))
        error("nc_get_var1_double: %s", nc_strerror(err));
    IF (var != 1.0)
        error("nc_get_var1_double: unexpected value");
    IF ((err = nc_close(ncid)))
        error("nc_close: %s", nc_strerror(err));

    /* open scratch file for writing, add another dim, var, att, then check */
    IF ((err = nc_open(scratch, NC_WRITE, &ncid)))
        error("nc_open: %s", nc_strerror(err));
    IF ((err = nc_redef(ncid)))
        error("nc_redef: %s", nc_strerror(err));
    IF ((err = nc_def_dim(ncid, "def", sizehint, &dimid)))
        error("nc_def_dim: %s", nc_strerror(err));
    IF ((err = nc_def_var(ncid, "defScalar", NC_INT, 0, NULL, &varid)))
        error("nc_def_var: %s", nc_strerror(err));
    IF ((err = nc_def_var(ncid, "def", NC_INT, 1, &dimid, &varid1)))
        error("nc_def_var: %s", nc_strerror(err));
    IF ((err = nc_put_att_text(ncid, NC_GLOBAL, "Credits", 1+strlen("Thanks!"), "Thanks!")))
        error("nc_put_att_text: %s", nc_strerror(err));
    IF ((err = nc_enddef(ncid)))
        error("nc_enddef: %s", nc_strerror(err));
    var = 2.0;
    IF ((err = nc_put_var1_double(ncid, varid, NULL, &var)))
        error("nc_put_var1_double: %s", nc_strerror(err));
    IF ((err = nc_close(ncid))) 
	error("nc_close: %s", nc_strerror(err));

    /* check scratch file written as expected */
    check_file(scratch);

    err = nc_open(scratch, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    err = nc_inq_dim(ncid, dimid, name, &length);
    IF (err) 
	error("nc_inq_dim: %s", nc_strerror(err));
    IF (strcmp(name, "def") != 0) 
	error("Unexpected dim name");
    IF (length != sizehint) 
	error("Unexpected dim length");
    err = nc_get_var1_double(ncid, varid, NULL, &var);
    IF (err)
        error("nc_get_var1_double: %s", nc_strerror(err));
    IF (var != 2.0)
        error("nc_get_var1_double: unexpected value");
    /* make sure format variant hasn't changed from when created */
    err = nc_inq_format(ncid, &fmt_variant2);
    IF (err)
	error("nc_inq_format: %s", nc_strerror(err));
    IF (fmt_variant1 != fmt_variant2)
	error("nc_enddef changed format variant");
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_enddef 
 * Simply calls test_nc_redef which tests both nc_redef & nc_enddef
 */
void
test_nc_enddef(void)
{
    test_nc_redef();
}


/*
 * Test nc_sync
 *    try with bad handle, check error
 *    try in define mode, check error
 *    try writing with one handle, reading with another on same netCDF
 */
void
test_nc_sync(void)
{
    int ncidw;         /* netcdf id for writing */
    int ncidr;         /* netcdf id for reading */
    int err;

        /* BAD_ID test */
    err = nc_sync(BAD_ID);
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);

        /* create scratch file & try nc_sync in define mode */
    err = nc_create(scratch, NC_NOCLOBBER, &ncidw);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
	return;
    }
    err = nc_sync(ncidw);
    IF (err != NC_EINDEFINE)
        error("nc_sync called in define mode: status = %d", err);

        /* write using same handle */
    def_dims(ncidw);
    def_vars(ncidw);
    put_atts(ncidw);
    err = nc_enddef(ncidw);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    put_vars(ncidw);
    err = nc_sync(ncidw);
    IF (err)
        error("nc_sync of ncidw failed: %s", nc_strerror(err));

        /* open another handle, nc_sync, read (check) */
    err = nc_open(scratch, NC_NOWRITE, &ncidr);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    err = nc_sync(ncidr);
    IF (err)
        error("nc_sync of ncidr failed: %s", nc_strerror(err));
    check_dims(ncidr);
    check_atts(ncidr);
    check_vars(ncidr);

        /* close both handles */
    err = nc_close(ncidr);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = nc_close(ncidw);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_abort
 *    try with bad handle, check error
 *    try in define mode before anything written, check that file was deleted
 *    try after nc_enddef, nc_redef, define new dims, vars, atts
 *    try after writing variable
 */
void
test_nc_abort(void)
{
    int ncid;          /* netcdf id */
    int err;
    int ndims;
    int nvars;
    int ngatts;

        /* BAD_ID test */
    err = nc_abort(BAD_ID);
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);

        /* create scratch file & try nc_abort in define mode */
    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);
    put_atts(ncid);
    err = nc_abort(ncid);
    IF (err)
        error("nc_abort of ncid failed: %s", nc_strerror(err));
    err = nc_close(ncid);	/* should already be closed */
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);
    err = remove(scratch);	/* should already be deleted */
    IF (!err)
        error("file %s should not exist", scratch);

        /* 
         * create scratch file
	 * do nc_enddef & nc_redef
	 * define new dims, vars, atts
	 * try nc_abort: should restore previous state (no dims, vars, atts)
	 */ 
    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    err = nc_redef(ncid);
    IF (err)
        error("nc_redef: %s", nc_strerror(err));
    def_dims(ncid);
    def_vars(ncid);
    put_atts(ncid);
    err = nc_abort(ncid);
    IF (err)
        error("nc_abort of ncid failed: %s", nc_strerror(err));
    err = nc_close(ncid);	/* should already be closed */
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);
    err = nc_open(scratch, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    err = nc_inq (ncid, &ndims, &nvars, &ngatts, NULL);
    IF (err)
        error("nc_inq: %s", nc_strerror(err));
    IF (ndims != 0)
        error("ndims should be 0");
    IF (nvars != 0)
        error("nvars should be 0");
    IF (ngatts != 0)
        error("ngatts should be 0");
    err = nc_close (ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

        /* try nc_abort in data mode - should just close */
    err = nc_create(scratch, NC_CLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);
    put_atts(ncid);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    put_vars(ncid);
    err = nc_abort(ncid);
    IF (err)
        error("nc_abort of ncid failed: %s", nc_strerror(err));
    err = nc_close(ncid);       /* should already be closed */
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);
    check_file(scratch);
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_def_dim
 *    try with bad netCDF handle, check error
 *    try in data mode, check error
 *    check that returned id is one more than previous id
 *    try adding same dimension twice, check error
 *    try with illegal sizes, check error
 *    make sure unlimited size works, shows up in nc_inq_unlimdim
 *    try to define a second unlimited dimension, check error
 */
void
test_nc_def_dim(void)
{
    int ncid;
    int  err;             /* status */
    int  i;
    int  dimid;         /* dimension id */
    size_t length;

        /* BAD_ID test */
    err = nc_def_dim(BAD_ID, "abc", 8, &dimid);
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);

        /* data mode test */
    err = nc_create(scratch, NC_CLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    err = nc_def_dim(ncid, "abc", 8, &dimid);
    IF (err != NC_ENOTINDEFINE)
        error("bad ncid: status = %d", err);

        /* define-mode tests: unlimited dim */
    err = nc_redef(ncid);
    IF (err)
        error("nc_redef: %s", nc_strerror(err));
    err = nc_def_dim(ncid, dim_name[0], NC_UNLIMITED, &dimid);
    IF (err) 
	error("nc_def_dim: %s", nc_strerror(err));
    IF (dimid != 0) 
	error("Unexpected dimid");
    err = nc_inq_unlimdim(ncid, &dimid);
    IF (err) 
	error("nc_inq_unlimdim: %s", nc_strerror(err));
    IF (dimid != 0) 
	error("Unexpected recdim");
    err = nc_inq_dimlen(ncid, dimid, &length);
    IF (err)
	error("nc_inq_dimlen: %s", nc_strerror(err));
    IF (length != 0) 
	error("Unexpected length");
    err = nc_def_dim(ncid, "abc", NC_UNLIMITED, &dimid);
    IF (err != NC_EUNLIMIT)
        error("2nd unlimited dimension: status = %d", err);
    /* define-mode tests: remaining dims */
    for (i = 1; i < NDIMS; i++) {
        err = nc_def_dim(ncid, dim_name[i-1], dim_len[i], &dimid);
	IF (err != NC_ENAMEINUSE)
	    error("duplicate name: status = %d", err);
	err = nc_def_dim(ncid, BAD_NAME, dim_len[i], &dimid);
	IF (err != NC_EBADNAME)
	    error("bad name: status = %d", err);
	/* Fix: dmh 11/4/2011: works only if sizeof(long) > 4 */
	if(sizeof(long) > 4) {
            err = nc_def_dim(ncid, dim_name[i], NC_UNLIMITED-1, &dimid);
    	    IF (err != NC_EDIMSIZE)
	        error("bad size: status = %d", err);
	}
        err = nc_def_dim(ncid, dim_name[i], dim_len[i], &dimid);
        IF (err) 
	    error("nc_def_dim: %s", nc_strerror(err));
	IF (dimid != i) 
	    error("Unexpected dimid");
    }

        /* Following just to expand unlimited dim */
    def_vars(ncid);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    put_vars(ncid);

        /* Check all dims */
    check_dims(ncid);

    err = nc_close(ncid);
    IF (err)
	error("nc_close: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_rename_dim
 *    try with bad netCDF handle, check error
 *    check that proper rename worked with nc_inq_dim
 *    try renaming to existing dimension name, check error
 *    try with bad dimension handle, check error
 */
void
test_nc_rename_dim(void)
{
    int ncid;
    int  err;             /* status */
    char name[NC_MAX_NAME];

        /* BAD_ID test */
    err = nc_rename_dim(BAD_ID, 0, "abc");
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);

        /* main tests */
    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    err = nc_rename_dim(ncid, BAD_DIMID, "abc");
    IF (err != NC_EBADDIM)
        error("bad dimid: status = %d", err);
    err = nc_rename_dim(ncid, 2, "abc");
    IF (err)
        error("nc_rename_dim: %s", nc_strerror(err));
    err = nc_inq_dimname(ncid, 2, name);
    IF (err)
        error("nc_inq_dimname: %s", nc_strerror(err));
    IF (strcmp(name, "abc") != 0)
        error("Unexpected name: %s", name);
    err = nc_rename_dim(ncid, 0, "abc");
    IF (err != NC_ENAMEINUSE)
        error("duplicate name: status = %d", err);

    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_def_var
 *    try with bad netCDF handle, check error
 *    try with bad name, check error
 *    scalar tests:
 *      check that proper define worked with nc_inq_var
 *      try redefining an existing variable, check error
 *      try with bad datatype, check error
 *      try with bad number of dimensions, check error
 *      try in data mode, check error
 *    check that returned id is one more than previous id
 *    try with bad dimension ids, check error
 */
void
test_nc_def_var(void)
{
    int  ncid;
    int  varid;
    int  err;             /* status */
    int  i;
    int  ndims;
    int  natts;
    char name[NC_MAX_NAME];
    int dimids[MAX_RANK];
    nc_type datatype;

        /* BAD_ID test */
    err = nc_def_var(BAD_ID, "abc", NC_SHORT, 0, NULL, &varid);
    IF (err != NC_EBADID)
        error("bad ncid: status = %d", err);

        /* scalar tests */
    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    err = nc_def_var(ncid, "abc", NC_SHORT, 0, NULL, &varid);
    IF (err)
        error("nc_def_var: %s", nc_strerror(err));
    err = nc_inq_var(ncid, varid, name, &datatype, &ndims, dimids, &natts);
    IF (err)
        error("nc_inq_var: %s", nc_strerror(err));
    IF (strcmp(name, "abc") != 0)
        error("Unexpected name: %s", name);
    IF (datatype != NC_SHORT)
        error("Unexpected datatype");
    IF (ndims != 0)
        error("Unexpected rank");
    err = nc_def_var(ncid, BAD_NAME, NC_SHORT, 0, NULL, &varid);
    IF (err != NC_EBADNAME)
        error("bad name: status = %d", err);
    err = nc_def_var(ncid, "abc", NC_SHORT, 0, NULL, &varid);
    IF (err != NC_ENAMEINUSE)
        error("duplicate name: status = %d", err);
    err = nc_def_var(ncid, "ABC", BAD_TYPE, -1, dimids, &varid);
    IF (err != NC_EBADTYPE)
        error("bad type: status = %d", err);
    err = nc_def_var(ncid, "ABC", NC_SHORT, -1, dimids, &varid);
    IF (err != NC_EINVAL)
        error("bad rank: status = %d", err);
    err = nc_enddef(ncid);
    IF (err)
	error("nc_enddef: %s", nc_strerror(err));
    err = nc_def_var(ncid, "ABC", NC_SHORT, 0, dimids, &varid);
    IF (err != NC_ENOTINDEFINE)
        error("nc_def_var called in data mode: status = %d", err);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);

        /* general tests using global vars */
    err = nc_create(scratch, NC_CLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    for (i = 0; i < NVARS; i++) {
        err = nc_def_var(ncid, var_name[i], var_type[i], var_rank[i],
            var_dimid[i], &varid);
        IF (err) 
	    error("nc_def_var: %s", nc_strerror(err));
	IF (varid != i)
	    error("Unexpected varid");
    }

        /* try bad dim ids */
    dimids[0] = BAD_DIMID;
    err = nc_def_var(ncid, "abc", NC_SHORT, 1, dimids, &varid);
    IF (err != NC_EBADDIM)
        error("bad dim ids: status = %d", err);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_put_var1
 */
void
test_nc_put_var1(void)
{
    int ncid;
    int i;
    int j;
    int err;
    size_t index[MAX_RANK];
    double value;
    double buf[1];		/* (void *) buffer */

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));

    for (i = 0; i < NVARS; i++) {
        for (j = 0; j < var_rank[i]; j++)
            index[j] = 0;
        err = nc_put_var1(BAD_ID, i, index, buf);
        IF (err != NC_EBADID)
            error("bad ncid: status = %d", err);
        err = nc_put_var1(ncid, BAD_VARID, index, buf);
        IF (err != NC_ENOTVAR)
            error("bad var id: status = %d", err);
        for (j = 0; j < var_rank[i]; j++) {
            if (var_dimid[i][j] > 0) {          /* skip record dim */
                index[j] = var_shape[i][j];
                err = nc_put_var1(ncid, i, index, buf);
                IF (err != NC_EINVALCOORDS)
                    error("bad index: status = %d", err);
                index[j] = 0;
            }
        }
        for (j = 0; j < var_nels[i]; j++) {
            err = toMixedBase(j, var_rank[i], var_shape[i], index);
            IF (err)
                error("error in toMixedBase");
            value = hash( var_type[i], var_rank[i], index);
	    if (inRange(value, var_type[i])) {
		err = dbl2nc(value, var_type[i], buf);
		IF (err)
		    error("error in dbl2nc");
		if (var_rank[i] == 0 && i%2 == 0)
		    err = nc_put_var1(ncid, i, NULL, buf);
		else
		    err = nc_put_var1(ncid, i, index, buf);
		IF (err)
		    error("%s", nc_strerror(err));
	    }
        }
    }

    check_vars(ncid);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_put_vara
 * Choose a random point dividing each dim into 2 parts
 * Put 2^rank (nslabs) slabs so defined
 * Redefine buffer for each put.
 * At end check all variables using check_vars
 */
void
test_nc_put_vara(void)
{
    int ncid;
    int d;
    int i;
    int j;
    int k;
    int err;
    int nels;
    int nslabs;
    size_t start[MAX_RANK];
    size_t edge[MAX_RANK];
    size_t index[MAX_RANK];
    size_t mid[MAX_RANK];
    double buf[MAX_NELS]; 	/* (void *) buffer */
    char *p;			/* (void *) pointer */
    double value;

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));

    for (i = 0; i < NVARS; i++) {
        assert(var_rank[i] <= MAX_RANK);
        assert(var_nels[i] <= MAX_NELS);
        for (j = 0; j < var_rank[i]; j++) {
            start[j] = 0;
            edge[j] = 1;
        }
        err = nc_put_vara(BAD_ID, i, start, edge, buf);
        IF (err != NC_EBADID)
            error("bad ncid: status = %d", err);
        err = nc_put_vara(ncid, BAD_VARID, start, edge, buf);
        IF (err != NC_ENOTVAR)
            error("bad var id: status = %d", err);
        for (j = 0; j < var_rank[i]; j++) {
            if (var_dimid[i][j] > 0) {          /* skip record dim */
		start[j] = var_shape[i][j];
		err = nc_put_vara(ncid, i, start, edge, buf);
		IF (err != NC_EINVALCOORDS)
		    error("bad index: status = %d", err);
		start[j] = 0;
		edge[j] = var_shape[i][j] + 1;
		err = nc_put_vara(ncid, i, start, edge, buf);
		IF (err != NC_EEDGE)
		    error("bad edge: status = %d", err);
		edge[j] = 1;
	    }
        }
            /* Choose a random point dividing each dim into 2 parts */
            /* put 2^rank (nslabs) slabs so defined */
        nslabs = 1;
        for (j = 0; j < var_rank[i]; j++) {
            mid[j] = roll( var_shape[i][j] );
            nslabs *= 2;
        }
            /* bits of k determine whether to put lower or upper part of dim */
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
	    p = (char *) buf;
	    for (j = 0; j < nels; j++) {
		err = toMixedBase(j, var_rank[i], edge, index);
		IF (err)
		    error("error in toMixedBase");
		for (d = 0; d < var_rank[i]; d++)
		    index[d] += start[d];
		value = hash( var_type[i], var_rank[i], index);
		if (!inRange(value, var_type[i]))
		    value = 0;
		err = dbl2nc(value, var_type[i], p);
		IF (err)
		    error("error in dbl2nc");
		p += nctypelen(var_type[i]);
            }
            if (var_rank[i] == 0 && i%2 == 0)
		err = nc_put_vara(ncid, i, NULL, NULL, buf);
            else
		err = nc_put_vara(ncid, i, start, edge, buf);
            IF (err) {
                error("%s", nc_strerror(err));
            }
        }
    }

    check_vars(ncid);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_put_vars
 * Choose a random point dividing each dim into 2 parts
 * Put 2^rank (nslabs) slabs so defined
 * Choose random stride from 1 to edge
 * Redefine buffer for each put.
 * At end check all variables using check_vars
 */
void
test_nc_put_vars(void)
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
    int nstarts;        /* number of different starts */
    size_t start[MAX_RANK];
    size_t edge[MAX_RANK];
    size_t index[MAX_RANK];
    size_t index2[MAX_RANK];
    size_t mid[MAX_RANK];
    size_t count[MAX_RANK];
    size_t sstride[MAX_RANK];
    ptrdiff_t stride[MAX_RANK];
    double buf[MAX_NELS]; /* (void *) buffer */
    char *p;			/* (void *) pointer */
    double value;

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));

    for (i = 0; i < NVARS; i++) {
        assert(var_rank[i] <= MAX_RANK);
        assert(var_nels[i] <= MAX_NELS);
        for (j = 0; j < var_rank[i]; j++) {
            start[j] = 0;
            edge[j] = 1;
            stride[j] = 1;
        }
        err = nc_put_vars(BAD_ID, i, start, edge, stride, buf);
        IF (err != NC_EBADID)
            error("bad ncid: status = %d", err);
        err = nc_put_vars(ncid, BAD_VARID, start, edge, stride, buf);
        IF (err != NC_ENOTVAR)
            error("bad var id: status = %d", err);
        for (j = 0; j < var_rank[i]; j++) {
            if (var_dimid[i][j] > 0) {          /* skip record dim */
		start[j] = var_shape[i][j];
		err = nc_put_vars(ncid, i, start, edge, stride, buf);
		IF (err != NC_EINVALCOORDS && err != NC_EEDGE)
		    error("bad index: status = %d", err);
		start[j] = 0;
		edge[j] = var_shape[i][j] + 1;
		err = nc_put_vars(ncid, i, start, edge, stride, buf);
		IF (err != NC_EEDGE)
		    error("bad edge: status = %d", err);
		edge[j] = 1;
		stride[j] = 0;
		err = nc_put_vars(ncid, i, start, edge, stride, buf);
		IF (err != NC_ESTRIDE)
		    error("bad stride: status = %d", err);
		stride[j] = 1;
	    }
        }
            /* Choose a random point dividing each dim into 2 parts */
            /* put 2^rank (nslabs) slabs so defined */
        nslabs = 1;
        for (j = 0; j < var_rank[i]; j++) {
            mid[j] = roll( var_shape[i][j] );
            nslabs *= 2;
        }
            /* bits of k determine whether to put lower or upper part of dim */
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
		p = (char *) buf;
		for (j = 0; j < nels; j++) {
		    err = toMixedBase(j, var_rank[i], count, index2);
		    IF (err)
			error("error in toMixedBase");
		    for (d = 0; d < var_rank[i]; d++)
			index2[d] = index[d] + index2[d] * stride[d];
		    value = hash( var_type[i], var_rank[i], index2);
		    if (!inRange(value, var_type[i]))
			value = 0;
		    err = dbl2nc(value, var_type[i], p);
		    IF (err)
			error("error in dbl2nc");
		    p += nctypelen(var_type[i]);
		}
		if (var_rank[i] == 0 && i%2 == 0)
		    err = nc_put_vars(ncid, i, NULL, NULL, NULL, buf);
		else
		    err = nc_put_vars(ncid, i, index, count, stride, buf);
		IF (err) {
		    error("%s", nc_strerror(err));
		}
            }
        }
    }

    check_vars(ncid);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_put_varm
 * Choose a random point dividing each dim into 2 parts
 * Put 2^rank (nslabs) slabs so defined
 * Choose random stride from 1 to edge
 * Buffer is bit image of whole external variable.
 * So all puts for a variable put different elements of buffer
 * At end check all variables using check_vars
 */
void
test_nc_put_varm(void)
{
    int ncid;
    int i;
    int j;
    int k;
    int m;
    int err;
    int nslabs;
    int nstarts;        /* number of different starts */
    size_t start[MAX_RANK];
    size_t edge[MAX_RANK];
    size_t index[MAX_RANK];
    size_t mid[MAX_RANK];
    size_t count[MAX_RANK];
    size_t sstride[MAX_RANK];
    ptrdiff_t stride[MAX_RANK];
    ptrdiff_t imap[MAX_RANK];
    ptrdiff_t imap2[MAX_RANK];
    double buf[MAX_NELS];       /* (void *) buffer */
    char *p;			/* (void *) pointer */
    double value;

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));

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
	p = (char *) buf;
	for (j = 0; j < var_nels[i]; j++) {
	    err = toMixedBase(j, var_rank[i], var_shape[i], index);
	    IF (err)
		error("error in toMixedBase");
	    value = hash( var_type[i], var_rank[i], index);
	    if (!inRange(value, var_type[i]))
		value = 0;
	    err = dbl2nc(value, var_type[i], p);
	    IF (err)
		error("error in dbl2nc");
	    p += nctypelen(var_type[i]);
	}
        err = nc_put_varm(BAD_ID, i, start, edge, stride, imap, buf);
        IF (err != NC_EBADID)
            error("bad ncid: status = %d", err);
        err = nc_put_varm(ncid, BAD_VARID, start, edge, stride, imap, buf);
        IF (err != NC_ENOTVAR)
            error("bad var id: status = %d", err);
        for (j = 0; j < var_rank[i]; j++) {
            if (var_dimid[i][j] > 0) {          /* skip record dim */
		start[j] = var_shape[i][j];
		err = nc_put_varm(ncid, i, start, edge, stride, imap, buf);
		IF (err != NC_EINVALCOORDS && err != NC_EEDGE)
		    error("bad index: status = %d", err);
		start[j] = 0;
		edge[j] = var_shape[i][j] + 1;
		err = nc_put_varm(ncid, i, start, edge, stride, imap, buf);
		IF (err != NC_EEDGE)
		    error("bad edge: status = %d", err);
		edge[j] = 1;
		stride[j] = 0;
		err = nc_put_varm(ncid, i, start, edge, stride, imap, buf);
		IF (err != NC_ESTRIDE)
		    error("bad stride: status = %d", err);
		stride[j] = 1;
	    }
        }
            /* Choose a random point dividing each dim into 2 parts */
            /* put 2^rank (nslabs) slabs so defined */
        nslabs = 1;
        for (j = 0; j < var_rank[i]; j++) {
            mid[j] = roll( var_shape[i][j] );
            nslabs *= 2;
        }
            /* bits of k determine whether to put lower or upper part of dim */
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
		if (var_rank[i] == 0 && i%2 == 0) {
		    err = nc_put_varm(ncid, i, NULL, NULL, NULL, NULL, buf);
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
		    err = nc_put_varm(ncid, i, index, count, stride, imap2, p);
		}
		IF (err) {
		    error("%s", nc_strerror(err));
		}
            }
        }
    }

    check_vars(ncid);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_rename_var
 *    try with bad netCDF handle, check error
 *    try with bad variable handle, check error
 *    try renaming to existing variable name, check error
 *    check that proper rename worked with nc_inq_varid
 *    try in data mode, check error
 */
void
test_nc_rename_var(void)
{
    int ncid;
    int varid;
    int err;
    int i;
    char name[NC_MAX_NAME];

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    err = nc_rename_var(ncid, BAD_VARID, "newName");
    IF (err != NC_ENOTVAR)
	error("bad var id: status = %d", err);
    def_dims(ncid);
    def_vars(ncid);

	/* Prefix "new_" to each name */
    for (i = 0; i < NVARS; i++) {
        err = nc_rename_var(BAD_ID, i, "newName");
        IF (err != NC_EBADID)
            error("bad ncid: status = %d", err);
        err = nc_rename_var(ncid, i, var_name[NVARS-1]);
        IF (err != NC_ENAMEINUSE)
            error("duplicate name: status = %d", err);
	(void) strcpy(name, "new_");
	(void) strcat(name, var_name[i]);
        err = nc_rename_var(ncid, i, name);
        IF (err)
	    error("nc_rename_var: %s", nc_strerror(err));
        err = nc_inq_varid(ncid, name, &varid);
        IF (err)
	    error("nc_inq_varid: %s", nc_strerror(err));
        IF (varid != i)
	    error("Unexpected varid");
    }

	/* Change to data mode */
	/* Try making names even longer. Then restore original names */
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	(void) strcpy(name, "even_longer_");
	(void) strcat(name, var_name[i]);
        err = nc_rename_var(ncid, i, name);
        IF (err != NC_ENOTINDEFINE)
            error("longer name in data mode: status = %d", err);
        err = nc_rename_var(ncid, i, var_name[i]);
        IF (err)
	    error("nc_rename_var: %s", nc_strerror(err));
        err = nc_inq_varid(ncid, var_name[i], &varid);
        IF (err)
	    error("nc_inq_varid: %s", nc_strerror(err));
        IF (varid != i)
	    error("Unexpected varid");
    }

    put_vars(ncid);
    check_vars(ncid);

    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


void
test_nc_put_att(void)
{
    int ncid;
    int varid;
    int i;
    int j;
    size_t k;
    int err;
    double buf[MAX_NELS];       /* (void *) buffer */
    char *p;                    /* (void *) pointer */
    char *name;			/* of att */
    nc_type datatype;		/* of att */
    size_t length;		/* of att */
    double value;

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);

    for (i = -1; i < NVARS; i++) {
	varid = VARID(i);
        for (j = 0; j < NATTS(i); j++) {
	    name = ATT_NAME(i,j);
	    datatype = ATT_TYPE(i,j);
	    length = ATT_LEN(i,j);
            err = nc_put_att(BAD_ID, varid, name, datatype, length, buf);
            IF (err != NC_EBADID)
                error("bad ncid: status = %d", err);
            err = nc_put_att(ncid, varid, BAD_NAME, datatype, length, buf);
	    IF (err != NC_EBADNAME)
		error("bad name: status = %d", err);
            err = nc_put_att(ncid, BAD_VARID, name, datatype, length, buf);
            IF (err != NC_ENOTVAR)
                error("bad var id: status = %d", err);
	    err = nc_put_att(ncid, varid, name, BAD_TYPE, length, buf);
	    IF (err != NC_EBADTYPE)
		error("bad type: status = %d", err);
	    p = (char *) buf;
	    for (k = 0; k < length; k++) {
		value = hash(datatype, -1, &k );
		if (!inRange(value, datatype))
		    value = 0;
		err = dbl2nc(value, datatype, p);
		IF (err)
		    error("error in dbl2nc");
		p += nctypelen(datatype);
	    }
            err = nc_put_att(ncid, varid, name, datatype, length, buf);
            IF (err) {
                error("%s", nc_strerror(err));
            }
        }
    }

    check_atts(ncid);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_copy_att
 *    try with bad source or target netCDF handles, check error
 *    try with bad source or target variable handle, check error
 *    try with nonexisting attribute, check error
 *    check that NC_GLOBAL variable for source or target works
 *    check that new attribute put works with target in define mode
 *    check that old attribute put works with target in data mode
 *    check that changing type and length of an attribute work OK
 *    try with same ncid for source and target, different variables
 *    try with same ncid for source and target, same variable
 */
void
test_nc_copy_att(void)
{
    int ncid_in;
    int ncid_out;
    int varid;
    int err;
    int i;
    int j;
    char *name;                 /* of att */
    nc_type datatype;           /* of att */
    size_t length;              /* of att */
    char  value;

    err = nc_open(testfile, NC_NOWRITE, &ncid_in);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    err = nc_create(scratch, NC_NOCLOBBER, &ncid_out);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid_out);
    def_vars(ncid_out);

    for (i = -1; i < NVARS; i++) {
        varid = VARID(i);
        for (j = 0; j < NATTS(i); j++) {
            name = ATT_NAME(i,j);
	    err = nc_copy_att(ncid_in, BAD_VARID, name, ncid_out, varid);
	    IF (err != NC_ENOTVAR)
		error("bad var id: status = %d", err);
	    err = nc_copy_att(ncid_in, varid, name, ncid_out, BAD_VARID);
	    IF (err != NC_ENOTVAR)
		error("bad var id: status = %d", err);
	    err = nc_copy_att(BAD_ID, varid, name, ncid_out, varid);
	    IF (err != NC_EBADID)
		error("bad ncid: status = %d", err);
	    err = nc_copy_att(ncid_in, varid, name, BAD_ID, varid);
	    IF (err != NC_EBADID)
		error("bad ncid: status = %d", err);
	    err = nc_copy_att(ncid_in, varid, "noSuch", ncid_out, varid);
	    IF (err != NC_ENOTATT)
		error("bad attname: status = %d", err);
	    err = nc_copy_att(ncid_in, varid, name, ncid_out, varid);
	    IF (err)
		error("nc_copy_att: %s", nc_strerror(err));
	    err = nc_copy_att(ncid_out, varid, name, ncid_out, varid);
	    IF (err)
		error("source = target: %s", nc_strerror(err));
	}
    }

    err = nc_close(ncid_in);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

        /* Close scratch. Reopen & check attributes */
    err = nc_close(ncid_out);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = nc_open(scratch, NC_WRITE, &ncid_out);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    check_atts(ncid_out);

       /* 
	* change to define mode
	* define single char. global att. ':a' with value 'A'
	* This will be used as source for following copies
	*/
    err = nc_redef(ncid_out);
    IF (err)
        error("nc_redef: %s", nc_strerror(err));
    err = nc_put_att_text(ncid_out, NC_GLOBAL, "a", 1, "A");
    IF (err)
	error("nc_put_att_text: %s", nc_strerror(err));

       /* 
	* change to data mode
	* Use scratch as both source & dest.
	* try copy to existing att. change type & decrease length
	* rename 1st existing att of each var (if any) 'a'
	* if this att. exists them copy ':a' to it
	*/
    err = nc_enddef(ncid_out);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	if (NATTS(i) > 0 && ATT_LEN(i,j) > 0) {
	    err = nc_rename_att(ncid_out, i, att_name[i][0], "a");
	    IF (err)
		error("nc_rename_att: %s", nc_strerror(err));
	    err = nc_copy_att(ncid_out, NC_GLOBAL, "a", ncid_out, i);
	    IF (err)
		error("nc_copy_att: %s", nc_strerror(err));
	}
    }
    err = nc_close(ncid_out);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

	/* Reopen & check */
    err = nc_open(scratch, NC_WRITE, &ncid_out);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    for (i = 0; i < NVARS; i++) {
	if (NATTS(i) > 0 && ATT_LEN(i,j) > 0) {
	    err = nc_inq_att(ncid_out, i, "a", &datatype, &length);
	    IF (err)
		error("nc_inq_att: %s", nc_strerror(err));
	    IF (datatype != NC_CHAR)
		error("Unexpected type");
	    IF (length != 1)
		error("Unexpected length");
	    err = nc_get_att_text(ncid_out, i, "a", &value);
	    IF (err)
		error("nc_get_att_text: %s", nc_strerror(err));
	    IF (value != 'A')
		error("Unexpected value");
	}                                                   
    }                                                   

    err = nc_close(ncid_out);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_rename_att
 *    try with bad netCDF handle, check error
 *    try with bad variable handle, check error
 *    try with nonexisting att name, check error
 *    try renaming to existing att name, check error
 *    check that proper rename worked with nc_inq_attid
 *    try in data mode, check error
 */
void
test_nc_rename_att(void)
{
    int ncid;
    int varid;
    int err;
    int i;
    int j;
    size_t  k;
    int attnum;
    char *attname;
    char name[NC_MAX_NAME];
    char oldname[NC_MAX_NAME];
    char newname[NC_MAX_NAME];
    int nok = 0;      /* count of valid comparisons */
    nc_type datatype;
    nc_type atttype;
    size_t length;
    size_t attlength;
    char  text[MAX_NELS];
    double value[MAX_NELS];
    double expect;

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    err = nc_rename_att(ncid, BAD_VARID, "abc", "newName");
    IF (err != NC_ENOTVAR)
	error("bad var id: status = %d", err);
    def_dims(ncid);
    def_vars(ncid);
    put_atts(ncid);

    for (i = -1; i < NVARS; i++) {
        varid = VARID(i);
        for (j = 0; j < NATTS(i); j++) {
	    attname = ATT_NAME(i,j);
	    err = nc_rename_att(BAD_ID, varid, attname, "newName");
	    IF (err != NC_EBADID)
		error("bad ncid: status = %d", err);
	    err = nc_rename_att(ncid, varid, "noSuch", "newName");
	    IF (err != NC_ENOTATT)
		error("bad attname: status = %d", err);
	    (void) strcpy(newname, "new_");
	    (void) strcat(newname, attname);
	    err = nc_rename_att(ncid, varid, attname, newname);
	    IF (err)
		error("nc_rename_att: %s", nc_strerror(err));
	    err = nc_inq_attid(ncid, varid, newname, &attnum);
	    IF (err)
		error("nc_inq_attid: %s", nc_strerror(err));
	    IF (attnum != j)
		error("Unexpected attnum");
	}
    }

        /* Close. Reopen & check */
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = nc_open(scratch, NC_WRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
        varid = VARID(i);
        for (j = 0; j < NATTS(i); j++) {
	    attname = ATT_NAME(i,j);
	    atttype = ATT_TYPE(i,j);
	    attlength = ATT_LEN(i,j);
            (void) strcpy(newname, "new_");
            (void) strcat(newname, attname);
            err = nc_inq_attname(ncid, varid, j, name);
            IF (err)
                error("nc_inq_attname: %s", nc_strerror(err));
            IF (strcmp(name, newname) != 0)
                error("nc_inq_attname: unexpected name");
            err = nc_inq_att(ncid, varid, name, &datatype, &length);
            IF (err)
                error("nc_inq_att: %s", nc_strerror(err));
            IF (datatype != atttype)
                error("nc_inq_att: unexpected type");
            IF (length != attlength)
                error("nc_inq_att: unexpected length");
            if (datatype == NC_CHAR) {
                err = nc_get_att_text(ncid, varid, name, text);
                IF (err)
                    error("nc_get_att_text: %s", nc_strerror(err));
                for (k = 0; k < attlength; k++) {
                    expect = hash(datatype, -1, &k);
                    IF (text[k] != expect) {
                        error("nc_get_att_text: unexpected value");
                    } else {
                        nok++;
                    }
                }
            } else {
                err = nc_get_att_double(ncid, varid, name, value);
                IF (err)
                    error("nc_get_att_double: %s", nc_strerror(err));
                for (k = 0; k < attlength; k++) {
                    expect = hash(datatype, -1, &k);
		    if (inRange(expect, datatype)) {
			IF (!equal(value[k],expect,datatype,NCT_DOUBLE)) {
			    error("nc_get_att_double: unexpected value");
			} else {
			    nok++;
			}
                    }
                }
            }
        }
    }
    print_nok(nok);

	/* Now in data mode */
	/* Try making names even longer. Then restore original names */

    for (i = -1; i < NVARS; i++) {
        varid = VARID(i);
        for (j = 0; j < NATTS(i); j++) {
	    attname = ATT_NAME(i,j);
	    (void) strcpy(oldname, "new_");
	    (void) strcat(oldname, attname);
	    (void) strcpy(newname, "even_longer_");
	    (void) strcat(newname, attname);
	    err = nc_rename_att(ncid, varid, oldname, newname);
	    IF (err != NC_ENOTINDEFINE)
		error("longer name in data mode: status = %d", err);
	    err = nc_rename_att(ncid, varid, oldname, attname);
	    IF (err)
		error("nc_rename_att: %s", nc_strerror(err));
	    err = nc_inq_attid(ncid, varid, attname, &attnum);
	    IF (err)
		error("nc_inq_attid: %s", nc_strerror(err));
	    IF (attnum != j)
		error("Unexpected attnum");
	}
    }

    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_del_att
 *    try with bad netCDF handle, check error
 *    try with bad variable handle, check error
 *    try with nonexisting att name, check error
 *    check that proper delete worked using:
 *      nc_inq_attid, nc_inq_natts, nc_inq_varnatts
 */
void
test_nc_del_att(void)
{
    int ncid;
    int err;
    int i;
    int j;
    int attnum;
    int natts;
    int numatts;
    int varid;
    char *name;                 /* of att */

    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    err = nc_del_att(ncid, BAD_VARID, "abc");
    IF (err != NC_ENOTVAR)
	error("bad var id: status = %d", err);
    def_dims(ncid);
    def_vars(ncid);
    put_atts(ncid);

    for (i = -1; i < NVARS; i++) {
	varid = VARID(i);
	numatts = NATTS(i);
        for (j = 0; j < numatts; j++) {
	    name = ATT_NAME(i,j);
	    err = nc_del_att(BAD_ID, varid, name);
	    IF (err != NC_EBADID)
		error("bad ncid: status = %d", err);
	    err = nc_del_att(ncid, varid, "noSuch");
	    IF (err != NC_ENOTATT)
		error("bad attname: status = %d", err);
	    err = nc_del_att(ncid, varid, name);
	    IF (err)
		error("nc_del_att: %s", nc_strerror(err));
	    err = nc_inq_attid(ncid, varid, name, &attnum);
	    IF (err != NC_ENOTATT)
		error("bad attname: status = %d", err);
	    if (i < 0) {
		err = nc_inq_natts(ncid, &natts);
		IF (err)
		    error("nc_inq_natts: %s", nc_strerror(err));
		IF (natts != numatts-j-1)
		    error("natts: expected %d, got %d", numatts-j-1, natts);
	    }
	    err = nc_inq_varnatts(ncid, varid, &natts);
	    IF (err)
		error("nc_inq_varnatts: %s", nc_strerror(err));
	    IF (natts != numatts-j-1)
		error("natts: expected %d, got %d", numatts-j-1, natts);
	}
    }

        /* Close. Reopen & check no attributes left */
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = nc_open(scratch, NC_WRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    err = nc_inq_natts(ncid, &natts);
    IF (err)
	error("nc_inq_natts: %s", nc_strerror(err));
    IF (natts != 0)
	error("natts: expected %d, got %d", 0, natts);
    for (i = -1; i < NVARS; i++) {
	varid = VARID(i);
	err = nc_inq_varnatts(ncid, varid, &natts);
	IF (err)
	    error("nc_inq_natts: %s", nc_strerror(err));
	IF (natts != 0)
	    error("natts: expected %d, got %d", 0, natts);
    }

	/* restore attributes. change to data mode. try to delete */
    err = nc_redef(ncid);
    IF (err)
        error("nc_redef: %s", nc_strerror(err));
    put_atts(ncid);
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));

    for (i = -1; i < NVARS; i++) {
	varid = VARID(i);
	numatts = NATTS(i);
        for (j = 0; j < numatts; j++) {
	    name = ATT_NAME(i,j);
	    err = nc_del_att(ncid, varid, name);
	    IF (err != NC_ENOTINDEFINE)
		error("in data mode: status = %d", err);
	}
    }

    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}


/*
 * Test nc_set_fill
 *    try with bad netCDF handle, check error
 *    try in read-only mode, check error
 *    try with bad new_fillmode, check error
 *    try in data mode, check error
 *    check that proper set to NC_FILL works for record & non-record variables
 *    (note that it is not possible to test NC_NOFILL mode!)
 *    close file & create again for test using attribute _FillValue
 */
void
test_nc_set_fill(void)
{
    int ncid;
    int varid;
    int err;
    int i;
    int j;
    int old_fillmode;
    int nok = 0;      /* count of valid comparisons */
    char text = 0;
    double value = 0;
    double fill;
    size_t index[MAX_RANK];

	/* bad ncid */
    err = nc_set_fill(BAD_ID, NC_NOFILL, &old_fillmode);
    IF (err != NC_EBADID)
	error("bad ncid: status = %d", err);

	/* try in read-only mode */
    err = nc_open(testfile, NC_NOWRITE, &ncid);
    IF (err)
        error("nc_open: %s", nc_strerror(err));
    err = nc_set_fill(ncid, NC_NOFILL, &old_fillmode);
    IF (err != NC_EPERM)
	error("read-only: status = %d", err);
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));

	/* create scratch */
    err = nc_create(scratch, NC_NOCLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }

	/* BAD_FILLMODE */
    err = nc_set_fill(ncid, BAD_FILLMODE, &old_fillmode);
    IF (err != NC_EINVAL)
        error("bad fillmode: status = %d", err);

	/* proper calls */
    err = nc_set_fill(ncid, NC_NOFILL, &old_fillmode);
    IF (err)
        error("nc_set_fill: %s", nc_strerror(err));
    IF (old_fillmode != NC_FILL)
        error("Unexpected old fill mode: %d", old_fillmode);
    err = nc_set_fill(ncid, NC_FILL, &old_fillmode);
    IF (err)
        error("nc_set_fill: %s", nc_strerror(err));
    IF (old_fillmode != NC_NOFILL)
        error("Unexpected old fill mode: %d", old_fillmode);

	/* define dims & vars */
    def_dims(ncid);
    def_vars(ncid);

	/* Change to data mode. Set fillmode again */
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    err = nc_set_fill(ncid, NC_FILL, &old_fillmode);
    IF (err)
        error("nc_set_fill: %s", nc_strerror(err));
    IF (old_fillmode != NC_FILL)
        error("Unexpected old fill mode: %d", old_fillmode);

	/* Write record number NRECS to force writing of preceding records */
	/* Assumes variable cr is char vector with UNLIMITED dimension */
    err = nc_inq_varid(ncid, "cr", &varid);
    IF (err)
        error("nc_inq_varid: %s", nc_strerror(err));
    index[0] = NRECS;
    err = nc_put_var1_text(ncid, varid, index, &text);
    IF (err)
        error("nc_put_var1_text: %s", nc_strerror(err));

	/* get all variables & check all values equal default fill */
    for (i = 0; i < NVARS; i++) {
	switch (var_type[i]) {
	    case NC_CHAR:   fill = NC_FILL_CHAR; break;
	    case NC_BYTE:   fill = NC_FILL_BYTE; break;
	    case NC_SHORT:  fill = NC_FILL_SHORT; break;
	    case NC_INT:   fill = NC_FILL_INT; break;
	    case NC_FLOAT:  fill = NC_FILL_FLOAT; break;
	    case NC_DOUBLE: fill = NC_FILL_DOUBLE; break;
	    default: assert(0);
	}
	for (j = 0; j < var_nels[i]; j++) {
            err = toMixedBase(j, var_rank[i], var_shape[i], index);
            IF (err)
                error("error in toMixedBase");
	    if (var_type[i] == NC_CHAR) {
		err = nc_get_var1_text(ncid, i, index, &text);
		IF (err)
		    error("nc_get_var1_text failed: %s", nc_strerror(err));
		value = text;
	    } else {
		err = nc_get_var1_double(ncid, i, index, &value);
		IF (err)
		    error("nc_get_var1_double failed: %s", nc_strerror(err));
	    }
	    IF (value != fill && fabs((fill - value)/fill) > DBL_EPSILON)
		error("\n\t\tValue expected: %-23.17e,\n\t\t          read: %-23.17e\n",
			fill, value);
	    else
		nok++;
        }
    }

	/* close scratch & create again for test using attribute _FillValue */
    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = nc_create(scratch, NC_CLOBBER, &ncid);
    IF (err) {
        error("nc_create: %s", nc_strerror(err));
        return;
    }
    def_dims(ncid);
    def_vars(ncid);

	/* set _FillValue = 42 for all vars */
    text = fill = 42;
    for (i = 0; i < NVARS; i++) {
	if (var_type[i] == NC_CHAR) {
	    err = nc_put_att_text(ncid, i, "_FillValue", 1, &text);
	    IF (err)
		error("nc_put_att_text: %s", nc_strerror(err));
	} else {
	    err = nc_put_att_double(ncid, i, "_FillValue",var_type[i],1,&fill);
	    IF (err)
		error("nc_put_att_double: %s", nc_strerror(err));
	}
    }

	/* data mode. write records */
    err = nc_enddef(ncid);
    IF (err)
        error("nc_enddef: %s", nc_strerror(err));
    index[0] = NRECS;
    err = nc_put_var1_text(ncid, varid, index, &text);
    IF (err)
        error("nc_put_var1_text: %s", nc_strerror(err));

	/* get all variables & check all values equal 42 */
    for (i = 0; i < NVARS; i++) {
	for (j = 0; j < var_nels[i]; j++) {
            err = toMixedBase(j, var_rank[i], var_shape[i], index);
            IF (err)
                error("error in toMixedBase");
	    if (var_type[i] == NC_CHAR) {
		err = nc_get_var1_text(ncid, i, index, &text);
		IF (err)
		    error("nc_get_var1_text failed: %s", nc_strerror(err));
		value = text;
	    } else {
		err = nc_get_var1_double(ncid, i, index, &value);
		IF (err)
		    error("nc_get_var1_double failed: %s", nc_strerror(err));
	    }
	    IF (value != fill)
		error(" Value expected: %g, read: %g\n", fill, value);
	    else
		nok++;
        }
    }
    print_nok(nok);

    err = nc_close(ncid);
    IF (err)
        error("nc_close: %s", nc_strerror(err));
    err = remove(scratch);
    IF (err)
        error("remove of %s failed", scratch);
}

/* This function gets the version of a netCDF file, 1 is for netCDF
   classic, 2 for 64-bit offset format, (someday) 3 for HDF5 format.
*/
#define MAGIC_NUM_LEN 4
static
int
nc_get_file_version(char *path, int *version)
{
   FILE *fp;
   char magic[MAGIC_NUM_LEN];

   /* Need two valid pointers - check for NULL. */
   if (!version || !path)
      return NC_EINVAL;

   /* Figure out if this is a netcdf or hdf5 file. */
   if (!(fp = fopen(path, "r")) ||
       fread(magic, MAGIC_NUM_LEN, 1, fp) != 1) {
     fclose(fp);
     return errno;
   }
   fclose(fp);
   if (strncmp(magic, "CDF", MAGIC_NUM_LEN-1)==0)
   {
      if (magic[MAGIC_NUM_LEN-1] == NC_FORMAT_CLASSIC || 
	  magic[MAGIC_NUM_LEN-1] == NC_FORMAT_64BIT)
	 *version = magic[MAGIC_NUM_LEN-1];
      else
	 return NC_ENOTNC;
   }
   /*   tomorrow, tomorrow, I love you tomorrow, you're always a day
	away! */
   /*if (magic[1] == 'H' && magic[2] == 'D' && magic[3] == 'F')
      *version = 3;*/
   return NC_NOERR;
}

/*
 * Test nc_set_default_format
 *    try with bad default format
 *    try with NULL old_formatp
 *    try in data mode, check error
 *    check that proper set to NC_FILL works for record & non-record variables
 *    (note that it is not possible to test NC_NOFILL mode!)
 *    close file & create again for test using attribute _FillValue
 */
void
test_nc_set_default_format(void)
{
    int ncid;
    int err;
    int i;
    int version;
    int old_format;

    /* bad format */
    err = nc_set_default_format(BAD_DEFAULT_FORMAT, &old_format);
    IF (err != NC_EINVAL)
	error("bad default format: status = %d", err);

    /* NULL old_formatp */
    err = nc_set_default_format(NC_FORMAT_64BIT, NULL);
    IF (err)
	error("null old_fortmatp: status = %d", err);

    /* Cycle through available formats. */
    for(i=1; i<3; i++)
    {
       if ((err = nc_set_default_format(i, NULL)))
	  error("setting classic format: status = %d", err);
       if ((err=nc_create(scratch, NC_CLOBBER, &ncid)))
	  error("bad nc_create: status = %d", err);
       if ((err=nc_put_att_text(ncid, NC_GLOBAL, "testatt", 
				sizeof("blah"), "blah")))
	  error("bad put_att: status = %d", err);
       if ((err=nc_close(ncid)))
	  error("bad close: status = %d", err);
       if ((err = nc_get_file_version(scratch, &version)))
	  error("bad file version = %d", err);
       if (version != i)
	  error("bad file version = %d", err);
    }

    /* Remove the left-over file. */
    if (remove(scratch))
       error("remove of %s failed", scratch);
}
