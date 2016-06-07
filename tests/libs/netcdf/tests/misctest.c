/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/misctest.c,v 1.11 2006/10/31 16:21:57 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <string.h>
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "add.h"		/* functions to update in-memory netcdf */
#include "error.h"
#include "tests.h"

/*
 * Test nctypelen
 *    try with bad datatype, check error
 *    check returned values for each proper datatype
 */
int
test_nctypelen()
{
    int nerrs = 0;
    static char pname[] = "test_nctypelen";

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if (nctypelen(NC_BYTE) != sizeof(char)) {
	error("%s: nctypelen failed for NC_BYTE", pname);
	nerrs++;
    }
    if (nctypelen(NC_CHAR) != sizeof(char)) {
	error("%s: nctypelen failed for NC_CHAR", pname);
	nerrs++;
    }
    if (nctypelen(NC_SHORT) != sizeof(short)) {
	error("%s: nctypelen failed for NC_SHORT", pname);
	nerrs++;
    }
    if (nctypelen(NC_LONG) != sizeof(nclong)) {
	error("%s: nctypelen failed for NC_LONG", pname);
	nerrs++;
    }
    if (nctypelen(NC_FLOAT) != sizeof(float)) {
	error("%s: nctypelen failed for NC_FLOAT", pname);
	nerrs++;
    }
    if (nctypelen(NC_DOUBLE) != sizeof(double)) {
	error("%s: nctypelen failed for NC_DOUBLE", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");

    return nerrs;
}
