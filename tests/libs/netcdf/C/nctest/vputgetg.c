/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: vputgetg.c,v 1.13 2006/10/31 16:19:40 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>		/* for free() */
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "add.h"		/* functions to update in-memory netcdf */
#include "val.h"
#include "error.h"
#include "tests.h"
#include "emalloc.h"

#undef max
#define max(A, B)	((A) > (B) ? (A) : (B))

/* 
 * For every variable in open netcdf, puts and gets three hypercubes 
 * of data of the appropriate type, comparing values from get to 
 * values put to check that both ncvarputg and ncvargetg worked.  The 
 * three hypercubes are
 *    - a large hypercube from (0, 0, ...) to the far corner (diagonally 
 *      opposite (0, 0, ...), trivial strides and index mapping vector;
 *    - a size 1 hypercube from the far corner with edge lengths of 1 
 *      in every direction, trivial strides and index mapping vector; and
 *    - a hypercube starting about 1/3 of the way along the diagonal
 *      from (0,0,...) extending 1/3 of the way in every direction 
 *      toward the far corner, dimension-dependent strides and inverted
 *	index mapping vector rooted at the "upper-left" corned.
 */

int
test_varputgetg(cdfid)
     int cdfid;			/* handle of netcdf open and in data mode */
{
    int nerrs = 0;
    static char pname[] = "test_varputgetg";
    int id, ie, iv;		/* loop indices */
    int ne = 3;			/* number of test hypercubes for each var */
    struct cdfhc {		/* a hypercube with generic values */
	long cor[MAX_NC_DIMS];	/* netcdf coordinates for lower corner */
	long npts[MAX_NC_DIMS];	/* netcdf edge lengths to upper corner */
	long strd[MAX_NC_DIMS];	/* external strides */
	long imap[MAX_NC_DIMS];	/* internal, index mapping vector */
	long offset;		/* offset in bytes to I/O start corner */
	void *vals;		/* pointer to block of values */
    } hc[3], tmp;		/* test hypercubes */
    long nel[3];		/* number of elements in hypercube */

    for (iv = 0; iv < test.nvars; iv++)	{ /* for each var in netcdf */

	for (ie = 0; ie < ne; ie++)
	  nel[ie] = 1;		/* to compute space for hypercube values */

	/*
	 * The following macro returns the size of a dimension for a
	 * variable with a maximum  dimension size of 5 for the record
	 * dimension.
	 */
#	define EXTNPTS(varid, idim)	\
	    (test.dims[test.vars[varid].dims[id]].size == NC_UNLIMITED \
		? 5 \
		: test.dims[test.vars[varid].dims[id]].size)
#	define STRIDE(idim)		(idim + 2)
#	define INTNPTS(extnpts, idim)	(1 + (extnpts - 1) / STRIDE(idim))


	for (id = test.vars[iv].ndims-1; id >= 0; --id) { /* set cubes */

	    /* start at "lower-left" corner, do whole variable.  unity
	     * strides and trivial index mapping */
	    hc[0].cor[id]	= 0;
	    hc[0].npts[id]	= EXTNPTS(iv, id);
	    hc[0].strd[id]	= 1;
	    hc[0].imap[id]	= id == test.vars[iv].ndims-1
					? nctypelen(test.vars[iv].type)
					: hc[0].imap[id+1] * hc[0].npts[id+1];
	    nel[0]		*= hc[0].npts[id];
	    if (id == 0)
		hc[0].offset	= 0;

	    /* start at "upper-right" corner, do one point */
	    hc[1].cor[id]	= EXTNPTS(iv, id) - 1;
	    hc[1].npts[id]	= 1;
	    hc[1].strd[id]	= 1;
	    hc[1].imap[id]	= id == test.vars[iv].ndims-1
					? nctypelen(test.vars[iv].type)
					: hc[1].imap[id+1] * hc[1].npts[id+1];
	    nel[1]		*= hc[1].npts[id];
	    if (id == 0)
		hc[1].offset	= 0;

	    /* start about 1/3 way along diagonal, do 1/3 in each direction.
	     * dimension-dependent strides; inverted index mapping starting
	     * from "upper-right" corner. */
	    hc[2].cor[id]	= EXTNPTS(iv, id)/3;
	    hc[2].npts[id]	= INTNPTS(max(EXTNPTS(iv, id)/3, 1), id);
	    hc[2].strd[id]	= STRIDE(id);
	    hc[2].imap[id]	= id == test.vars[iv].ndims-1
					? -nctypelen(test.vars[iv].type)
					: hc[2].imap[id+1] * hc[2].npts[id+1];
	    nel[2]		*= hc[2].npts[id];
	    if (id == 0)
		hc[2].offset	= (nel[2]-1)*nctypelen(test.vars[iv].type);
	}

	for (ie = 0; ie < ne; ie++) { /* for each test */
	    int nelms = (int)nel[ie]*nctypelen(test.vars[iv].type) + 8;
	    /* allocate space for the cube of values */
	    hc[ie].vals	= emalloc(nelms);
	    tmp.vals = emalloc(nelms);

	    /* fill allocated space with different values of right type */
	    val_fill(test.vars[iv].type, nel[ie], hc[ie].vals);

	    if(ncvarputg (cdfid, iv, hc[ie].cor, hc[ie].npts, 
			  hc[ie].strd, hc[ie].imap, 
			  (char*)hc[ie].vals+hc[ie].offset)
	       == -1) {
		error("%s: ncvarputg failed for point %d, variable %s",
		      pname, ie, test.vars[iv].name);
		nerrs++;
		errvar(&test, &test.vars[iv]);
		(void)fprintf(stderr,"  corner = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%ld%s",(long)hc[ie].cor[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
		(void)fprintf(stderr,"  npts = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%ld%s",(long)hc[ie].npts[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
		(void)fprintf(stderr,"  external strides = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%ld%s",(long)hc[ie].strd[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
		(void)fprintf(stderr,"  internal index mapping vector = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%ld%s",(long)hc[ie].imap[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
	    } else {
		long	dsize[MAX_NC_DIMS];

		for (id = 0; id < test.vars[iv].ndims; id++)
		    dsize[id]	= EXTNPTS(iv, id);
		add_data(&test, iv, hc[ie].cor, dsize);
						    /* keep test in sync */
		if(ncvargetg (cdfid, iv, hc[ie].cor, hc[ie].npts, 
			      hc[ie].strd, hc[ie].imap,
			      (char*)tmp.vals+hc[ie].offset)
		   == -1) {
		    error("%s: ncvargetg failed for point %d, variable %s",
			  pname, ie, test.vars[iv].name);
		    nerrs++;
		}
		else {
		    if (val_cmp(test.vars[iv].type, nel[ie],
				hc[ie].vals, tmp.vals) != 0) {
			error("%s: bad values returned from ncvargetg",
			      pname);
			nerrs++;
			errvar(&test, &test.vars[iv]); /* describe var */
		    }
		}
	    }

	    free (hc[ie].vals);
	    free (tmp.vals);
	}
    }
    return nerrs;
}
