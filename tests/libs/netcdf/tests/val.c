/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/val.c,v 1.13 2006/10/31 16:22:01 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include "netcdf.h"
#include "testcdf.h"
#include "val.h"
#include "error.h"


/* fill typed value block with values of specified type */
void
val_fill(type, len, vals)
     nc_type type;		/* netcdf type, NC_BYTE, ..., NC_DOUBLE */
     long len;			/* number of elements to fill with */
     void *vals;		/* start of first block of values */
{
    static char pname[] = "val_fill";
    long half = len/2;
    int iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp;

    switch (type) {
      case NC_BYTE:
      case NC_CHAR:
	gp.cp = (char *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.cp++ = (char) iel;
	break;
      case NC_SHORT:
	gp.sp = (short *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.sp++ = (short) (iel - half); /* negative and positive values */
	break;
      case NC_LONG:
	gp.lp = (nclong *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.lp++ = (nclong) (iel - half);
	break;
      case NC_FLOAT:
	gp.fp = (float *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.fp++ = (float) (iel+1);
	break;
      case NC_DOUBLE:
	gp.dp = (double *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.dp++ = (double) (iel - half);
	break;
      default:
	error("%s: bad type, test program error", pname);
    }
}


/* fill typed value block with zeros of specified type */
void
val_fill_zero(type, len, vals)
     nc_type type;		/* netcdf type, NC_BYTE, ..., NC_DOUBLE */
     long len;			/* number of elements to fill with */
     void *vals;		/* start of first block of values */
{
    static char pname[] = "val_fill_zero";
    int iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp;

    switch (type) {
      case NC_BYTE:
      case NC_CHAR:
	gp.cp = (char *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.cp++ = (char) 0;
	break;
      case NC_SHORT:
	gp.sp = (short *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.sp++ = (short) 0;
	break;
      case NC_LONG:
	gp.lp = (nclong *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.lp++ = (nclong) 0;
	break;
      case NC_FLOAT:
	gp.fp = (float *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.fp++ = (float) 0;
	break;
      case NC_DOUBLE:
	gp.dp = (double *) vals;
	for (iel = 0; iel < len; iel++)
	  *gp.dp++ = (double) 0;
	break;
      default:
	error("%s: bad type, test program error", pname);
    }
}



/* 
 * compare two typed value blocks, return 0 if equal, 1+n otherwise, 
 * where n is the index of the first differing element.
 */
int
val_cmp (type, len, v1, v2)
     nc_type type;		/* netcdf type, NC_BYTE, ..., NC_DOUBLE */
     long len;			/* number of elements of type to compare */
     void *v1;			/* start of first block of values */
     void *v2;			/* start of second block of values */
{
    static char pname[] = "val_cmp";
    int iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp, hp;

    switch (type) {
      case NC_CHAR:
      case NC_BYTE:
	gp.cp = (char *) v1;
	hp.cp = (char *) v2;
	for (iel = 0; iel < len; iel++) {
	    if (*gp.cp != *hp.cp)
	      return (iel + 1);
	    gp.cp++;
	    hp.cp++;
	}
	break;
      case NC_SHORT:
	gp.sp = (short *) v1;
	hp.sp = (short *) v2;
	for (iel = 0; iel < len; iel++) {
	    if (*gp.sp != *hp.sp)
	      return (iel + 1);
	    gp.sp++;
	    hp.sp++;
	}
	break;
      case NC_LONG:
	gp.lp = (nclong *) v1;
	hp.lp = (nclong *) v2;
	for (iel = 0; iel < len; iel++) {
	    if (*gp.lp != *hp.lp)
	      return (iel + 1);
	    gp.lp++;
	    hp.lp++;
	}
	break;
      case NC_FLOAT:
	gp.fp = (float *) v1;
	hp.fp = (float *) v2;
	for (iel = 0; iel < len; iel++) {
	    if (*gp.fp != *hp.fp)
	      return (iel + 1);
	    gp.fp++;
	    hp.fp++;
	}
	break;
      case NC_DOUBLE:
	gp.dp = (double *) v1;
	hp.dp = (double *) v2;
	for (iel = 0; iel < len; iel++) {
	    if (*gp.dp != *hp.dp)
	      return (iel + 1);
	    gp.dp++;
	    hp.dp++;
	}
	break;
      default:
	error("%s: bad type, test program error", pname);
    }
    return 0;			/* all values agree */
}


/* print typed value block with values of specified type */
void
val_out(type, len, vals)
     nc_type type;		/* netcdf type, NC_BYTE, ..., NC_DOUBLE */
     long len;			/* number of elements to fill with */
     void *vals;		/* start of first block of values */
{
    static char pname[] = "val_oout";
    int iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp;

    (void) fprintf(stderr,"   ");
    switch (type) {
      case NC_BYTE:
      case NC_CHAR:
	gp.cp = (char *) vals;
	for (iel = 0; iel < len; iel++)
	  (void)fprintf(stderr,"%d%s",*gp.cp++,iel<len-1 ? ", " : "");
	break;
      case NC_SHORT:
	gp.sp = (short *) vals;
	for (iel = 0; iel < len; iel++)
	  (void)fprintf(stderr,"%d%s",*gp.sp++,iel<len-1 ? ", " : "");
	break;
      case NC_LONG:
	gp.lp = (nclong *) vals;
	for (iel = 0; iel < len; iel++)
	  (void)fprintf(stderr,"%d%s",*gp.lp++,iel<len-1 ? ", " : "");
	break;
      case NC_FLOAT:
	gp.fp = (float *) vals;
	for (iel = 0; iel < len; iel++)
	  (void)fprintf(stderr,"%g%s",*gp.fp++,iel<len-1 ? ", " : "");
	break;
      case NC_DOUBLE:
	gp.dp = (double *) vals;
	for (iel = 0; iel < len; iel++)
	  (void)fprintf(stderr,"%g%s",*gp.dp++,iel<len-1 ? ", " : "");
	break;
      default:
	error("%s: bad type, test program error", pname);
    }
    (void) putc('\n',stderr);
}
