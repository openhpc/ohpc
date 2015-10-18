/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/val.h,v 1.7 2006/10/31 16:23:51 ed Exp $
 *********************************************************************/

#include <config.h>
#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* fill typed value block with values of specified type */
extern	void	val_fill	PROTO((
				       nc_type ,
				       long,
				       void *
				       ));

/* fill typed value block with zeros of specified type */
extern	void	val_fill_zero	PROTO((
				       nc_type ,
				       long,
				       void *
				       ));

/* 
 * compare two typed value blocks, return 0 if equal, 1+n otherwise, 
 * where n is the index of the first differing element.
 */
extern	int	val_cmp		PROTO((
				       nc_type ,
				       long,
				       void *,
				       void *
				       ));

/* print typed value block with values of specified type */
extern	void	val_out		PROTO((
				       nc_type ,
				       long,
				       void *
				       ));
#ifdef __cplusplus
}
#endif
