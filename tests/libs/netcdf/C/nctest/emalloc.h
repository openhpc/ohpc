/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header $
 *********************************************************************/
#ifndef _EMALLOC_H_
#define _EMALLOC_H_

#include <config.h>
#include <stdlib.h> /* free() */

#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern void	*emalloc	PROTO((
				       size_t size
				       ));

extern void	*erealloc	PROTO((
				       void *ptr,
				       size_t size
				       ));

#ifdef __cplusplus
}
#endif
#endif /* !_EMALLOC_H_ */
