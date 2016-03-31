/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header $
 *********************************************************************/

#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Print error message to stderr, don't exit */
extern void	error		PROTO((
				       const char *fmt,
				       ...
				       ));

/*
 * Turn off netCDF library handling of errors.  Caller must check all error
 * returns after calling this, until on_errs() is called.
 */
extern void	off_errs	PROTO((
				       void
				       ));

/*
 * Let netCDF library handle subsequent errors.  Callers don't need to check
 * error returns after this.  (This is the initial default.)
 */
extern void	on_errs		PROTO((
				       void
				       ));

#ifdef __cplusplus
}
#endif
