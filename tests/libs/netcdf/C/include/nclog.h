/*********************************************************************
 *   Copyright 2010, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header$
 *********************************************************************/

#ifndef NCLOG_H
#define NCLOG_H

#define NCENVFLAG "NCLOGFILE"

/* Suggested tag values */
#define NCLOGNOTE 0
#define NCLOGWARN 1
#define NCLOGERR 2
#define NCLOGDBG 3

extern void ncloginit(void);
extern int ncsetlogging(int tf);
extern int nclogopen(const char* file);
extern void nclogclose(void);

/* The tag value is an arbitrary integer */
extern void nclog(int tag, const char* fmt, ...);
extern void nclogtext(int tag, const char* text);
extern void nclogtextn(int tag, const char* text, size_t count);

/* Provide printable names for tags */
extern void nclogsettags(char** tagset, char* dfalt);

#endif /*NCLOG_H*/
