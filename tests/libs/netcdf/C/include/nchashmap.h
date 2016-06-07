/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header$
 *********************************************************************/
#ifndef NCHASHMAP_H
#define NCHASHMAP_H 1

#if defined(_CPLUSPLUS_) || defined(__CPLUSPLUS__)
#define externC extern "C"
#else
#define externC extern
#endif

#include "nclist.h"

/* Define the type of the elements in the hashmap*/
typedef unsigned long nchashid;

externC int nchashnull(void*);

typedef struct NChashmap {
  size_t alloc;
  size_t size; /* # of pairs still in table*/
  NClist** table;
} NChashmap;

externC NChashmap* nchashnew(void);
externC NChashmap* nchashnew0(size_t);
externC int nchashfree(NChashmap*);

/* Insert a (ncnchashid,void*) pair into the table*/
/* Fail if already there*/
externC int nchashinsert(NChashmap*, nchashid nchash, void* value);

/* Insert a (nchashid,void*) pair into the table*/
/* Overwrite if already there*/
externC int nchashreplace(NChashmap*, nchashid nchash, void* value);

/* lookup a nchashid and return found/notfound*/
externC int nchashlookup(NChashmap*, nchashid nchash, void** valuep);

/* lookup a nchashid and return 0 or the value*/
externC void* nchashget(NChashmap*, nchashid nchash);

/* remove a nchashid*/
externC int nchashremove(NChashmap*, nchashid nchash);

/* Return the ith pair; order is completely arbitrary*/
/* Can be expensive*/
externC int nchashith(NChashmap*, int i, nchashid*, void**);

externC int nchashkeys(NChashmap* hm, nchashid** keylist);

/* return the # of pairs in table*/
#define nchashsize(hm) ((hm)?(hm)->size:0)

#endif /*NCHASHMAP_H*/

