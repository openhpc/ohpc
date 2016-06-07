/* Copyright 2009, UCAR/Unidata and OPeNDAP, Inc.
   See the COPYRIGHT file for more information. */
#ifndef NCLIST_H
#define NCLIST_H 1

/* Define the type of the elements in the list*/

#if defined(_CPLUSPLUS_) || defined(__CPLUSPLUS__)
#define EXTERNC extern "C"
#else
#define EXTERNC extern
#endif

EXTERNC int nclistnull(void*);

typedef struct NClist {
  unsigned long alloc;
  unsigned long length;
  void** content;
} NClist;

EXTERNC NClist* nclistnew(void);
EXTERNC int nclistfree(NClist*);
EXTERNC int nclistsetalloc(NClist*,unsigned long);
EXTERNC int nclistsetlength(NClist*,unsigned long);

/* Set the ith element */
EXTERNC int nclistset(NClist*,unsigned long,void*);
/* Get value at position i */
EXTERNC void* nclistget(NClist*,unsigned long);/* Return the ith element of l */
/* Insert at position i; will push up elements i..|seq|. */
EXTERNC int nclistinsert(NClist*,unsigned long,void*);
/* Remove element at position i; will move higher elements down */
EXTERNC void* nclistremove(NClist* l, unsigned long i);

/* Tail operations */
EXTERNC int nclistpush(NClist*,void*); /* Add at Tail */
EXTERNC void* nclistpop(NClist*);
EXTERNC void* nclisttop(NClist*);

/* Duplicate and return the content (null terminate) */
EXTERNC void** nclistdup(NClist*);

/* Look for value match */
EXTERNC int nclistcontains(NClist*, void*);

/* Remove element by value; only removes first encountered */
EXTERNC int nclistelemremove(NClist* l, void* elem);

/* remove duplicates */
EXTERNC int nclistunique(NClist*);

/* Create a clone of a list */
EXTERNC NClist* nclistclone(NClist*);

/* Following are always "in-lined"*/
#define nclistclear(l) nclistsetlength((l),0)
#define nclistextend(l,len) nclistsetalloc((l),(len)+(l->alloc))
#define nclistcontents(l)  ((l)==NULL?NULL:(l)->content)
#define nclistlength(l)  ((l)==NULL?0:(l)->length)

#endif /*NCLIST_H*/
