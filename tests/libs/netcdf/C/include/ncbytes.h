/* Copyright 2009, UCAR/Unidata and OPeNDAP, Inc.
   See the COPYRIGHT file for more information. */

#ifndef NCBYTES_H
#define NCBYTES_H 1

typedef struct NCbytes {
  int nonextendible; /* 1 => fail if an attempt is made to extend this buffer*/
  unsigned long alloc;
  unsigned long length;
  char* content;
} NCbytes;

#if defined(_CPLUSPLUS_) || defined(__CPLUSPLUS__) || defined(__CPLUSPLUS)
#define EXTERNC extern "C"
#else
#define EXTERNC extern
#endif

EXTERNC NCbytes* ncbytesnew(void);
EXTERNC void ncbytesfree(NCbytes*);
EXTERNC int ncbytessetalloc(NCbytes*,unsigned long);
EXTERNC int ncbytessetlength(NCbytes*,unsigned long);
EXTERNC int ncbytesfill(NCbytes*, char fill);

/* Produce a duplicate of the contents*/
EXTERNC char* ncbytesdup(NCbytes*);
/* Extract the contents and leave buffer empty */
EXTERNC char* ncbytesextract(NCbytes*);

/* Return the ith byte; -1 if no such index */
EXTERNC int ncbytesget(NCbytes*,unsigned long);
/* Set the ith byte */
EXTERNC int ncbytesset(NCbytes*,unsigned long,char);

/* Append one byte */
EXTERNC int ncbytesappend(NCbytes*,char); /* Add at Tail */
/* Append n bytes */
EXTERNC int ncbytesappendn(NCbytes*,const void*,unsigned long); /* Add at Tail */

/* Null terminate the byte string without extending its length (for debugging) */
EXTERNC int ncbytesnull(NCbytes*);

/* Concatenate a null-terminated string to the end of the buffer */
EXTERNC int ncbytescat(NCbytes*,const char*);

/* Set the contents of the buffer; mark the buffer as non-extendible */
EXTERNC int ncbytessetcontents(NCbytes*, char*, unsigned long);

/* Following are always "in-lined"*/
#define ncbyteslength(bb) ((bb)!=NULL?(bb)->length:0)
#define ncbytesalloc(bb) ((bb)!=NULL?(bb)->alloc:0)
#define ncbytescontents(bb) (((bb)!=NULL && (bb)->content!=NULL)?(bb)->content:(char*)"")
#define ncbytesextend(bb,len) ncbytessetalloc((bb),(len)+(bb->alloc))
#define ncbytesclear(bb) ((bb)!=NULL?(bb)->length=0:0)
#define ncbytesavail(bb,n) ((bb)!=NULL?((bb)->alloc - (bb)->length) >= (n):0)

#endif /*NCBYTES_H*/
