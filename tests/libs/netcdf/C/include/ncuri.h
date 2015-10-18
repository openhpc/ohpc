/* Copyright 2009, UCAR/Unidata and OPeNDAP, Inc.
   See the COPYRIGHT file for more information. */

#ifndef NCURI_H
#define NCURI_H

/*! This is an open structure meaning
	it is ok to directly access its fields*/
typedef struct NCURI {
    char* uri;        /* as passed by the caller */
    char* params;     /* all params */
    char** paramlist;    /*!<null terminated list */
    char* constraint; /*!< projection+selection */
    char* projection; /*!< without leading '?'*/
    char* selection;  /*!< with leading '&'*/
    char* strings;    /* first char of strings is always '\0' */
    /* Following all point into the strings field */
    char* protocol;
    char* user; /* from user:password@ */
    char* password; /* from user:password@ */
    char* host;	      /*!< host*/
    char* port;	      /*!< host */
    char* file;	      /*!< file */
} NCURI;

extern int ncuriparse(const char* s, NCURI** ncuri);
extern void ncurifree(NCURI* ncuri);

/* Replace the constraints */
extern void ncurisetconstraints(NCURI*,const char* constraints);

/* Construct a complete NC URI; caller frees returned string */

/* Define flags to control what is included */
#define NCURICONSTRAINTS	 1
#define NCURIUSERPWD	  	 2
#define NCURIPREFIXPARAMS  	 4
#define NCURISUFFIXPARAMS	 8
#define NCURIPARAMS	  	NCURIPREFIXPARAMS
#define NCURIENCODE		16 /* If output should be encoded */
#define NCURISTD	  	(NCURICONSTRAINTS|NCURIUSERPWD)

extern char* ncuribuild(NCURI*,const char* prefix, const char* suffix, int flags);


/* Param Management */
extern int ncuridecodeparams(NCURI* ncuri);
extern int ncurisetparams(NCURI* ncuri,const char*);

/*! 0 result => entry not found; 1=>found; result holds value (may be null).
    In any case, the result is imutable and should not be free'd.
*/
extern int ncurilookup(NCURI*, const char* param, const char** result);

extern char* ncuriencode(char* s, char* allowable);
extern char* ncuridecode(char* s);
extern char* ncuridecodeonly(char* s, char*);

#endif /*NCURI_H*/
