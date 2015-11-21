/* include/c_defs.h.  Generated from c_defs.h.in by configure.  */
/*
Copyright (C) 2006 Quantum-ESPRESSO group
This file is distributed under the terms of the
GNU General Public License. See the file `License'
in the root directory of the present distribution,
or http://www.gnu.org/copyleft/gpl.txt .
*/

/* File c_defs.h.in is used by configure to generate c_defs.h
   Variables that configure defines should be #undef-ined in
   include/c_defs.h.in !!! */

/* fortran-to-C naming convention, for functions with and without
   underscores in the name (some compilers treat them differently) */ 

#define F77_FUNC(name,NAME) name ## _
#define F77_FUNC_(name,NAME) name ## _

/* do we have the mallinfo structure (see clib/memstat.c) ? */

/* #undef HAVE_MALLINFO */
