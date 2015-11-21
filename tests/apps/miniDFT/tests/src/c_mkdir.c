/*
  Copyright (C) 2003-2007 Quantum ESPRESSO group
  This file is distributed under the terms of the
  GNU General Public License. See the file `License'
  in the root directory of the present distribution,
  or http://www.gnu.org/copyleft/gpl.txt .
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <time.h>
#include "c_defs.h"

static void fatal ( const char * msg )
{

   fprintf( stderr , "fatal: %s" , *msg ? msg : "Oops!" ) ;
   exit( -1 ) ;

} /* fatal */


static void * xcmalloc ( size_t size )
{

  register void * ptr = malloc( size ) ;

  if ( ptr == NULL )
    fatal( "c_mkdir: virtual memory exhausted" ) ;
  else
    memset( ptr , 0 , size ) ;

  return ptr ;

} /* xcmalloc */


int F77_FUNC_(c_mkdir_int,C_MKDIR_INT)( const int * dirname , const int * length )
{

   int i, retval = -1 ;

   mode_t mode = 0777 ;

   char * ldir = ( char * ) xcmalloc( (*length) + 1 ) ;

   for( i = 0; i < * length; i++ ) ldir[ i ] = (char)dirname[ i ];

   ldir[*length] = '\0' ;	/* memset() in xcmalloc() already do this */

   retval = mkdir( ldir , mode ) ;

   if ( retval == -1  && errno != EEXIST ) {
     fprintf( stderr , "mkdir fail: [%d] %s\n" , errno , strerror( errno ) ) ;
     }
   else {
     retval = 0 ;
     }


   free( ldir ) ;

   return retval ;

} /* end of c_mkdir */

int c_mkdir_safe( const char * dirname )
{
   int i, retval = -1 ;

   mode_t mode = 0777 ;
   retval = mkdir( dirname , mode ) ;

   if ( retval == -1  && errno != EEXIST ) {
     fprintf( stderr , "mkdir fail: [%d] %s\n" , errno , strerror( errno ) ) ;
     }
   else {
     retval = 0 ;
     }
   return retval ;
}

int F77_FUNC_(c_chdir_int,C_CHDIR_INT)( const int * dirname , const int * length )
{

   int i, retval = -1 ;

   char * ldir = ( char * ) xcmalloc( (*length) + 1 ) ;

   for( i = 0; i < * length; i++ ) ldir[ i ] = (char)dirname[ i ];

   ldir[*length] = '\0' ;       /* memset() in xcmalloc() already do this */

   retval = chdir( ldir ) ;

   if ( retval == -1  && errno != EEXIST ) {
     fprintf( stderr , "chdir fail: [%d] %s\n" , errno , strerror( errno ) ) ;
     }
   else {
     retval = 0 ;
     }


   free( ldir ) ;

   return retval ;

} /* end of c_chdir */

/* c_rename: call from fortran as
   ios = c_remame ( integer old-file-name(:), integer old-file-name, &
                    integer new-file-name(:), integer new-file-name )
   renames file old-file-name into new-file-name (don't try this on open files!)
   ios should return 0 if everything is ok, -1 otherwise.
   Written by PG by imitating "c_mkdir" without really understanding it */

int F77_FUNC_(c_rename_int,C_RENAME_INT)( const int * oldname, const int * oldlength ,
                                  const int * newname, const int * newlength )
{

   int i, retval = -1 ;

   char * oldname_ = ( char * ) xcmalloc( (*oldlength) + 1 ) ;
   char * newname_ = ( char * ) xcmalloc( (*newlength) + 1 ) ;

   for( i = 0; i < * oldlength; i++ ) oldname_[ i ] = (char)oldname[ i ];
   for( i = 0; i < * newlength; i++ ) newname_[ i ] = (char)newname[ i ];

   oldname_[*oldlength] = '\0' ;
   newname_[*newlength] = '\0' ;

   retval = rename( oldname_, newname_ ) ;

   if ( retval == -1 )
     fprintf( stderr , "mv fail: [%d] %s\n" , errno , strerror( errno ) ) ;

   free( oldname_ ) ;
   free( newname_ ) ;

   return retval ;

} /* c_rename */

int F77_FUNC_(c_link_int,C_LINK_INT)( const int * oldname, const int * oldlength ,
                                  const int * newname, const int * newlength )
{

   int i, retval = -1 ;

   char * oldname_ = ( char * ) xcmalloc( (*oldlength) + 1 ) ;
   char * newname_ = ( char * ) xcmalloc( (*newlength) + 1 ) ;

   for( i = 0; i < * oldlength; i++ ) oldname_[ i ] = (char)oldname[ i ];
   for( i = 0; i < * newlength; i++ ) newname_[ i ] = (char)newname[ i ];

   oldname_[*oldlength] = '\0' ;
   newname_[*newlength] = '\0' ;

   retval = symlink( oldname_, newname_ ) ;

   if ( retval == -1 )
     fprintf( stderr , "ln fail: [%d] %s\n" , errno , strerror( errno ) ) ;

   free( oldname_ ) ;
   free( newname_ ) ;

   return retval ;

} /* c_link */

/* EOF */
