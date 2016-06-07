/* err/test_errnos.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Gerard Jungman, Brian Gough
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_test.h>

#define CHECK(x) errors[n].number = x ; errors[n].name = #x ; n++ ;
#define MAX_ERRS 64

int verbose = 0 ;

int
main (void)
{
  int i, j, n = 0 ;

  struct { 
    int number; 
    const char * name; 
  } errors[MAX_ERRS] ;

  CHECK(GSL_SUCCESS);
  CHECK(GSL_FAILURE);
  CHECK(GSL_CONTINUE);
  CHECK(GSL_EDOM);
  CHECK(GSL_ERANGE);
  CHECK(GSL_EFAULT);
  CHECK(GSL_EINVAL);
  CHECK(GSL_EFAILED);
  CHECK(GSL_EFACTOR);
  CHECK(GSL_ESANITY);
  CHECK(GSL_ENOMEM);
  CHECK(GSL_EBADFUNC);
  CHECK(GSL_ERUNAWAY);
  CHECK(GSL_EMAXITER);
  CHECK(GSL_EZERODIV);
  CHECK(GSL_EBADTOL);
  CHECK(GSL_ETOL);
  CHECK(GSL_EUNDRFLW);
  CHECK(GSL_EOVRFLW);
  CHECK(GSL_ELOSS);
  CHECK(GSL_EROUND);
  CHECK(GSL_EBADLEN);
  CHECK(GSL_ENOTSQR);
  CHECK(GSL_ESING);
  CHECK(GSL_EDIVERGE);
  CHECK(GSL_EUNSUP);
  CHECK(GSL_EUNIMPL);
  CHECK(GSL_ECACHE);
  CHECK(GSL_ETABLE);
  CHECK(GSL_ENOPROG);
  CHECK(GSL_ENOPROGJ);
  CHECK(GSL_ETOLF);
  CHECK(GSL_ETOLX);
  CHECK(GSL_ETOLG);
  CHECK(GSL_EOF);

  for (i = 0 ; i < n ; i++) 
    {
      if (verbose) printf ("%s = %d\n", errors[i].name, errors[i].number) ;
    }

  for (i = 0; i < n; i++)
    {
      int status = 0;
      for (j = 0; j < n; j++)
        {
          if (j != i)
              status |= (errors[i].number == errors[j].number);
        }

      gsl_test (status, "%s is distinct from other error values",
                errors[i].name);
    }

  for (i = 0; i < n; i++)
    {
      int status = 0;
      int e1 = errors[i].number ;
      for (j = 0; j < n; j++)
        {
          if (j != i)
            {
              int e2 = errors[j].number;
              status |= (gsl_strerror(e1) == gsl_strerror(e2)) ;
            }
        }
      gsl_test (status, "%s has a distinct error message",
                errors[i].name);
    }

  
  exit (gsl_test_summary ());
}

