/* sort/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Thomas Walter, Brian Gough
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
#include <gsl/gsl_math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_heapsort.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_sort_vector.h>
#include <gsl/gsl_ieee_utils.h>

size_t urand (size_t);

#include "test_heapsort.c"

#define BASE_LONG_DOUBLE
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_LONG_DOUBLE

#define BASE_DOUBLE
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_DOUBLE

#define BASE_FLOAT
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_FLOAT

#define BASE_ULONG
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_ULONG

#define BASE_LONG
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_LONG

#define BASE_UINT
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_UINT

#define BASE_INT
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_INT

#define BASE_USHORT
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_USHORT

#define BASE_SHORT
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_SHORT

#define BASE_UCHAR
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_UCHAR

#define BASE_CHAR
#include "templates_on.h"
#include "test_source.c"
#include "templates_off.h"
#undef  BASE_CHAR

int
main (void)
{
  size_t i, s;

  gsl_ieee_env_setup ();

  /* Test for lengths of 1 ... 31, then 32, 64, 128, 256, ... */

  for (i = 1; i < 1024; i = (i < 32) ? i + 1 : 2 * i)
    test_heapsort (i);

  for (i = 1; i < 1024; i = (i < 32) ? i + 1 : 2 * i)
    {
      for (s = 1; s < 4; s++)
        {
          test_sort_vector (i, s);
          test_sort_vector_float (i, s);
          test_sort_vector_long_double (i, s);
          test_sort_vector_ulong (i, s);
          test_sort_vector_long (i, s);
          test_sort_vector_uint (i, s);
          test_sort_vector_int (i, s);
          test_sort_vector_ushort (i, s);
          test_sort_vector_short (i, s);
          test_sort_vector_uchar (i, s);
          test_sort_vector_char (i, s);
        }
    }

  exit (gsl_test_summary ());
}

size_t 
urand (size_t N)
{
  static unsigned long int x = 1;
  x = (1103515245 * x + 12345) & 0x7fffffffUL;
  return (size_t) ((x / 2147483648.0) * N);
}
