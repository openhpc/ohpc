/* statistics/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Jim Davies, Brian Gough
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
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_sort_vector.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_ieee_utils.h>

int test_nist (void);

/* Test program for mean.c.  JimDavies 7.96 */

#define BASE_LONG_DOUBLE
#include "templates_on.h"
#include "test_float_source.c"
#include "templates_off.h"
#undef  BASE_LONG_DOUBLE

#define BASE_DOUBLE
#include "templates_on.h"
#include "test_float_source.c"
#include "templates_off.h"
#undef  BASE_DOUBLE

#define BASE_FLOAT
#include "templates_on.h"
#include "test_float_source.c"
#include "templates_off.h"
#undef  BASE_FLOAT

#define BASE_ULONG
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_ULONG

#define BASE_LONG
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_LONG

#define BASE_UINT
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_UINT

#define BASE_INT
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_INT

#define BASE_USHORT
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_USHORT

#define BASE_SHORT
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_SHORT

#define BASE_UCHAR
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_UCHAR

#define BASE_CHAR
#include "templates_on.h"
#include "test_int_source.c"
#include "templates_off.h"
#undef  BASE_CHAR


int
main (void)
{
  size_t s1, s2;

  gsl_ieee_env_setup ();

  for (s1 = 1; s1 < 4 ; s1++) 
    {
      s2 = (s1 < 3) ? 1 : (s1 - 1) ;

      test_func (s1,s2);
      test_float_func (s1,s2);
      test_long_double_func (s1,s2);
      
      test_ulong_func (s1,s2);
      test_long_func (s1,s2);
      test_uint_func (s1,s2);
      test_int_func (s1,s2);
      test_ushort_func (s1,s2);
      test_short_func (s1,s2);
      test_uchar_func (s1,s2);
      test_char_func (s1,s2);
    }

  test_nist();

  exit (gsl_test_summary ());
}

