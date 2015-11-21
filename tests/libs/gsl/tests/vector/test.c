/* vector/test.c
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

#if (!GSL_RANGE_CHECK) && defined(HAVE_INLINE)
#undef GSL_RANGE_CHECK
#define GSL_RANGE_CHECK 1
#endif

#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

int status = 0;

#ifndef DESC
#define DESC ""
#endif

#define BASE_GSL_COMPLEX_LONG
#include "templates_on.h"
#include "test_complex_source.c"
#include "templates_off.h"
#undef  BASE_GSL_COMPLEX_LONG

#define BASE_GSL_COMPLEX
#include "templates_on.h"
#include "test_complex_source.c"
#include "templates_off.h"
#undef  BASE_GSL_COMPLEX

#define BASE_GSL_COMPLEX_FLOAT
#include "templates_on.h"
#include "test_complex_source.c"
#include "templates_off.h"
#undef  BASE_GSL_COMPLEX_FLOAT

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

void my_error_handler (const char *reason, const char *file,
                       int line, int err);

int
main (void)
{
  size_t stride, ostride, N;

  gsl_ieee_env_setup ();

  for (N = 10; N < 1024; N = 2*N + 1) 
    {
      for (stride = 1; stride < 5 ; stride++)
        {
          test_func (stride, N);
          test_float_func (stride, N);
          test_long_double_func (stride, N);
          test_ulong_func (stride, N);
          test_long_func (stride, N);
          test_uint_func (stride, N);
          test_int_func (stride, N);
          test_ushort_func (stride, N);
          test_short_func (stride, N);
          test_uchar_func (stride, N);
          test_char_func (stride, N);

          test_complex_func (stride, N);
          test_complex_float_func (stride, N);
          test_complex_long_double_func (stride, N);

          for (ostride = 1; ostride < 5 ; ostride++)
            {
              test_ops (stride, ostride, N);
              test_float_ops (stride, ostride, N);
              test_long_double_ops (stride, ostride, N);
              test_ulong_ops (stride, ostride, N);
              test_long_ops (stride, ostride, N);
              test_uint_ops (stride, ostride, N);
              test_int_ops (stride, ostride, N);
              test_ushort_ops (stride, ostride, N);
              test_short_ops (stride, ostride, N);
              test_uchar_ops (stride, ostride, N);
              test_char_ops (stride, ostride, N);
              test_complex_ops (stride, ostride, N);
              test_complex_float_ops (stride, ostride, N);
              test_complex_long_double_ops (stride, ostride, N);
            }              

          test_text (stride, N);
          test_float_text (stride, N);
#if HAVE_PRINTF_LONGDOUBLE
          test_long_double_text (stride, N);
#endif
          test_ulong_text (stride, N);
          test_long_text (stride, N);
          test_uint_text (stride, N);
          test_int_text (stride, N);
          test_ushort_text (stride, N);
          test_short_text (stride, N);
          test_uchar_text (stride, N);
          test_char_text (stride, N);

          test_complex_text (stride, N);
          test_complex_float_text (stride, N);
#if HAVE_PRINTF_LONGDOUBLE
          test_complex_long_double_text (stride, N);
#endif

          test_file (stride, N);
          test_float_file (stride, N);
          test_long_double_file (stride, N);
          test_ulong_file (stride, N);
          test_long_file (stride, N);
          test_uint_file (stride, N);
          test_int_file (stride, N);
          test_ushort_file (stride, N);
          test_short_file (stride, N);
          test_uchar_file (stride, N);
          test_char_file (stride, N);
          test_complex_file (stride, N);
          test_complex_float_file (stride, N);
          test_complex_long_double_file (stride, N);
        }
    }

#if GSL_RANGE_CHECK
  gsl_set_error_handler (&my_error_handler);

  for (N = 1; N < 1024; N *=2) 
    {
      for (stride = 1; stride < 5 ; stride++)
        {
          test_trap (stride, N);
          test_float_trap (stride, N);
          test_long_double_trap (stride, N);
          test_ulong_trap (stride, N);
          test_long_trap (stride, N);
          test_uint_trap (stride, N);
          test_int_trap (stride, N);
          test_ushort_trap (stride, N);
          test_short_trap (stride, N);
          test_uchar_trap (stride, N);
          test_char_trap (stride, N);
          test_complex_trap (stride, N);
          test_complex_float_trap (stride, N);
          test_complex_long_double_trap (stride, N);
        }
    }
#endif

  exit (gsl_test_summary ());
}

void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (0)
    printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err);
  status = 1;
}
