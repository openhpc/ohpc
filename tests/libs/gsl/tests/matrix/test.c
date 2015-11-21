/* matrix/test.c
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
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
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
  size_t M = 53;
  size_t N = 107;

  gsl_ieee_env_setup ();

  test_func (M, N);
  test_float_func (M, N);
  test_long_double_func (M, N);
  test_ulong_func (M, N);
  test_long_func (M, N);
  test_uint_func (M, N);
  test_int_func (M, N);
  test_ushort_func (M, N);
  test_short_func (M, N);
  test_uchar_func (M, N);
  test_char_func (M, N);
  test_complex_func (M, N);
  test_complex_float_func (M, N);
  test_complex_long_double_func (M, N);

  test_ops (M, N);
  test_float_ops (M, N);
  test_long_double_ops (M, N);
  test_ulong_ops (M, N);
  test_long_ops (M, N);
  test_uint_ops (M, N);
  test_int_ops (M, N);
  test_ushort_ops (M, N);
  test_short_ops (M, N);
  test_uchar_ops (M, N);
  test_char_ops (M, N);

  /* Must use smaller dimensions to prevent approximation of floats in
     float_mul_elements test*/
  
  {
    const size_t P = 8;
    const size_t Q = 12;

    test_complex_ops (P, Q);
    test_complex_float_ops (P, Q);
    test_complex_long_double_ops (P, Q);
  }

  test_text (M, N);
  test_float_text (M, N);
#if HAVE_PRINTF_LONGDOUBLE
  test_long_double_text (M, N);
#endif
  test_ulong_text (M, N);
  test_long_text (M, N);
  test_uint_text (M, N);
  test_int_text (M, N);
  test_ushort_text (M, N);
  test_short_text (M, N);
  test_uchar_text (M, N);
  test_char_text (M, N);
  test_complex_text (M, N);
  test_complex_float_text (M, N);
#if HAVE_PRINTF_LONGDOUBLE
  test_complex_long_double_text (M, N);
#endif

  test_binary (M, N);
  test_float_binary (M, N);
  test_long_double_binary (M, N);
  test_ulong_binary (M, N);
  test_long_binary (M, N);
  test_uint_binary (M, N);
  test_int_binary (M, N);
  test_ushort_binary (M, N);
  test_short_binary (M, N);
  test_uchar_binary (M, N);
  test_char_binary (M, N);
  test_complex_binary (M, N);
  test_complex_float_binary (M, N);
  test_complex_long_double_binary (M, N);

  test_binary_noncontiguous (M, N);
  test_float_binary_noncontiguous (M, N);
  test_long_double_binary_noncontiguous (M, N);
  test_ulong_binary_noncontiguous (M, N);
  test_long_binary_noncontiguous (M, N);
  test_uint_binary_noncontiguous (M, N);
  test_int_binary_noncontiguous (M, N);
  test_ushort_binary_noncontiguous (M, N);
  test_short_binary_noncontiguous (M, N);
  test_uchar_binary_noncontiguous (M, N);
  test_char_binary_noncontiguous (M, N);
  test_complex_binary_noncontiguous (M, N);
  test_complex_float_binary_noncontiguous (M, N);
  test_complex_long_double_binary_noncontiguous (M, N);

#if GSL_RANGE_CHECK
  gsl_set_error_handler (&my_error_handler);

  test_trap (M, N);
  test_float_trap (M, N);
  test_long_double_trap (M, N);
  test_ulong_trap (M, N);
  test_long_trap (M, N);
  test_uint_trap (M, N);
  test_int_trap (M, N);
  test_ushort_trap (M, N);
  test_short_trap (M, N);
  test_uchar_trap (M, N);
  test_char_trap (M, N);
  test_complex_trap (M, N);
  test_complex_float_trap (M, N);
  test_complex_long_double_trap (M, N);
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
