/* min/test_funcs.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Brian Gough
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

#include <gsl/gsl_math.h>
#include <gsl/gsl_min.h>

#include "test.h"

gsl_function create_function (double (*f)(double, void *)) 
{
  gsl_function F ;
  F.function = f ;
  F.params = 0 ;
  return F ;
}

double
f_cos (double x, void * p)
{
  p = 0;  /* avoid warning about unused parameter */
  return cos(x);
}

/* f(x) = x^4 - 1 */
/* minimum at x = 0 */

double
func1 (double x, void * p)
{
  p = 0;  /* avoid warning about unused parameter */
  return pow (x, 4.0) - 1;
}

/* f(x) = sqrt(|x|) */
/* minimum at x = 0 */

double
func2 (double x, void * p)
{
  p = 0;  /* avoid warning about unused parameter */
  return sqrt(fabs(x));
}


/* f(x) = 1 for x < 1 and -exp(-x) for x >= 1 */
/* minimum at x = 1 */

double
func3 (double x, void * p)
{
  p = 0;  /* avoid warning about unused parameter */

  if (x < 1)
    return 1 ;
  else
    return - exp(-x) ;
}

/* f(x) = x - 30/(1+1e5*(x-0.8)**2) */
/* minimum near x = 0.8 */

double
func4 (double x, void * p)
{
  p = 0;  /* avoid warning about unused parameter */

  return x - 30.0 / (1.0 + 1e5 * pow(x-0.8, 2.0));
}

