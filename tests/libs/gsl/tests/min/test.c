/* min/test.c
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
#include <config.h>
#include <stdlib.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

#include "test.h"
#include "min.h"

/* stopping parameters */

const double EPSABS = 0.001 ;
const double EPSREL = 0.001 ;

const unsigned int MAX_ITERATIONS = 100;

void my_error_handler (const char *reason, const char *file,
                       int line, int err);

#define WITHIN_TOL(a, b, epsrel, epsabs) \
 (fabs((a) - (b)) < (epsrel) * GSL_MIN(fabs(a), fabs(b)) + (epsabs))

int
main (void)
{
  gsl_function F_cos, F_func1, F_func2, F_func3, F_func4;
  
  const gsl_min_fminimizer_type * fminimizer[4] ;
  const gsl_min_fminimizer_type ** T;

  gsl_ieee_env_setup ();

  fminimizer[0] = gsl_min_fminimizer_goldensection;
  fminimizer[1] = gsl_min_fminimizer_brent;
  fminimizer[2] = gsl_min_fminimizer_quad_golden;
  fminimizer[3] = 0;

  F_cos = create_function (f_cos) ;
  F_func1 = create_function (func1) ;
  F_func2 = create_function (func2) ;
  F_func3 = create_function (func3) ;
  F_func4 = create_function (func4) ;

  gsl_set_error_handler (&my_error_handler);

  for (T = fminimizer ; *T != 0 ; T++)
    {
      test_f (*T, "cos(x) [0 (3) 6]", &F_cos, 0.0, 3.0, 6.0, M_PI);
      test_f (*T, "x^4 - 1 [-3 (-1) 17]", &F_func1, -3.0, -1.0, 17.0, 0.0);
      test_f (*T, "sqrt(|x|) [-2 (-1) 1.5]", &F_func2, -2.0, -1.0, 1.5, 0.0);
      test_f (*T, "func3(x) [-2 (3) 4]", &F_func3, -2.0, 3.0, 4.0, 1.0);
      test_f (*T, "func4(x) [0 (0.782) 1]", &F_func4, 0, 0.782, 1.0, 0.8);

      test_f_e (*T, "invalid range check [4, 0]", &F_cos, 4.0, 3.0, 0.0, M_PI);
      test_f_e (*T, "invalid range check [1, 1]", &F_cos, 1.0, 1.0, 1.0, M_PI);
      test_f_e (*T, "invalid range check [-1, 1]", &F_cos, -1.0, 0.0, 1.0, M_PI);
    }
  test_bracket("cos(x) [1,2]",&F_cos,1.0,2.0,15);
  test_bracket("sqrt(|x|) [-1,0]",&F_func2,-1.0,0.0,15);
  test_bracket("sqrt(|x|) [-1,-0.6]",&F_func2,-1.0,-0.6,15);
  test_bracket("sqrt(|x|) [-1,1]",&F_func2,-1.0,1.0,15);

  exit (gsl_test_summary ());
}

void
test_f (const gsl_min_fminimizer_type * T, 
        const char * description, gsl_function *f,
        double lower_bound, double middle, double upper_bound, 
        double correct_minimum)
{
  int status;
  size_t iterations = 0;
  double m, a, b;
  double x_lower, x_upper;
  gsl_min_fminimizer * s;

  x_lower = lower_bound;
  x_upper = upper_bound;

  s = gsl_min_fminimizer_alloc (T) ;
  gsl_min_fminimizer_set (s, f, middle, x_lower, x_upper) ;
  
  do 
    {
      iterations++ ;

      status = gsl_min_fminimizer_iterate (s);

      m = gsl_min_fminimizer_x_minimum(s);
      a = gsl_min_fminimizer_x_lower(s);
      b = gsl_min_fminimizer_x_upper(s);

#ifdef DEBUG
      printf("%.12f %.18f %.12f %.18f %.12f %.18f status=%d\n", 
             a, GSL_FN_EVAL(f, a), m, GSL_FN_EVAL(f, m), b, GSL_FN_EVAL(f, b), status);
#endif

      if (a > b)
        gsl_test (GSL_FAILURE, "interval is invalid (%g,%g)", a, b);

      if (m < a || m > b)
        gsl_test (GSL_FAILURE, "m lies outside interval %g (%g,%g)", m, a, b);

      if (status) break ;

      status = gsl_min_test_interval (a, b, EPSABS, EPSREL);
    }
  while (status == GSL_CONTINUE && iterations < MAX_ITERATIONS);

  gsl_test (status, "%s, %s (%g obs vs %g expected) ", 
            gsl_min_fminimizer_name(s), description, 
            gsl_min_fminimizer_x_minimum(s), correct_minimum);

  /* check the validity of the returned result */

  if (!WITHIN_TOL (m, correct_minimum, EPSREL, EPSABS))
    {
      gsl_test (GSL_FAILURE, "incorrect precision (%g obs vs %g expected)", 
                m, correct_minimum);
    }

  gsl_min_fminimizer_free (s);

}

void
test_f_e (const gsl_min_fminimizer_type * T, 
          const char * description, gsl_function *f,
          double lower_bound, double middle, double upper_bound, 
          double correct_minimum)
{
  int status;
  size_t iterations = 0;
  double x_lower, x_upper;
  double a, b;
  gsl_min_fminimizer * s;

  x_lower = lower_bound;
  x_upper = upper_bound;

  s = gsl_min_fminimizer_alloc (T) ;
  status = gsl_min_fminimizer_set (s, f, middle, x_lower, x_upper) ; 

  if (status != GSL_SUCCESS) 
    {
      gsl_min_fminimizer_free (s) ;
      gsl_test (status == GSL_SUCCESS, "%s, %s", T->name, description);
      return ;
    }

  do 
    {
      iterations++ ;
      gsl_min_fminimizer_iterate (s);
      a = gsl_min_fminimizer_x_lower(s);
      b = gsl_min_fminimizer_x_upper(s);

      status = gsl_min_test_interval (a, b, EPSABS, EPSREL);
    }
  while (status == GSL_CONTINUE && iterations < MAX_ITERATIONS);

  gsl_test (!status, "%s, %s", gsl_min_fminimizer_name(s), description, 
            gsl_min_fminimizer_x_minimum(s) - correct_minimum);

  gsl_min_fminimizer_free (s);
}

void
my_error_handler (const char *reason, const char *file, int line, int err)
{
  if (0)
    printf ("(caught [%s:%d: %s (%d)])\n", file, line, reason, err);
}

int
test_bracket (const char * description,gsl_function *f,double lower_bound, 
              double upper_bound, unsigned int max)
{
  int status;
  double x_lower, x_upper;
  double f_upper,f_lower,f_minimum;
  double x_minimum;

  x_lower=lower_bound;
  x_upper=upper_bound;
  SAFE_FUNC_CALL (f,x_lower,&f_lower);
  SAFE_FUNC_CALL (f,x_upper,&f_upper);
  status=gsl_min_find_bracket(f,&x_minimum,&f_minimum,&x_lower,&f_lower,&x_upper,&f_upper,max);
  gsl_test (status,"%s, interval: [%g,%g], values: (%g,%g), minimum at: %g, value: %g",
            description,x_lower,x_upper,f_lower,f_upper,x_minimum,f_minimum);
  return status;
}



