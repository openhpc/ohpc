/* roots/test.h
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Reid Priedhorsky, Brian Gough
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

gsl_function create_function (double (*f)(double, void *)) ;
gsl_function_fdf create_fdf (double (*f)(double, void *),
                             double (*df)(double, void *),
                             void (*fdf)(double, void *, double *, double *));

void
  test_macros (void);

void
  test_roots (void);

void
  test_poly (void);

void
test_f (const gsl_root_fsolver_type * T, 
        const char * description, gsl_function *f,
        double lower_bound, double upper_bound, double correct_root);

void
test_f_e (const gsl_root_fsolver_type * T, const char * description, 
          gsl_function *f,
          double lower_bound, double upper_bound, double correct_root);

void
test_fdf (const gsl_root_fdfsolver_type * T, const char * description, 
          gsl_function_fdf *fdf, double root, double correct_root);

void
test_fdf_e (const gsl_root_fdfsolver_type * T, const char * description, 
            gsl_function_fdf *fdf, double root, double correct_root);


void
  usage (void);

void
  error_handler (const char *reason, const char *file, int line);

double
  func1 (double x, void * p);

double
  func1_df (double x, void * p);

void
  func1_fdf (double x, void * p, double *y, double *yprime);

double
  func2 (double x, void * p);

double
  func2_df (double x, void * p);

void
  func2_fdf (double x, void * p, double *y, double *yprime);

double
  func3 (double x, void * p);

double
  func3_df (double x, void * p);

void
  func3_fdf (double x, void * p, double *y, double *yprime);

double
  func4 (double x, void * p);

double
  func4_df (double x, void * p);

void
  func4_fdf (double x, void * p, double *y, double *yprime);

double
  func5 (double x, void * p);

double
  func5_df (double x, void * p);

void
  func5_fdf (double x, void * p, double *y, double *yprime);

double
  func6 (double x, void * p);

double
  func6_df (double x, void * p);

void
  func6_fdf (double x, void * p, double *y, double *yprime);

double
  sin_f (double x, void * p);

double
  sin_df (double x, void * p);

void
  sin_fdf (double x, void * p, double *y, double *yprime);

double
  cos_f (double x, void * p);

double
  cos_df (double x, void * p);

void
  cos_fdf (double x, void * p, double *y, double *yprime);
