/* multiroots/test.c
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
#include <stdio.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_multiroots.h>

#include <gsl/gsl_ieee_utils.h>

#include "test_funcs.h"
int test_fdf (const char * desc, gsl_multiroot_function_fdf * function, initpt_function initpt, double factor, const gsl_multiroot_fdfsolver_type * T);
int test_f (const char * desc, gsl_multiroot_function_fdf * fdf, initpt_function initpt, double factor, const gsl_multiroot_fsolver_type * T);


int 
main (void)
{
  const gsl_multiroot_fsolver_type * fsolvers[5] ;
  const gsl_multiroot_fsolver_type ** T1 ;

  const gsl_multiroot_fdfsolver_type * fdfsolvers[5] ;
  const gsl_multiroot_fdfsolver_type ** T2 ;

  double f;

  fsolvers[0] = gsl_multiroot_fsolver_dnewton;
  fsolvers[1] = gsl_multiroot_fsolver_broyden;
  fsolvers[2] = gsl_multiroot_fsolver_hybrid;
  fsolvers[3] = gsl_multiroot_fsolver_hybrids;
  fsolvers[4] = 0;

  fdfsolvers[0] = gsl_multiroot_fdfsolver_newton;
  fdfsolvers[1] = gsl_multiroot_fdfsolver_gnewton;
  fdfsolvers[2] = gsl_multiroot_fdfsolver_hybridj;
  fdfsolvers[3] = gsl_multiroot_fdfsolver_hybridsj;
  fdfsolvers[4] = 0;

  gsl_ieee_env_setup();


  f = 1.0 ;
  
  T1 = fsolvers ;
  
  while (*T1 != 0) 
    {
      test_f ("Rosenbrock", &rosenbrock, rosenbrock_initpt, f, *T1);
      test_f ("Roth", &roth, roth_initpt, f, *T1);
      test_f ("Powell badly scaled", &powellscal, powellscal_initpt, f, *T1);
      test_f ("Brown badly scaled", &brownscal, brownscal_initpt, f, *T1);
      test_f ("Powell singular", &powellsing, powellsing_initpt, f, *T1);
      test_f ("Wood", &wood, wood_initpt, f, *T1);
      test_f ("Helical", &helical, helical_initpt, f, *T1);
      test_f ("Discrete BVP", &dbv, dbv_initpt, f, *T1);
      test_f ("Trig", &trig, trig_initpt, f, *T1);
      T1++;
    }
  
  T2 = fdfsolvers ;
  
  while (*T2 != 0) 
    {
      test_fdf ("Rosenbrock", &rosenbrock, rosenbrock_initpt, f, *T2);
      test_fdf ("Roth", &roth, roth_initpt, f, *T2);
      test_fdf ("Powell badly scaled", &powellscal, powellscal_initpt, f, *T2);
      test_fdf ("Brown badly scaled", &brownscal, brownscal_initpt, f, *T2);
      test_fdf ("Powell singular", &powellsing, powellsing_initpt, f, *T2);
      test_fdf ("Wood", &wood, wood_initpt, f, *T2);
      test_fdf ("Helical", &helical, helical_initpt, f, *T2);
      test_fdf ("Discrete BVP", &dbv, dbv_initpt, f, *T2);
      test_fdf ("Trig", &trig, trig_initpt, f, *T2);
      T2++;
    }

  exit (gsl_test_summary ());
}

void scale (gsl_vector * x, double factor);

void
scale (gsl_vector * x, double factor)
{
  size_t i, n = x->size;

  if (gsl_vector_isnull(x))
    {
      for (i = 0; i < n; i++)
        {
          gsl_vector_set (x, i, factor);
        }
    }
  else
    {
      for (i = 0; i < n; i++)
        {
          double xi = gsl_vector_get(x, i);
          gsl_vector_set(x, i, factor * xi);
        }
    } 
}

int
test_fdf (const char * desc, gsl_multiroot_function_fdf * function, 
          initpt_function initpt, double factor,
          const gsl_multiroot_fdfsolver_type * T)
{
  int status;
  double residual = 0;
  size_t i, n = function->n, iter = 0;
  
  gsl_vector *x = gsl_vector_alloc (n);
  gsl_matrix *J = gsl_matrix_alloc (n, n);

  gsl_multiroot_fdfsolver *s;

  (*initpt) (x);

  if (factor != 1.0) scale(x, factor);

  s = gsl_multiroot_fdfsolver_alloc (T, n);
  gsl_multiroot_fdfsolver_set (s, function, x);
 
  do
    {
      iter++;
      status = gsl_multiroot_fdfsolver_iterate (s);
      
      if (status)
        break ;

      status = gsl_multiroot_test_residual (s->f, 0.0000001);
    }
  while (status == GSL_CONTINUE && iter < 1000);

#ifdef DEBUG
  printf("x "); gsl_vector_fprintf (stdout, s->x, "%g"); printf("\n");
  printf("f "); gsl_vector_fprintf (stdout, s->f, "%g"); printf("\n");
#endif


#ifdef TEST_JACOBIAN
 {
    double r,sum; size_t j;

    gsl_multiroot_function f1 ;
    f1.f = function->f ;
    f1.n = function->n ;
    f1.params = function->params ;
    
    gsl_multiroot_fdjacobian (&f1, s->x, s->f, GSL_SQRT_DBL_EPSILON, J);
  
    /* compare J and s->J */
    
    r=0;sum=0;
    for (i = 0; i < n; i++)
      for (j = 0; j< n ; j++)
        {
          double u = gsl_matrix_get(J,i,j);
          double su = gsl_matrix_get(s->J, i, j);
          r = fabs(u - su)/(1e-6 + 1e-6 * fabs(u)); sum+=r;
          if (fabs(u - su) > 1e-6 + 1e-6 * fabs(u))
            printf("broken jacobian %g\n", r);
        }
    printf("avg r = %g\n", sum/(n*n));
  }
#endif

  for (i = 0; i < n ; i++)
    {
      residual += fabs(gsl_vector_get(s->f, i));
    }

  gsl_multiroot_fdfsolver_free (s);
  gsl_matrix_free(J);
  gsl_vector_free(x);

  gsl_test(status, "%s on %s (%g), %u iterations, residual = %.2g", T->name, desc, factor, iter, residual);

  return status;
}


int
test_f (const char * desc, gsl_multiroot_function_fdf * fdf, 
        initpt_function initpt, double factor,
        const gsl_multiroot_fsolver_type * T)
{
  int status;
  size_t i, n = fdf->n, iter = 0;
  double residual = 0;

  gsl_vector *x;

  gsl_multiroot_fsolver *s;
  gsl_multiroot_function function;

  function.f = fdf->f;
  function.params = fdf->params;
  function.n = n ;

  x = gsl_vector_alloc (n);

  (*initpt) (x);

  if (factor != 1.0) scale(x, factor);

  s = gsl_multiroot_fsolver_alloc (T, n);
  gsl_multiroot_fsolver_set (s, &function, x);

/*   printf("x "); gsl_vector_fprintf (stdout, s->x, "%g"); printf("\n"); */
/*   printf("f "); gsl_vector_fprintf (stdout, s->f, "%g"); printf("\n"); */

  do
    {
      iter++;
      status = gsl_multiroot_fsolver_iterate (s);
      
      if (status)
        break ;

      status = gsl_multiroot_test_residual (s->f, 0.0000001);
    }
  while (status == GSL_CONTINUE && iter < 1000);

#ifdef DEBUG
  printf("x "); gsl_vector_fprintf (stdout, s->x, "%g"); printf("\n");
  printf("f "); gsl_vector_fprintf (stdout, s->f, "%g"); printf("\n");
#endif

  for (i = 0; i < n ; i++)
    {
      residual += fabs(gsl_vector_get(s->f, i));
    }

  gsl_multiroot_fsolver_free (s);
  gsl_vector_free(x);

  gsl_test(status, "%s on %s (%g), %u iterations, residual = %.2g", T->name, desc, factor, iter, residual);

  return status;
}
