/* ode-initval/test_odeiv.c
 * 
 * Copyright (C) 2004, 2009 Tuomo Keskitalo
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

/* Some functions and tests based on test.c by G. Jungman.
*/

#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_odeiv.h>
#include "odeiv_util.h"

/* Maximum number of ODE equations */
#define MAXEQ 4

/* RHS for f=2. Solution y = 2 * t + t0 */

int
rhs_linear (double t, const double y[], double f[], void *params)
{
  f[0] = 2.0;

  return GSL_SUCCESS;
}

int
jac_linear (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  dfdy[0] = 0.0;
  dfdt[0] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_lin = {
  rhs_linear,
  jac_linear,
  1,
  0
};

/* RHS for f=y. Equals y=exp(t) with initial value y(0)=1.0 */

int
rhs_exp (double t, const double y[], double f[], void *params)
{
  f[0] = y[0];

  return GSL_SUCCESS;
}

int
jac_exp (double t, const double y[], double *dfdy, double dfdt[],
         void *params)
{
  dfdy[0] = y[0];
  dfdt[0] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_exp = {
  rhs_exp,
  jac_exp,
  1,
  0
};

/* RHS for f0 = -y1, f1 = y0
   equals y = [cos(t), sin(t)] with initial values [1, 0]
*/

int
rhs_sin (double t, const double y[], double f[], void *params)
{
  f[0] = -y[1];
  f[1] = y[0];

  return GSL_SUCCESS;
}

int
jac_sin (double t, const double y[], double *dfdy, double dfdt[],
         void *params)
{
  dfdy[0] = 0.0;
  dfdy[1] = -1.0;
  dfdy[2] = 1.0;
  dfdy[3] = 0.0;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_sin = {
  rhs_sin,
  jac_sin,
  2,
  0
};

/*  Sine/cosine with random failures */

static int rhs_xsin_reset = 0;
static int jac_xsin_reset = 0;

int
rhs_xsin (double t, const double y[], double f[], void *params)
{
  static int n = 0, m = 0;
  
  if (rhs_xsin_reset) { rhs_xsin_reset = 0; n = 0; m = 1;}
  n++;

  if (n >= m) { 
    m = n * 1.3;
    return GSL_EFAILED; 
  } ;

  if (n > 40 && n < 65) {
    f[0] = GSL_NAN;
    f[1] = GSL_NAN;
    return GSL_EFAILED;
  }

  f[0] = -y[1];
  f[1] = y[0];

  return GSL_SUCCESS;
}

int
jac_xsin (double t, const double y[], double *dfdy, double dfdt[],
         void *params)
{
  static int n = 0;

  if (jac_xsin_reset) { jac_xsin_reset = 0; n = 0; }

  n++; 

  if (n > 50 && n < 55) {
    dfdy[0] = GSL_NAN;
    dfdy[1] = GSL_NAN;
    dfdy[2] = GSL_NAN;
    dfdy[3] = GSL_NAN;
    
    dfdt[0] = GSL_NAN;
    dfdt[1] = GSL_NAN;
    return GSL_EFAILED;
  }

  dfdy[0] = 0.0;
  dfdy[1] = -1.0;
  dfdy[2] = 1.0;
  dfdy[3] = 0.0;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_xsin = {
  rhs_xsin,
  jac_xsin,
  2,
  0
};


/* RHS for classic stiff example
   dy0 / dt =  998 * y0 + 1998 * y1    y0(0) = 1.0
   dy1 / dt = -999 * y0 - 1999 * y1    y1(0) = 0.0

   solution is
   y0 = 2 * exp(-t) - exp(-1000 * t)
   y1 = - exp(-t) + exp(-1000 * t)
*/

int
rhs_stiff (double t, const double y[], double f[], void *params)
{
  f[0] = 998.0 * y[0] + 1998.0 * y[1];
  f[1] = -999.0 * y[0] - 1999.0 * y[1];

  return GSL_SUCCESS;
}

int
jac_stiff (double t, const double y[], double *dfdy, double dfdt[],
           void *params)
{
  dfdy[0] = 998.0;
  dfdy[1] = 1998.0;
  dfdy[2] = -999.0;
  dfdy[3] = -1999.0;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_stiff = {
  rhs_stiff,
  jac_stiff,
  2,
  0
};

/* van Der Pol oscillator:
   f0 = y1                           y0(0) = 1.0
   f1 = -y0 + mu * y1 * (1 - y0^2)   y1(0) = 0.0
*/

int
rhs_vanderpol (double t, const double y[], double f[], void *params)
{
  const double mu = 10.0;

  f[0] = y[1];
  f[1] = -y[0] + mu * y[1] * (1.0 - y[0]*y[0]); 

  return GSL_SUCCESS;
}

int
jac_vanderpol (double t, const double y[], double *dfdy, double dfdt[],
	       void *params)
{
  const double mu = 10.0;

  dfdy[0] = 0.0;
  dfdy[1] = 1.0;
  dfdy[2] = -2.0 * mu * y[0] * y[1] - 1.0;
  dfdy[3] = mu * (1.0 - y[0] * y[0]);

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_vanderpol = {
  rhs_vanderpol,
  jac_vanderpol,
  2,
  0
};

/* The Oregonator - chemical Belusov-Zhabotinskii reaction 
   y0(0) = 1.0, y1(0) = 2.0, y2(0) = 3.0
*/

int
rhs_oregonator (double t, const double y[], double f[], void *params)
{
  const double c1=77.27;
  const double c2=8.375e-6;
  const double c3=0.161;

  f[0] = c1 * (y[1] + y[0] * (1 - c2 * y[0] - y[1]));
  f[1] = 1/c1 * (y[2] - y[1] * (1 + y[0]));
  f[2] = c3 * (y[0] - y[2]);

  return GSL_SUCCESS;
}

int
jac_oregonator (double t, const double y[], double *dfdy, double dfdt[],
		void *params)
{
  const double c1=77.27;
  const double c2=8.375e-6;
  const double c3=0.161;

  dfdy[0] = c1 * (1 - 2 * c2 * y[0] - y[1]);
  dfdy[1] = c1 * (1 - y[0]);
  dfdy[2] = 0.0;

  dfdy[3] = 1/c1 * (-y[1]);
  dfdy[4] = 1/c1 * (-1 - y[0]);
  dfdy[5] = 1/c1;

  dfdy[6] = c3;
  dfdy[7] = 0.0;
  dfdy[8] = -c3;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;
  dfdt[2] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_oregonator = {
  rhs_oregonator,
  jac_oregonator,
  3,
  0
};

/* Volterra-Lotka predator-prey model

   f0 = (a - b * y1) * y0     y0(0) = 3.0
   f1 = (-c + d * y0) * y1    y1(0) = 1.0
 */

int
rhs_vl (double t, const double y[], double f[], void *params)
{
  const double a = 1.0;
  const double b = 1.0;
  const double c = 1.0;
  const double d = 1.0;
    
  f[0] = (a - b * y[1]) * y[0];
  f[1] = (-c + d * y[0]) * y[1];

  return GSL_SUCCESS;
}

int
jac_vl (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  const double a = 1.0;
  const double b = 1.0;
  const double c = 1.0;
  const double d = 1.0;

  dfdy[0] = a - b * y[1];
  dfdy[1] = -b * y[0];
  dfdy[2] = d * y[1];
  dfdy[3] = -c + d * y[0];

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_vl = {
  rhs_vl,
  jac_vl,
  2,
  0
};

/* Stiff trigonometric example 

   f0 = -50 * (y0 - cos(t))    y0(0) = 0.0
 */

int
rhs_stifftrig (double t, const double y[], double f[], void *params)
{
  f[0] = -50 * (y[0] - cos(t));

  return GSL_SUCCESS;
}

int
jac_stifftrig (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  dfdy[0] = -50;

  dfdt[0] = -50 * sin(t);

  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_stifftrig = {
  rhs_stifftrig,
  jac_stifftrig,
  1,
  0
};

/* E5 - a stiff badly scaled chemical problem by Enright, Hull &
   Lindberg (1975): Comparing numerical methods for stiff systems of
   ODEs. BIT, vol. 15, pp. 10-48.

   f0 = -a * y0 - b * y0 * y2                            y0(0) = 1.76e-3
   f1 = a * y0 - m * c * y1 * y2                         y1(0) = 0.0
   f2 = a * y0 - b * y0 * y2 - m * c * y1 * y2 + c * y3  y2(0) = 0.0
   f3 = b * y0 * y2 - c * y3                             y3(0) = 0.0
 */

int
rhs_e5 (double t, const double y[], double f[], void *params)
{
  const double a = 7.89e-10;
  const double b = 1.1e7;
  const double c = 1.13e3;
  const double m = 1.0e6;

  f[0] = -a * y[0] - b * y[0] * y[2];
  f[1] = a * y[0] - m * c * y[1] * y[2];
  f[3] = b * y[0] * y[2] - c * y[3];
  f[2] = f[1] - f[3];

  return GSL_SUCCESS;
}

int
jac_e5 (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  const double a = 7.89e-10;
  const double b = 1.1e7;
  const double c = 1.13e3;
  const double m = 1.0e6;

  dfdy[0] = -a - b * y[2];
  dfdy[1] = 0.0;
  dfdy[2] = -b * y[0];
  dfdy[3] = 0.0;

  dfdy[4] = a;
  dfdy[5] = -m * c * y[2];
  dfdy[6] = -m * c * y[1];
  dfdy[7] = 0.0;

  dfdy[8] = a - b * y[2];
  dfdy[9] = -m * c * y[2];
  dfdy[10] = -b * y[0] - m * c * y[1];
  dfdy[11] = c;

  dfdy[12] = b * y[2];
  dfdy[13] = 0.0;
  dfdy[14] = b * y[0];
  dfdy[15] = -c;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;
  dfdt[2] = 0.0;
  dfdt[3] = 0.0;
  
  return GSL_SUCCESS;
}

gsl_odeiv_system rhs_func_e5 = {
  rhs_e5,
  jac_e5,
  4,
  0
};

void
test_odeiv_stepper (const gsl_odeiv_step_type *T, const gsl_odeiv_system *sys,
		    const double h, const double t, const char desc[],
		    const double ystart[], const double yfin[], 
		    const double relerr)
{
  /* tests stepper T with one fixed length step advance of system sys
     and compares with given values yfin
  */

  double y[MAXEQ] = {0.0};
  double yerr[MAXEQ] = {0.0};
  size_t ne = sys->dimension;
  size_t i;

  gsl_odeiv_step *step = gsl_odeiv_step_alloc (T, ne);

  DBL_MEMCPY (y, ystart, MAXEQ);

  {
    int s = gsl_odeiv_step_apply (step, t, h, y, yerr, 0, 0, sys);
    if (s != GSL_SUCCESS)
      {
	gsl_test(s, "test_odeiv_stepper: %s step_apply returned %d", desc, s);
      }
  }
  
  for (i = 0; i < ne; i++)
    { 
      gsl_test_rel (y[i], yfin[i], relerr, 
		    "%s %s step(%d)",
		    gsl_odeiv_step_name (step), desc,i);
    }

  gsl_odeiv_step_free (step);
}

void
test_stepper (const gsl_odeiv_step_type *T) 
{
  /* Tests stepper T with a step of selected systems */

  double y[MAXEQ] = {0.0};
  double yfin[MAXEQ] = {0.0};

  /* Step length */
  double h;

  /* Required tolerance */
  double err_target;

  /* linear */
  h = 1e-1;
  err_target = 1e-10;
  y[0] = 0.58;
  yfin[0] = y[0] + 2 * h;
  test_odeiv_stepper (T, &rhs_func_lin, h, 0.0, "linear",
		      y, yfin, err_target);
  
  /* exponential */
  h = 1e-4;
  err_target = 1e-8;
  y[0] = exp(2.7);
  yfin[0] = exp(2.7 + h);
  test_odeiv_stepper (T, &rhs_func_exp, h, 2.7, "exponential",
		      y, yfin, err_target);
  /* cosine-sine */
  h = 1e-3;
  err_target = 1e-6;
  y[0] = cos(1.2);
  y[1] = sin(1.2);
  yfin[0] = cos(1.2 + h);
  yfin[1] = sin(1.2 + h);
  test_odeiv_stepper (T, &rhs_func_sin, h, 1.2, "cosine-sine",
		      y, yfin, err_target);

  /* classic stiff */
  h = 1e-7;
  err_target = 1e-4;
  y[0] = 1.0;
  y[1] = 0.0;

  {
    const double e1 = exp (-h);
    const double e2 = exp (-1000.0 * h);
    yfin[0] = 2.0 * e1 - e2;
    yfin[1] = -e1 + e2;
  }

  test_odeiv_stepper (T, &rhs_func_stiff, h, 0.0, "classic_stiff",
		      y, yfin, err_target);  
}

void
test_evolve_system (const gsl_odeiv_step_type * T,
                    const gsl_odeiv_system * sys,
                    double t0, double t1, double hstart,
                    double y[], double yfin[],
                    double err_target, const char *desc)
{
  /* Tests system sys with stepper T. Step length is controlled by
     error estimation from the stepper.
  */     
  
  int steps = 0;
  size_t i;

  double t = t0;
  double h = hstart;

  /* Tolerance factor in testing errors */
  const double factor = 10;

  gsl_odeiv_step * step = gsl_odeiv_step_alloc (T, sys->dimension);

  gsl_odeiv_control *c =
    gsl_odeiv_control_standard_new (err_target, err_target, 1.0, 0.0);

  gsl_odeiv_evolve *e = gsl_odeiv_evolve_alloc (sys->dimension);

  double * y_orig = malloc (sys->dimension * sizeof(double));

  while (t < t1)
    {
      double t_orig = t;
      int s;
      memcpy (y_orig, y, sys->dimension * sizeof(double));
      s= gsl_odeiv_evolve_apply (e, c, step, sys, &t, t1, &h, y);

      if (s != GSL_SUCCESS)
	{
          /* check that t and y are unchanged */
          gsl_test_abs(t, t_orig, 0.0, "%s, t must be restored on failure",
                       gsl_odeiv_step_name (step));

          for (i = 0; i < sys->dimension; i++)
            {
              gsl_test_abs (y[i], y_orig[i], 0.0, 
                            "%s, y must be restored on failure",
                            gsl_odeiv_step_name (step), desc, i);
            }
          
          if (sys != &rhs_func_xsin) {
            /* apart from xsin, other functions should not return errors */
            gsl_test(s, "%s evolve_apply returned %d",
                     gsl_odeiv_step_name (step), s);
            break;
          }
	}

      if (steps > 100000)
	{
	  gsl_test(GSL_EFAILED, 
		   "%s evolve_apply reached maxiter",
		   gsl_odeiv_step_name (step));
	  break;
	}

      steps++;
    }

  /* err_target is target error of one step. Test if stepper has made
     larger error than (tolerance factor times) the number of steps
     times the err_target */

  for (i = 0; i < sys->dimension; i++)
    {
      gsl_test_abs (y[i], yfin[i], factor * e->count * err_target,
		    "%s %s evolve(%d)",
		    gsl_odeiv_step_name (step), desc, i);
    }

  free (y_orig);
  gsl_odeiv_evolve_free (e);
  gsl_odeiv_control_free (c);
  gsl_odeiv_step_free (step);
}

int
sys_driver (const gsl_odeiv_step_type * T,
	    const gsl_odeiv_system * sys,
	    double t0, double t1, double hstart,
	    double y[], double epsabs, double epsrel,
	    const char desc[])
{
  /* This function evolves a system sys with stepper T from t0 to t1.
     Step length is varied via error control with possibly different
     absolute and relative error tolerances.
  */
  
  int s = 0;
  int steps = 0;

  double t = t0;
  double h = hstart;

  gsl_odeiv_step * step = gsl_odeiv_step_alloc (T, sys->dimension);

  gsl_odeiv_control *c =
    gsl_odeiv_control_standard_new (epsabs, epsrel, 1.0, 0.0);
  gsl_odeiv_evolve *e = gsl_odeiv_evolve_alloc (sys->dimension);

  while (t < t1)
    {
      s = gsl_odeiv_evolve_apply (e, c, step, sys, &t, t1, &h, y);

      if (s != GSL_SUCCESS) 
	{
	  gsl_test(s, "sys_driver: %s evolve_apply returned %d",
		   gsl_odeiv_step_name (step), s);
	  break;
	}

      if (steps > 1e7)
	{
	  gsl_test(GSL_EMAXITER, 
		   "sys_driver: %s evolve_apply reached maxiter at t=%g",
		   gsl_odeiv_step_name (step), t);
	  s = GSL_EMAXITER;
	  break;
	}

      steps++;
    }

  gsl_test(s, "%s %s [%g,%g], %d steps completed", 
	   gsl_odeiv_step_name (step), desc, t0, t1, steps);

  gsl_odeiv_evolve_free (e);
  gsl_odeiv_control_free (c);
  gsl_odeiv_step_free (step);

  return s;
}

void
test_compare_vanderpol (void)
{
  /* Compares output of van Der Pol oscillator with several steppers */
  
  /* system dimension */
  const size_t sd = 2;
  
  const gsl_odeiv_step_type *steppers[20];
  const gsl_odeiv_step_type **T;

  /* Required error tolerance for each stepper. */
  double err_target[20];

  /* number of ODE solvers */
  const size_t ns = 11;

  /* initial values for each ode-solver */
  double y[11][2];
  double *yp = &y[0][0];

  size_t i, j, k;
  int status = 0;

  /* Parameters for the problem and stepper  */
  const double start = 0.0;
  const double end = 100.0;
  const double epsabs = 1e-8;
  const double epsrel = 1e-8;
  const double initstepsize = 1e-5;

  /* Initialize */

  steppers[0] = gsl_odeiv_step_rk2;
  err_target[0] = 1e-6;
  steppers[1] = gsl_odeiv_step_rk4;
  err_target[1] = 1e-6;
  steppers[2] = gsl_odeiv_step_rkf45;
  err_target[2] = 1e-6;
  steppers[3] = gsl_odeiv_step_rkck;
  err_target[3] = 1e-6;
  steppers[4] = gsl_odeiv_step_rk8pd;
  err_target[4] = 1e-6;
  steppers[5] = gsl_odeiv_step_rk2imp;
  err_target[5] = 1e-5;
  steppers[6] = gsl_odeiv_step_rk2simp;
  err_target[6] = 1e-5;
  steppers[7] = gsl_odeiv_step_rk4imp;
  err_target[7] = 1e-6;
  steppers[8] = gsl_odeiv_step_bsimp;
  err_target[8] = 1e-7;
  steppers[9] = gsl_odeiv_step_gear1;
  err_target[9] = 1e-2;
  steppers[10] = gsl_odeiv_step_gear2;
  err_target[10] = 1e-6;
  steppers[11] = 0;

  T = steppers;

  for (i = 0; i < ns; i++) 
    {
      y[i][0] = 1.0;
      y[i][1] = 0.0;
    }
  
  /* Call each solver for the problem */

  i = 0;
  while (*T != 0)
    {
      {
	int s = sys_driver (*T, &rhs_func_vanderpol,
			    start, end, initstepsize, &yp[i], 
			    epsabs, epsrel, "vanderpol");
	if (s != GSL_SUCCESS)
	  {
	    status++;
	  }
      }
      
      T++;
      i += sd;
    }

  if (status != GSL_SUCCESS)
    {
      return;
    }

  /* Compare results */
      
  T = steppers;

  for (i = 0; i < ns; i++)
    for (j = i+1; j < ns; j++)
      for (k = 0; k < sd; k++)
	{
	  const double val1 = yp[sd * i + k];
	  const double val2 = yp[sd * j + k];
	  gsl_test_abs (val1, val2, 
			( GSL_MAX(err_target[i], err_target[j]) ),
			"%s/%s vanderpol",
			T[i]->name, T[j]->name);
	}

}

void
test_compare_oregonator (void)
{
  /* Compares output of the Oregonator with several steppers */
  
  /* system dimension */
  const size_t sd = 3;
  
  const gsl_odeiv_step_type *steppers[20];
  const gsl_odeiv_step_type **T;

  /* Required error tolerance for each stepper. */
  double err_target[20];

  /* number of ODE solvers */
  const size_t ns = 2;

  /* initial values for each ode-solver */
  double y[2][3];
  double *yp = &y[0][0];

  size_t i, j, k;
  int status = 0;
  
  /* Parameters for the problem and stepper  */
  const double start = 0.0;
  const double end = 360.0;
  const double epsabs = 1e-8;
  const double epsrel = 1e-8;
  const double initstepsize = 1e-5;

  /* Initialize */

  steppers[0] = gsl_odeiv_step_rk2simp;
  err_target[0] = 1e-6;
  steppers[1] = gsl_odeiv_step_bsimp;
  err_target[1] = 1e-6;
  steppers[2] = 0;

  T = steppers;

  for (i = 0; i < ns; i++) 
    {
      y[i][0] = 1.0;
      y[i][1] = 2.0;
      y[i][2] = 3.0;
    }
  
  /* Call each solver for the problem */

  i = 0;
  while (*T != 0)
    {
      {
	int s = sys_driver (*T, &rhs_func_oregonator,
			    start, end, initstepsize, &yp[i], 
			    epsabs, epsrel, "oregonator");

	if (s != GSL_SUCCESS)
	  {
	    status++;
	  }
      }

      T++;
      i += sd;
    }

  if (status != GSL_SUCCESS)
    {
      return;
    }
      
  /* Compare results */
  
  T = steppers;

  for (i = 0; i < ns; i++)
    for (j = i+1; j < ns; j++)
      for (k = 0; k < sd; k++)
	{
	  const double val1 = yp[sd * i + k];
	  const double val2 = yp[sd * j + k];
	  gsl_test_rel (val1, val2, 
			( GSL_MAX(err_target[i], err_target[j]) ),
			"%s/%s oregonator",
			T[i]->name, T[j]->name);
	}

}

void
test_evolve_linear (const gsl_odeiv_step_type * T, double h, double err)
{
  double y[1];
  double yfin[1];

  y[0] = 1.0;
  yfin[0] = 9.0;
  test_evolve_system (T, &rhs_func_lin, 0.0, 4.0, h, y, yfin, err,
		      "linear[0,4]");
}

void
test_evolve_exp (const gsl_odeiv_step_type * T, double h, double err)
{
  double y[1];
  double yfin[1];

  y[0] = 1.0;
  yfin[0] = exp (2.0);
  test_evolve_system (T, &rhs_func_exp, 0.0, 2.0, h, y, yfin, err,
		      "exp[0,2]");
}

void
test_evolve_sin (const gsl_odeiv_step_type * T, double h, double err)
{
  double y[2];
  double yfin[2];

  y[0] = 1.0;
  y[1] = 0.0;
  yfin[0] = cos (2.0);
  yfin[1] = sin (2.0);
  test_evolve_system (T, &rhs_func_sin, 0.0, 2.0, h, y, yfin, err,
		      "sine[0,2]");
}

void
test_evolve_xsin (const gsl_odeiv_step_type * T, double h, double err)
{
  double y[2];
  double yfin[2];
  y[0] = 1.0;
  y[1] = 0.0;
  yfin[0] = cos (2.0);
  yfin[1] = sin (2.0);
  rhs_xsin_reset = 1;
  jac_xsin_reset = 1;
  test_evolve_system (T, &rhs_func_xsin, 0.0, 2.0, h, y, yfin, err,
                      "sine[0,2] w/errors");
}


void
test_evolve_stiff1 (const gsl_odeiv_step_type * T, double h, double err)
{
  double y[2];
  double yfin[2];

  y[0] = 1.0;
  y[1] = 0.0;
  {
    double arg = 1.0;
    double e1 = exp (-arg);
    double e2 = exp (-1000.0 * arg);
    yfin[0] = 2.0 * e1 - e2;
    yfin[1] = -e1 + e2;
  }
  test_evolve_system (T, &rhs_func_stiff, 0.0, 1.0, h, y, yfin, err,
                      "stiff[0,1]");
}

void
test_evolve_stiff5 (const gsl_odeiv_step_type * T, double h, double err)
{
  double y[2];
  double yfin[2];

  y[0] = 1.0;
  y[1] = 0.0;
  {
    double arg = 5.0;
    double e1 = exp (-arg);
    double e2 = exp (-1000.0 * arg);
    yfin[0] = 2.0 * e1 - e2;
    yfin[1] = -e1 + e2;
  }
  test_evolve_system (T, &rhs_func_stiff, 0.0, 5.0, h, y, yfin, err,
                      "stiff[0,5]");
}

/* Test cases from Frank Reininghaus <frank78ac@googlemail.com> */

int rhs_stepfn (double t, const double * y, double * dydt, void * params) {
  if (t >= 1.0)
    dydt [0] = 1;
  else
    dydt [0] = 0;

  return GSL_SUCCESS;
}

void test_stepfn (void) {
  /* infinite loop for epsabs = 1e-18, but not for 1e-17 */
  double epsabs = 1e-18;
  double epsrel = 1e-6;

  const gsl_odeiv_step_type * T = gsl_odeiv_step_rk2;
  gsl_odeiv_step * s = gsl_odeiv_step_alloc (T, 1);
  gsl_odeiv_control * c = gsl_odeiv_control_y_new (epsabs, epsrel);
  gsl_odeiv_evolve * e = gsl_odeiv_evolve_alloc (1);
  gsl_odeiv_system sys = {rhs_stepfn, 0, 1, 0};
     
  double t = 0.0;
  double h = 1e-6;
  double y = 0.0;
  int i = 0;
  int status;

  while (t < 2.0 && i < 1000000) {
    status = gsl_odeiv_evolve_apply (e, c, s, &sys, &t, 2, &h, &y);
#ifdef DEBUG
    printf("i=%d status=%d t=%g h=%g y=%g\n", i, status, t, h, y);
#endif
    if (status != GSL_SUCCESS)
      break;
    
    i++;
  }

  gsl_test_abs(t, 2.0, 1e-16, "evolve step function, t (stepfn/rk2)");
  gsl_test_rel(y, 1.0, epsrel, "evolve step function, y (stepfn/rk2)");
       
  gsl_odeiv_evolve_free (e);
  gsl_odeiv_control_free (c);
  gsl_odeiv_step_free (s);
}

int rhs_stepfn2 (double t, const double * y, double * dydt, void * params) {
  if (t >= 0.0)
    dydt [0] = 1e300;
  else
    dydt [0] = 0;

  return GSL_SUCCESS;
}

void test_stepfn2 (void) {
  /* infinite loop for epsabs = 1e-25, but not for 1e-24 */
  double epsabs = 1e-25;
  double epsrel = 1e-6;

  const gsl_odeiv_step_type * T = gsl_odeiv_step_rk2;
  gsl_odeiv_step * s = gsl_odeiv_step_alloc (T, 1);
  gsl_odeiv_control * c = gsl_odeiv_control_y_new (epsabs, epsrel);
  gsl_odeiv_evolve * e = gsl_odeiv_evolve_alloc (1);
  gsl_odeiv_system sys = {rhs_stepfn2, 0, 1, 0};
     
  double t = -1.0;
  double h = 1e-6;
  double y = 0.0;

  int i = 0;
  int status;

  while (t < 1.0 && i < 10000) {
    status = gsl_odeiv_evolve_apply (e, c, s, &sys, &t, 1.0, &h, &y);
#ifdef DEBUG
    printf("i=%d status=%d t=%g h=%g y=%g\n", i, status, t, h, y);
#endif
    if (status != GSL_SUCCESS)
      break;

    i++;
  }

  gsl_test_abs(t, 1.0, 1e-16, "evolve big step function, t (stepfn2/rk2)");
  gsl_test_rel(y, 1e300, epsrel, "evolve big step function, y (stepfn2/rk2)");
     
  gsl_odeiv_evolve_free (e);
  gsl_odeiv_control_free (c);
  gsl_odeiv_step_free (s);
}

int rhs_stepfn3 (double t, const double * y, double * dydt, void * params) {

  static int calls = 0;

  if (t >= 0.0)
    dydt [0] = 1e300;
  else
    dydt [0] = 0;

  calls++;

  return (calls < 100000) ? GSL_SUCCESS : -999;
}


void test_stepfn3 (void) {
  /* infinite loop for epsabs = 1e-26, but not for 1e-25 */
  double epsabs = 1e-26;
  double epsrel = 1e-6;

  const gsl_odeiv_step_type * T = gsl_odeiv_step_rkf45;
  gsl_odeiv_step * s = gsl_odeiv_step_alloc (T, 1);
  gsl_odeiv_control * c = gsl_odeiv_control_y_new (epsabs, epsrel);
  gsl_odeiv_evolve * e = gsl_odeiv_evolve_alloc (1);
  gsl_odeiv_system sys = {rhs_stepfn3, 0, 1, 0};
     
  double t = -1.0;
  double h = 1e-6;
  double y = 0.0;

  int i = 0;
  int status;

  while (t < 1.0 && i < 10000) {
    status = gsl_odeiv_evolve_apply (e, c, s, &sys, &t, 1.0, &h, &y);
#ifdef DEBUG
    printf("i=%d status=%d t=%g h=%g y=%g\n", i, status, t, h, y);
#endif
    if (status != GSL_SUCCESS)
      break;

    i++;
  }

  gsl_test_abs(t, 1.0, 1e-16, "evolve big step function, t (stepfn3/rkf45)");
  gsl_test_rel(y, 1e300, epsrel, "evolve big step function, y (stepfn3/rkf45)");
     
  gsl_odeiv_evolve_free (e);
  gsl_odeiv_control_free (c);
  gsl_odeiv_step_free (s);
}

int rhs_cos (double t, const double * y, double * dydt, void * params) {
  dydt [0] = cos (t);
  return GSL_SUCCESS;
}

int jac_cos (double t, const double y[], double *dfdy, double dfdt[],
             void *params)
{
  dfdy[0] = 0.0;
  dfdt[0] = -sin(t);

  return GSL_SUCCESS;
}

/* Test evolution in negative direction */

void
test_evolve_negative_h (const gsl_odeiv_step_type * T, double h, double err)
{ 
  /* Tolerance factor in testing errors */
  const double factor = 10;

  gsl_odeiv_step * step = gsl_odeiv_step_alloc (T, 1);
  gsl_odeiv_control * c = gsl_odeiv_control_standard_new (err, err, 1.0, 0.0);
  gsl_odeiv_evolve * e = gsl_odeiv_evolve_alloc (1);
  gsl_odeiv_system sys = {rhs_cos, jac_cos, 1, 0};

  double t = 0;
  double t1 = -4.0;

  double y = 0.0;
  double yfin = sin (t1);

  /* Make initial h negative */
  h = -fabs(h);

  while (t > t1) {
    int status = gsl_odeiv_evolve_apply (e, c, step, &sys, &t, t1, &h, &y);
    
    if (status != GSL_SUCCESS) 
      {
	gsl_test(status, "%s evolve_apply returned %d for negative h",
		 gsl_odeiv_step_name (step), status);
	break;
      }
  }

  gsl_test_abs (y, yfin, factor * e->count * err,
		"evolution with negative h (using %s)", 
                gsl_odeiv_step_name (step));

  gsl_odeiv_evolve_free (e);
  gsl_odeiv_control_free (c);
  gsl_odeiv_step_free (step);
}

int
main (void)
{
  int i;

  struct ptype
  {
    const gsl_odeiv_step_type *type;
    double h;
  }
  p[20];

  p[0].type = gsl_odeiv_step_rk2;
  p[0].h = 1.0e-3;
  p[1].type = gsl_odeiv_step_rk4;
  p[1].h = 1.0e-3;
  p[2].type = gsl_odeiv_step_rkf45;
  p[2].h = 1.0e-3;
  p[3].type = gsl_odeiv_step_rkck;
  p[3].h = 1.0e-3;
  p[4].type = gsl_odeiv_step_rk8pd;
  p[4].h = 1.0e-3;
  p[5].type = gsl_odeiv_step_rk2imp;
  p[5].h = 1.0e-3;
  p[6].type = gsl_odeiv_step_rk2simp;
  p[6].h = 1.0e-3;
  p[7].type = gsl_odeiv_step_rk4imp;
  p[7].h = 1.0e-3;
  p[8].type = gsl_odeiv_step_bsimp;
  p[8].h = 1.0e-3;
  p[9].type = gsl_odeiv_step_gear1;
  p[9].h = 1.0e-3;
  p[10].type = gsl_odeiv_step_gear2;
  p[10].h = 1.0e-3;
  p[11].type = 0;

  gsl_ieee_env_setup ();

  for (i = 0; p[i].type != 0; i++)
    {
      test_stepper(p[i].type);
    }

  for (i = 0; p[i].type != 0; i++)
    {
      test_evolve_linear (p[i].type, p[i].h, 1e-10);
      test_evolve_exp (p[i].type, p[i].h, 1e-6);
      test_evolve_sin (p[i].type, p[i].h, 1e-8);
      test_evolve_xsin (p[i].type, p[i].h, 1e-8);
      test_evolve_xsin (p[i].type, 0.1, 1e-8); /* test with large step size */
      test_evolve_stiff1 (p[i].type, p[i].h, 1e-7);
      test_evolve_stiff5 (p[i].type, p[i].h, 1e-7);
      test_evolve_negative_h (p[i].type, p[i].h, 1e-7);
    }

  test_compare_vanderpol();
  test_compare_oregonator();
  
  test_stepfn();
  test_stepfn2();
  test_stepfn3();
 
  exit (gsl_test_summary ());
}
