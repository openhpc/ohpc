/* ode-initval/test.c
 * 
 * Copyright (C) 2009, 2010 Tuomo Keskitalo
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

/* Some functions and tests based on test.c by G. Jungman. */

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
#include <gsl/gsl_odeiv2.h>
#include "odeiv_util.h"

/* Maximum number of ODE equations */
#define MAXEQ 15

/* Maximum number of ODE solvers */
#define MAXNS 20

/* Track number of function and jacobian evaluations in tests
   with global variables 
 */
int nfe;
int nje;

/**********************************************************/
/* ODE test system definitions                            */
/**********************************************************/

/* RHS for f=2. Solution y = 2 * t + t0 */

int
rhs_linear (double t, const double y[], double f[], void *params)
{
  extern int nfe;
  nfe += 1;

  f[0] = 2.0;

  return GSL_SUCCESS;
}

int
jac_linear (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  extern int nje;
  nje += 1;

  dfdy[0] = 0.0;
  dfdt[0] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_lin = {
  rhs_linear,
  jac_linear,
  1,
  0
};

/* RHS for f=y. Equals y=exp(t) with initial value y(0)=1.0 */

int
rhs_exp (double t, const double y[], double f[], void *params)
{
  extern int nfe;
  nfe += 1;

  f[0] = y[0];

  return GSL_SUCCESS;
}

int
jac_exp (double t, const double y[], double *dfdy, double dfdt[],
         void *params)
{
  extern int nje;
  nje += 1;

  dfdy[0] = y[0];
  dfdt[0] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_exp = {
  rhs_exp,
  jac_exp,
  1,
  0
};

int
rhs_sin (double t, const double y[], double f[], void *params)
{
  extern int nfe;
  nfe += 1;

  f[0] = -y[1];
  f[1] = y[0];

  return GSL_SUCCESS;
}

int
jac_sin (double t, const double y[], double *dfdy, double dfdt[],
         void *params)
{
  extern int nje;
  nje += 1;

  dfdy[0] = 0.0;
  dfdy[1] = -1.0;
  dfdy[2] = 1.0;
  dfdy[3] = 0.0;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_sin = {
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
  extern int nfe;
  nfe += 1;

  if (rhs_xsin_reset)
    {
      rhs_xsin_reset = 0;
      n = 0;
      m = 1;
    }
  n++;

  if (n >= m)
    {
      m = n * 1.3;
      return GSL_EFAILED;
    }

  if (n > 40 && n < 65)
    {
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
  extern int nje;
  nje += 1;

  if (jac_xsin_reset)
    {
      jac_xsin_reset = 0;
      n = 0;
    }
  n++;

  if (n > 50 && n < 55)
    {
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

gsl_odeiv2_system rhs_func_xsin = {
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
  extern int nfe;
  nfe += 1;

  f[0] = 998.0 * y[0] + 1998.0 * y[1];
  f[1] = -999.0 * y[0] - 1999.0 * y[1];

  return GSL_SUCCESS;
}

int
jac_stiff (double t, const double y[], double *dfdy, double dfdt[],
           void *params)
{
  extern int nje;
  nje += 1;

  dfdy[0] = 998.0;
  dfdy[1] = 1998.0;
  dfdy[2] = -999.0;
  dfdy[3] = -1999.0;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_stiff = {
  rhs_stiff,
  jac_stiff,
  2,
  0
};

/* Cosine function */

int
rhs_cos (double t, const double *y, double *dydt, void *params)
{
  dydt[0] = cos (t);
  return GSL_SUCCESS;
}

int
jac_cos (double t, const double y[], double *dfdy, double dfdt[],
         void *params)
{
  dfdy[0] = 0.0;
  dfdt[0] = -sin (t);

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_cos = {
  rhs_cos,
  jac_cos,
  1,
  0
};

/* Broken problem for testing numerical problems in user function that
   leads to decrease of step size in gsl_odeiv2_evolve below machine
   precision.
 */

int
rhs_broken (double t, const double y[], double f[], void *params)
{
  if (t < 10.0)
    {
      f[0] = 1.0;
    }

  else
    {
      f[0] = GSL_NAN;
      return 123;
    }

  return GSL_SUCCESS;
}

int
jac_broken (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  if (t < 10.0)
    {
      dfdy[0] = 0.0;
      dfdt[0] = 0.0;
    }
  else
    {
      dfdy[0] = GSL_NAN;
      dfdt[0] = GSL_NAN;
      return 123;
    }

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_broken = {
  rhs_broken,
  jac_broken,
  1,
  0
};

/* Immediate user break (at t > 1.5) test sine system */

int
rhs_sin_ub (double t, const double y[], double f[], void *params)
{
  extern int nfe;
  nfe += 1;

  f[0] = -y[1];
  f[1] = y[0];

  if (t > 1.5)
    {
      return GSL_EBADFUNC;
    }

  return GSL_SUCCESS;
}

int
jac_sin_ub (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  extern int nje;
  nje += 1;

  dfdy[0] = 0.0;
  dfdy[1] = -1.0;
  dfdy[2] = 1.0;
  dfdy[3] = 0.0;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  if (t > 1.5)
    {
      return GSL_EBADFUNC;
    }

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_sin_ub = {
  rhs_sin_ub,
  jac_sin_ub,
  2,
  0
};

/* Badly scaled random function */

int
rhs_br (double t, const double *y, double *dydt, void *params)
{
  dydt[0] = (rand () - RAND_MAX / 2) * 2e100;
  return GSL_SUCCESS;
}

int
jac_br (double t, const double y[], double *dfdy, double dfdt[], void *params)
{
  dfdy[0] = (rand () - RAND_MAX / 2) * 2e100;
  dfdt[0] = (rand () - RAND_MAX / 2) * 2e100;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_br = {
  rhs_br,
  jac_br,
  1,
  0
};

/* stepfn and stepfn2 based on testcases from Frank Reininghaus
   <frank78ac@googlemail.com> 
*/

/* Derivative change at t=0, small derivative */

int
rhs_stepfn (double t, const double *y, double *dydt, void *params)
{
  if (t >= 1.0)
    dydt[0] = 1;
  else
    dydt[0] = 0;

  return GSL_SUCCESS;
}

int
jac_stepfn (double t, const double y[], double *dfdy, double dfdt[],
            void *params)
{
  dfdy[0] = 0.0;
  dfdt[0] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_stepfn = {
  rhs_stepfn,
  jac_stepfn,
  1,
  0
};

/* Derivative change at t=0, large derivative */

int
rhs_stepfn2 (double t, const double *y, double *dydt, void *params)
{
  if (t >= 0.0)
    dydt[0] = 1e300;
  else
    dydt[0] = 0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_stepfn2 = {
  rhs_stepfn2,
  jac_stepfn,
  1,
  0
};

/* Volterra-Lotka predator-prey model
   f0 = (a - b * y1) * y0     y0(0) = 2.725
   f1 = (-c + d * y0) * y1    y1(0) = 1.0
 */

int
rhs_vl (double t, const double y[], double f[], void *params)
{
  const double a = -1.0;
  const double b = -1.0;
  const double c = -2.0;
  const double d = -1.0;

  extern int nfe;
  nfe += 1;

  f[0] = (a - b * y[1]) * y[0];
  f[1] = (-c + d * y[0]) * y[1];

  return GSL_SUCCESS;
}

int
jac_vl (double t, const double y[], double *dfdy, double dfdt[], void *params)
{
  const double a = -1.0;
  const double b = -1.0;
  const double c = -2.0;
  const double d = -1.0;

  extern int nje;
  nje += 1;

  dfdy[0] = a - b * y[1];
  dfdy[1] = -b * y[0];
  dfdy[2] = d * y[1];
  dfdy[3] = -c + d * y[0];

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_vl = {
  rhs_vl,
  jac_vl,
  2,
  0
};

/* van Der Pol oscillator
   f0 = y1                           y0(0) = 1.0
   f1 = -y0 + mu * y1 * (1 - y0^2)   y1(0) = 0.0
*/

int
rhs_vanderpol (double t, const double y[], double f[], void *params)
{
  const double mu = 10.0;

  extern int nfe;
  nfe += 1;

  f[0] = y[1];
  f[1] = -y[0] + mu * y[1] * (1.0 - y[0] * y[0]);

  return GSL_SUCCESS;
}

int
jac_vanderpol (double t, const double y[], double *dfdy, double dfdt[],
               void *params)
{
  const double mu = 10.0;

  extern int nje;
  nje += 1;

  dfdy[0] = 0.0;
  dfdy[1] = 1.0;
  dfdy[2] = -2.0 * mu * y[0] * y[1] - 1.0;
  dfdy[3] = mu * (1.0 - y[0] * y[0]);

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_vanderpol = {
  rhs_vanderpol,
  jac_vanderpol,
  2,
  0
};

/* Stiff trigonometric example 
   f0 = -50 * (y0 - cos(t))    y0(0) = 0.0
 */

int
rhs_stifftrig (double t, const double y[], double f[], void *params)
{
  extern int nfe;
  nfe += 1;

  f[0] = -50 * (y[0] - cos (t));

  return GSL_SUCCESS;
}

int
jac_stifftrig (double t, const double y[], double *dfdy, double dfdt[],
               void *params)
{
  extern int nje;
  nje += 1;

  dfdy[0] = -50;

  dfdt[0] = -50 * sin (t);

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_stifftrig = {
  rhs_stifftrig,
  jac_stifftrig,
  1,
  0
};

/* The Oregonator - chemical Belusov-Zhabotinskii reaction 
   y0(0) = 1.0, y1(0) = 2.0, y2(0) = 3.0
*/

int
rhs_oregonator (double t, const double y[], double f[], void *params)
{
  const double c1 = 77.27;
  const double c2 = 8.375e-6;
  const double c3 = 0.161;

  extern int nfe;
  nfe += 1;

  f[0] = c1 * (y[1] + y[0] * (1 - c2 * y[0] - y[1]));
  f[1] = 1 / c1 * (y[2] - y[1] * (1 + y[0]));
  f[2] = c3 * (y[0] - y[2]);

  return GSL_SUCCESS;
}

int
jac_oregonator (double t, const double y[], double *dfdy, double dfdt[],
                void *params)
{
  const double c1 = 77.27;
  const double c2 = 8.375e-6;
  const double c3 = 0.161;

  extern int nje;
  nje += 1;

  dfdy[0] = c1 * (1 - 2 * c2 * y[0] - y[1]);
  dfdy[1] = c1 * (1 - y[0]);
  dfdy[2] = 0.0;

  dfdy[3] = 1 / c1 * (-y[1]);
  dfdy[4] = 1 / c1 * (-1 - y[0]);
  dfdy[5] = 1 / c1;

  dfdy[6] = c3;
  dfdy[7] = 0.0;
  dfdy[8] = -c3;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;
  dfdt[2] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_oregonator = {
  rhs_oregonator,
  jac_oregonator,
  3,
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

  extern int nfe;
  nfe += 1;

  f[0] = -a * y[0] - b * y[0] * y[2];
  f[1] = a * y[0] - m * c * y[1] * y[2];
  f[3] = b * y[0] * y[2] - c * y[3];
  f[2] = f[1] - f[3];

  return GSL_SUCCESS;
}

int
jac_e5 (double t, const double y[], double *dfdy, double dfdt[], void *params)
{
  const double a = 7.89e-10;
  const double b = 1.1e7;
  const double c = 1.13e3;
  const double m = 1.0e6;

  extern int nje;
  nje += 1;

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

gsl_odeiv2_system rhs_func_e5 = {
  rhs_e5,
  jac_e5,
  4,
  0
};

/* Chemical reaction system of H.H. Robertson (1966): The solution of
   a set of reaction rate equations. In: J. Walsh, ed.: Numer.
   Anal., an Introduction, Academ. Press, pp. 178-182.

   f0 = -a * y0 + b * y1 * y2             y0(0) = 1.0
   f1 = a * y0 - b * y1 * y2 - c * y1^2   y1(0) = 0.0
   f2 = c * y1^2                          y2(0) = 0.0
 */

int
rhs_robertson (double t, const double y[], double f[], void *params)
{
  const double a = 0.04;
  const double b = 1.0e4;
  const double c = 3.0e7;

  extern int nfe;
  nfe += 1;

  f[0] = -a * y[0] + b * y[1] * y[2];
  f[2] = c * y[1] * y[1];
  f[1] = -f[0] - f[2];

  return GSL_SUCCESS;
}

int
jac_robertson (double t, const double y[], double *dfdy, double dfdt[],
               void *params)
{
  const double a = 0.04;
  const double b = 1.0e4;
  const double c = 3.0e7;

  extern int nje;
  nje += 1;

  dfdy[0] = -a;
  dfdy[1] = b * y[2];
  dfdy[2] = b * y[1];

  dfdy[3] = a;
  dfdy[4] = -b * y[2] - 2 * c * y[1];
  dfdy[5] = -b * y[1];

  dfdy[6] = 0.0;
  dfdy[7] = 2 * c * y[1];
  dfdy[8] = 0.0;

  dfdt[0] = 0.0;
  dfdt[1] = 0.0;
  dfdt[2] = 0.0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_robertson = {
  rhs_robertson,
  jac_robertson,
  3,
  0
};

/* A two-dimensional oscillating Brusselator system.

   f0 = a + y0^2 * y1 - (b + 1) * y0      y0(0) = 1.5
   f1 = b * y0 - y0^2 * y1                y1(0) = 3.0
 */

int
rhs_brusselator (double t, const double y[], double f[], void *params)
{
  const double a = 1.0;
  const double b = 3.0;

  extern int nfe;
  nfe += 1;

  f[0] = a + y[0] * y[0] * y[1] - (b + 1.0) * y[0];
  f[1] = b * y[0] - y[0] * y[0] * y[1];

  return GSL_SUCCESS;
}

int
jac_brusselator (double t, const double y[], double *dfdy, double dfdt[],
                 void *params)
{
  const double b = 3.0;

  extern int nje;
  nje += 1;

  dfdy[0] = 2 * y[0] * y[1] - (b + 1.0);
  dfdy[1] = y[0] * y[0];

  dfdy[2] = b - 2 * y[0] * y[1];
  dfdy[3] = -y[0] * y[0];

  dfdt[0] = 0;
  dfdt[1] = 0;

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_brusselator = {
  rhs_brusselator,
  jac_brusselator,
  2,
  0
};

/* Ring Modulator, stiff ODE of dimension 15. 

   Reference: Walter M. Lioen, Jacques J.B. de Swart, Test Set for
   Initial Value Problem Solvers, Release 2.1 September 1999,
   http://ftp.cwi.nl/IVPtestset/software.htm
 */

#define NRINGMOD 15

int
rhs_ringmod (double t, const double y[], double f[], void *params)
{
  const double c = 1.6e-8;
  const double cs = 2e-12;
  const double cp = 1e-8;
  const double r = 25e3;
  const double rp = 50e0;
  const double lh = 4.45e0;
  const double ls1 = 2e-3;
  const double ls2 = 5e-4;
  const double ls3 = 5e-4;
  const double rg1 = 36.3;
  const double rg2 = 17.3;
  const double rg3 = 17.3;
  const double ri = 5e1;
  const double rc = 6e2;
  const double gamma = 40.67286402e-9;
  const double delta = 17.7493332;
  const double pi = 3.141592653589793238462643383;

  const double uin1 = 0.5 * sin (2e3 * pi * t);
  const double uin2 = 2 * sin (2e4 * pi * t);
  const double ud1 = +y[2] - y[4] - y[6] - uin2;
  const double ud2 = -y[3] + y[5] - y[6] - uin2;
  const double ud3 = +y[3] + y[4] + y[6] + uin2;
  const double ud4 = -y[2] - y[5] + y[6] + uin2;

  const double qud1 = gamma * (exp (delta * ud1) - 1.0);
  const double qud2 = gamma * (exp (delta * ud2) - 1.0);
  const double qud3 = gamma * (exp (delta * ud3) - 1.0);
  const double qud4 = gamma * (exp (delta * ud4) - 1.0);

  extern int nfe;
  nfe += 1;

  f[0] = (y[7] - 0.5 * y[9] + 0.5 * y[10] + y[13] - y[0] / r) / c;
  f[1] = (y[8] - 0.5 * y[11] + 0.5 * y[12] + y[14] - y[1] / r) / c;
  f[2] = (y[9] - qud1 + qud4) / cs;
  f[3] = (-y[10] + qud2 - qud3) / cs;
  f[4] = (y[11] + qud1 - qud3) / cs;
  f[5] = (-y[12] - qud2 + qud4) / cs;
  f[6] = (-y[6] / rp + qud1 + qud2 - qud3 - qud4) / cp;
  f[7] = -y[0] / lh;
  f[8] = -y[1] / lh;
  f[9] = (0.5 * y[0] - y[2] - rg2 * y[9]) / ls2;
  f[10] = (-0.5 * y[0] + y[3] - rg3 * y[10]) / ls3;
  f[11] = (0.5 * y[1] - y[4] - rg2 * y[11]) / ls2;
  f[12] = (-0.5 * y[1] + y[5] - rg3 * y[12]) / ls3;
  f[13] = (-y[0] + uin1 - (ri + rg1) * y[13]) / ls1;
  f[14] = (-y[1] - (rc + rg1) * y[14]) / ls1;

  return GSL_SUCCESS;
}

int
jac_ringmod (double t, const double y[], double *dfdy, double dfdt[],
             void *params)
{
  const double c = 1.6e-8;
  const double cs = 2e-12;
  const double cp = 1e-8;
  const double r = 25e3;
  const double rp = 50e0;
  const double lh = 4.45e0;
  const double ls1 = 2e-3;
  const double ls2 = 5e-4;
  const double ls3 = 5e-4;
  const double rg1 = 36.3;
  const double rg2 = 17.3;
  const double rg3 = 17.3;
  const double ri = 5e1;
  const double rc = 6e2;
  const double gamma = 40.67286402e-9;
  const double delta = 17.7493332;
  const double pi = 3.141592653589793238462643383;

  const double uin2 = 2 * sin (2e4 * pi * t);
  const double ud1 = +y[2] - y[4] - y[6] - uin2;
  const double ud2 = -y[3] + y[5] - y[6] - uin2;
  const double ud3 = +y[3] + y[4] + y[6] + uin2;
  const double ud4 = -y[2] - y[5] + y[6] + uin2;
  const double qpud1 = gamma * delta * exp (delta * ud1);
  const double qpud2 = gamma * delta * exp (delta * ud2);
  const double qpud3 = gamma * delta * exp (delta * ud3);
  const double qpud4 = gamma * delta * exp (delta * ud4);

  extern int nje;
  size_t i;

  nje += 1;

  for (i = 0; i < NRINGMOD * NRINGMOD; i++)
    {
      dfdy[i] = 0.0;
    }



  dfdy[0 * NRINGMOD + 0] = -1 / (c * r);
  dfdy[0 * NRINGMOD + 7] = 1 / c;
  dfdy[0 * NRINGMOD + 9] = -0.5 / c;
  dfdy[0 * NRINGMOD + 10] = -dfdy[0 * NRINGMOD + 9];
  dfdy[0 * NRINGMOD + 13] = dfdy[0 * NRINGMOD + 7];
  dfdy[1 * NRINGMOD + 1] = dfdy[0 * NRINGMOD + 0];
  dfdy[1 * NRINGMOD + 8] = dfdy[0 * NRINGMOD + 7];
  dfdy[1 * NRINGMOD + 11] = dfdy[0 * NRINGMOD + 9];
  dfdy[1 * NRINGMOD + 12] = dfdy[0 * NRINGMOD + 10];
  dfdy[1 * NRINGMOD + 14] = dfdy[0 * NRINGMOD + 13];
  dfdy[2 * NRINGMOD + 2] = (-qpud1 - qpud4) / cs;
  dfdy[2 * NRINGMOD + 4] = qpud1 / cs;
  dfdy[2 * NRINGMOD + 5] = -qpud4 / cs;
  dfdy[2 * NRINGMOD + 6] = (qpud1 + qpud4) / cs;
  dfdy[2 * NRINGMOD + 9] = 1 / cs;
  dfdy[3 * NRINGMOD + 3] = (-qpud2 - qpud3) / cs;
  dfdy[3 * NRINGMOD + 4] = -qpud3 / cs;
  dfdy[3 * NRINGMOD + 5] = qpud2 / cs;
  dfdy[3 * NRINGMOD + 6] = (-qpud2 - qpud3) / cs;
  dfdy[3 * NRINGMOD + 10] = -1 / cs;
  dfdy[4 * NRINGMOD + 2] = qpud1 / cs;
  dfdy[4 * NRINGMOD + 3] = -qpud3 / cs;
  dfdy[4 * NRINGMOD + 4] = (-qpud1 - qpud3) / cs;
  dfdy[4 * NRINGMOD + 6] = (-qpud1 - qpud3) / cs;
  dfdy[4 * NRINGMOD + 11] = 1 / cs;
  dfdy[5 * NRINGMOD + 2] = -qpud4 / cs;
  dfdy[5 * NRINGMOD + 3] = qpud2 / cs;
  dfdy[5 * NRINGMOD + 5] = (-qpud2 - qpud4) / cs;
  dfdy[5 * NRINGMOD + 6] = (qpud2 + qpud4) / cs;
  dfdy[5 * NRINGMOD + 12] = -1 / cs;
  dfdy[6 * NRINGMOD + 2] = (qpud1 + qpud4) / cp;
  dfdy[6 * NRINGMOD + 3] = (-qpud2 - qpud3) / cp;
  dfdy[6 * NRINGMOD + 4] = (-qpud1 - qpud3) / cp;
  dfdy[6 * NRINGMOD + 5] = (qpud2 + qpud4) / cp;
  dfdy[6 * NRINGMOD + 6] = (-qpud1 - qpud2 - qpud3 - qpud4 - 1 / rp) / cp;
  dfdy[7 * NRINGMOD + 0] = -1 / lh;
  dfdy[8 * NRINGMOD + 1] = dfdy[7 * NRINGMOD + 0];
  dfdy[9 * NRINGMOD + 0] = 0.5 / ls2;
  dfdy[9 * NRINGMOD + 2] = -1 / ls2;
  dfdy[9 * NRINGMOD + 9] = -rg2 / ls2;
  dfdy[10 * NRINGMOD + 0] = -0.5 / ls3;
  dfdy[10 * NRINGMOD + 3] = 1 / ls3;
  dfdy[10 * NRINGMOD + 10] = -rg3 / ls3;
  dfdy[11 * NRINGMOD + 1] = dfdy[9 * NRINGMOD + 0];
  dfdy[11 * NRINGMOD + 4] = dfdy[9 * NRINGMOD + 2];
  dfdy[11 * NRINGMOD + 11] = dfdy[9 * NRINGMOD + 9];
  dfdy[12 * NRINGMOD + 1] = dfdy[10 * NRINGMOD + 0];
  dfdy[12 * NRINGMOD + 5] = dfdy[10 * NRINGMOD + 3];
  dfdy[12 * NRINGMOD + 12] = dfdy[10 * NRINGMOD + 10];
  dfdy[13 * NRINGMOD + 0] = -1 / ls1;
  dfdy[13 * NRINGMOD + 13] = -(ri + rg1) / ls1;
  dfdy[14 * NRINGMOD + 1] = dfdy[13 * NRINGMOD + 0];
  dfdy[14 * NRINGMOD + 14] = -(rc + rg1) / ls1;

  for (i = 0; i < NRINGMOD; i++)
    {
      dfdt[i] = 0.0;
    }

  return GSL_SUCCESS;
}

gsl_odeiv2_system rhs_func_ringmod = {
  rhs_ringmod,
  jac_ringmod,
  NRINGMOD,
  NULL
};


/**********************************************************/
/* Functions for carrying out tests                       */
/**********************************************************/

void
test_odeiv_stepper (const gsl_odeiv2_step_type * T,
                    const gsl_odeiv2_system * sys, const double h,
                    const double t, const char desc[], const double ystart[],
                    const double yfin[], const double relerr)
{
  /* tests stepper T with one fixed length step advance of system sys
     and compares the result with given values yfin
   */

  double y[MAXEQ] = { 0.0 };
  double yerr[MAXEQ] = { 0.0 };
  double scale_abs[MAXEQ];
  size_t ne = sys->dimension;
  size_t i;
  gsl_odeiv2_driver *d;

  for (i = 0; i < MAXEQ; i++)
    {
      scale_abs[i] = 1.0;
    }

  d = gsl_odeiv2_driver_alloc_scaled_new (sys, T, h, relerr, relerr,
                                          1.0, 0.0, scale_abs);

  DBL_MEMCPY (y, ystart, MAXEQ);

  {
    int s = gsl_odeiv2_step_apply (d->s, t, h, y, yerr, 0, 0, sys);

    if (s != GSL_SUCCESS)
      {
        gsl_test (s, "test_odeiv_stepper: %s step_apply returned %d", desc,
                  s);
      }
  }

  for (i = 0; i < ne; i++)
    {
      gsl_test_rel (y[i], yfin[i], relerr,
                    "%s %s step(%d)", gsl_odeiv2_step_name (d->s), desc, i);
    }

  gsl_odeiv2_driver_free (d);
}

void
test_stepper (const gsl_odeiv2_step_type * T)
{
  /* Tests stepper T with a step of selected systems */

  double y[MAXEQ] = { 0.0 };
  double yfin[MAXEQ] = { 0.0 };

  /* Step length */
  double h;

  /* Required tolerance */
  double err_target;

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
  y[0] = exp (2.7);
  yfin[0] = exp (2.7 + h);
  test_odeiv_stepper (T, &rhs_func_exp, h, 2.7, "exponential",
                      y, yfin, err_target);
  /* cosine-sine */
  h = 1e-3;
  err_target = 1e-6;
  y[0] = cos (1.2);
  y[1] = sin (1.2);
  yfin[0] = cos (1.2 + h);
  yfin[1] = sin (1.2 + h);
  test_odeiv_stepper (T, &rhs_func_sin, h, 1.2, "cosine-sine",
                      y, yfin, err_target);
}

void
test_evolve_system (const gsl_odeiv2_step_type * T,
                    const gsl_odeiv2_system * sys,
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

  gsl_odeiv2_driver *d =
    gsl_odeiv2_driver_alloc_standard_new (sys, T, h, err_target, err_target,
                                          1.0, 0.0);

  double *y_orig = (double *) malloc (sys->dimension * sizeof (double));

  int s = 0;

  extern int nfe, nje;
  nfe = 0;
  nje = 0;

  while (t < t1)
    {
      double t_orig = t;
      memcpy (y_orig, y, sys->dimension * sizeof (double));
      s = gsl_odeiv2_evolve_apply (d->e, d->c, d->s, sys, &t, t1, &h, y);

#ifdef DEBUG
      printf (" %.5e %.5e %.5e %d\n", t, y[0], y[1],
              gsl_odeiv2_step_order (d->s));
#endif

      if (s != GSL_SUCCESS)
        {
          /* check that t and y are unchanged */
          gsl_test_abs (t, t_orig, 0.0, "%s t must be restored on failure",
                        gsl_odeiv2_step_name (d->s));

          for (i = 0; i < sys->dimension; i++)
            {
              gsl_test_abs (y[i], y_orig[i], 0.0,
                            "%s y must be restored on failure",
                            gsl_odeiv2_step_name (d->s), desc, i);
            }

          if (sys != &rhs_func_xsin)
            {
              /* apart from xsin, other functions should not return errors */
              gsl_test (s, "%s evolve_apply returned %d",
                        gsl_odeiv2_step_name (d->s), s);
              break;
            }
        }

      if (steps > 100000)
        {
          gsl_test (GSL_EFAILED,
                    "%s evolve_apply reached maxiter",
                    gsl_odeiv2_step_name (d->s));
          break;
        }

      steps++;
    }

  gsl_test (s, "%s %s [%g,%g], %d steps (nfe %d, nje %d) completed",
            gsl_odeiv2_step_name (d->s), desc, t0, t1, steps, nfe, nje);

  /* err_target is target error of one step. Test if stepper has made
     larger error than (tolerance factor times) the number of steps
     times the err_target.
   */

  for (i = 0; i < sys->dimension; i++)
    {
      gsl_test_abs (y[i], yfin[i], factor * d->e->count * err_target,
                    "%s %s evolve(%d)", gsl_odeiv2_step_name (d->s), desc, i);
    }

  free (y_orig);
  gsl_odeiv2_driver_free (d);
}

int
sys_driver (const gsl_odeiv2_step_type * T,
            const gsl_odeiv2_system * sys,
            double t0, double t1, double hstart,
            double y[], double epsabs, double epsrel, const char desc[])
{
  /* This function evolves a system sys with stepper T from t0 to t1.
     Step length is varied via error control with possibly different
     absolute and relative error tolerances.
   */

  int s = 0;
  int steps = 0;

  double t = t0;
  double h = hstart;

  gsl_odeiv2_driver *d =
    gsl_odeiv2_driver_alloc_standard_new (sys, T, h, epsabs, epsrel,
                                          1.0, 0.0);

  extern int nfe, nje;
  nfe = 0;
  nje = 0;

  while (t < t1)
    {
      s = gsl_odeiv2_evolve_apply (d->e, d->c, d->s, sys, &t, t1, &h, y);

#ifdef DEBUG
      printf ("%.5e %.5e %.5e %d\n", t, y[0], y[1],
              gsl_odeiv2_step_order (d->s));
#endif

      if (s != GSL_SUCCESS)
        {
          gsl_test (s, "sys_driver: %s evolve_apply returned %d",
                    gsl_odeiv2_step_name (d->s), s);
          break;
        }

      if (steps > 1e7)
        {
          gsl_test (GSL_EMAXITER,
                    "sys_driver: %s evolve_apply reached maxiter at t=%g",
                    gsl_odeiv2_step_name (d->s), t);
          s = GSL_EMAXITER;
          break;
        }

      steps++;
    }

  gsl_test (s, "%s %s [%g,%g], %d steps (nfe %d, nje %d) completed",
            gsl_odeiv2_step_name (d->s), desc, t0, t1, steps, nfe, nje);

  gsl_odeiv2_driver_free (d);

  return s;
}

void
test_evolve_linear (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Test linear evolve */

  double y[1];
  double yfin[1];

  y[0] = 1.0;
  yfin[0] = 9.0;
  test_evolve_system (T, &rhs_func_lin, 0.0, 4.0, h, y, yfin, err,
                      "linear[0,4]");
}

void
test_evolve_exp (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Test exponential evolve */

  double y[1];
  double yfin[1];

  y[0] = 1.0;
  yfin[0] = exp (2.0);
  test_evolve_system (T, &rhs_func_exp, 0.0, 2.0, h, y, yfin, err,
                      "exp[0,2]");
}

void
test_evolve_sin (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Test sinusoidal evolve */

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
test_evolve_xsin (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Test sinusoidal evolve including a failing window */

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
test_evolve_stiff1 (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Test classical stiff problem evolve, t=[0,1] */

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
test_evolve_stiff5 (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Test classical stiff problem evolve, t=[0,5] */

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

void
test_evolve_negative_h (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Test evolution in negative direction */

  /* Tolerance factor in testing errors */
  const double factor = 10;

  const gsl_odeiv2_system sys = rhs_func_cos;

  double t = 0;
  double t1 = -4.0;

  double y = 0.0;
  double yfin = sin (t1);
  gsl_odeiv2_driver *d;

  /* Make initial h negative */
  h = -fabs (h);
  d = gsl_odeiv2_driver_alloc_standard_new (&sys, T, h, err, err, 1.0, 0.0);

  while (t > t1)
    {
      int status = gsl_odeiv2_evolve_apply (d->e, d->c, d->s,
                                            &sys, &t, t1, &h, &y);

      if (status != GSL_SUCCESS)
        {
          gsl_test (status, "%s evolve_apply returned %d for negative h",
                    gsl_odeiv2_step_name (d->s), status);
          break;
        }
    }

  gsl_test_abs (y, yfin, factor * d->e->count * err,
                "%s evolution with negative h", gsl_odeiv2_step_name (d->s));

  gsl_odeiv2_driver_free (d);
}

void
test_broken (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Check for gsl_odeiv2_evolve_apply. The user function fails at
     t>=10, which in this test causes step size to decrease below
     machine precision and return with a failure code.
   */

  /* Tolerance factor in testing errors */
  const double factor = 10;

  const gsl_odeiv2_system sys = rhs_func_broken;

  gsl_odeiv2_driver *d = gsl_odeiv2_driver_alloc_y_new (&sys, T, h, err, err);

  double t = 0;
  double t1 = 100.0;

  double y = 0.0;
  const double final_max_h = GSL_DBL_EPSILON;

  int status;
  while (t < t1)
    {
      status =
        gsl_odeiv2_evolve_apply (d->e, d->c, d->s, &sys, &t, t1, &h, &y);

      if (status != GSL_SUCCESS)
        {
          break;
        }
    }

  gsl_test_abs (h + final_max_h, final_max_h, factor * final_max_h,
                "%s test_broken: step size at break point",
                gsl_odeiv2_step_name (d->s));

  gsl_test_abs (t, 10.0, factor * err,
                "%s test_broken: point of break",
                gsl_odeiv2_step_name (d->s));

  /* GSL_FAILURE results from stepper failure, 123 from user function */

  if (status != GSL_FAILURE && status != 123)
    {
      gsl_test (status, "%s test_broken: evolve return value %d",
                gsl_odeiv2_step_name (d->s), status);
    }
  else
    {
      gsl_test (GSL_SUCCESS, "%s test_broken: evolve return value %d",
                gsl_odeiv2_step_name (d->s), status);
    }

  gsl_odeiv2_driver_free (d);
}

void
test_stepsize_fail (const gsl_odeiv2_step_type * T, double h)
{
  /* Check for gsl_odeiv2_evolve_apply. The user function works
     apparently fine (returns GSL_SUCCESS) but is faulty in this case.
     gsl_odeiv2_evolve_apply therefore decreases the step-size below
     machine precision and therefore stops with GSL_FAILURE.
   */

  /* Error tolerance */
  const double epsabs = 1e-16;
  const double epsrel = 1e-6;

  /* Tolerance factor in testing errors */
  const double factor = 10;

  const double final_max_h = GSL_DBL_EPSILON;
  const gsl_odeiv2_system sys = rhs_func_br;

  gsl_odeiv2_driver *d =
    gsl_odeiv2_driver_alloc_y_new (&sys, T, h, epsabs, epsrel);

  double t = 1.0;
  double t1 = 1e5;
  double y = 0.0;
  int status;

  while (t < t1)
    {
      status =
        gsl_odeiv2_evolve_apply (d->e, d->c, d->s, &sys, &t, t1, &h, &y);

      if (status != GSL_SUCCESS)
        {
          break;
        }
    }

  gsl_test_abs (h + final_max_h, final_max_h, factor * final_max_h,
                "%s test_stepsize_fail: step size at break point",
                gsl_odeiv2_step_name (d->s));

  gsl_test_abs (t, 1.0, 1e-6,
                "%s test_stepsize_fail: point of break",
                gsl_odeiv2_step_name (d->s));

  gsl_test_int (status, GSL_FAILURE,
                "%s test_stepsize_fail: evolve return value",
                gsl_odeiv2_step_name (d->s));

  gsl_odeiv2_driver_free (d);
}

void
test_user_break (const gsl_odeiv2_step_type * T, double h)
{
  /* Tests for user signaled immediate break */

  const double tol = 1e-8;

  gsl_odeiv2_driver *d = gsl_odeiv2_driver_alloc_y_new (&rhs_func_sin_ub, T,
                                                        h, tol, tol);

  double y[] = { 1.0, 0.0 };
  double t = 0.0;
  const double t1 = 8.25;

  int s = gsl_odeiv2_driver_apply (d, &t, t1, y);

  gsl_test ((s - GSL_EBADFUNC), "%s test_user_break returned %d",
            gsl_odeiv2_step_name (d->s), s);

  gsl_odeiv2_driver_free (d);
}

void
test_stepfn (const gsl_odeiv2_step_type * T)
{
  /* Test evolve on a small derivative change at t=0 */

  double epsabs = 1e-16;
  double epsrel = 1e-6;

  const gsl_odeiv2_system sys = rhs_func_stepfn;

  double t = 0.0;
  double h = 1e-6;
  double y = 0.0;
  int i = 0;
  int status = 0;

  gsl_odeiv2_driver *d =
    gsl_odeiv2_driver_alloc_y_new (&sys, T, h, epsabs, epsrel);

  while (t < 2 && i < 1000000)
    {
      status =
        gsl_odeiv2_evolve_apply (d->e, d->c, d->s, &sys, &t, 2, &h, &y);
#ifdef DEBUG
      printf ("i=%d status=%d t=%g h=%g y=%g\n", i, status, t, h, y);
#endif

      if (status != GSL_SUCCESS)
        break;

      i++;
    }

  gsl_test (status, "evolve step function, return value (stepfn/%s): %d",
            gsl_odeiv2_step_name (d->s), status);

  gsl_test_abs (t, 2, 1e-16, "evolve step function, t (stepfn/%s)",
                gsl_odeiv2_step_name (d->s));

  gsl_test_rel (y, 1, epsrel, "evolve step function, y (stepfn/%s)",
                gsl_odeiv2_step_name (d->s));

  gsl_odeiv2_driver_free (d);
}

void
test_stepfn2 (const gsl_odeiv2_step_type * T)
{
  /* Test evolve on a large derivative change at t=0 */

  double epsabs = 1e-16;
  double epsrel = 1e-6;

  const gsl_odeiv2_system sys = rhs_func_stepfn2;

  double t = -1.0;
  double h = 1e-6;
  double y = 0.0;

  int i = 0;
  int status;
  const int maxiter = 100000;

  gsl_odeiv2_driver *d =
    gsl_odeiv2_driver_alloc_yp_new (&sys, T, h, epsabs, epsrel);

  while (t < 1.0 && i < maxiter)
    {
      status =
        gsl_odeiv2_evolve_apply (d->e, d->c, d->s, &sys, &t, 1.0, &h, &y);
#ifdef DEBUG
      printf ("i=%d status=%d t=%g h=%g y=%g\n", i, status, t, h, y);
#endif
      if (status != GSL_SUCCESS)
        break;

      i++;
    }

  if (i >= maxiter)
    printf ("FAIL: evolve big step function, (stepfn2/%s) reached maxiter\n",
            gsl_odeiv2_step_name (d->s));

  gsl_test_abs (t, 1.0, 1e-16, "evolve big step function, t (stepfn2/%s)",
                gsl_odeiv2_step_name (d->s));
  gsl_test_rel (y, 1e300, epsrel, "evolve big step function, y (stepfn2/%s)",
                gsl_odeiv2_step_name (d->s));

  gsl_odeiv2_driver_free (d);
}

void
test_nonstiff_problems (void)
{
  /* Compares output of non-stiff (or only moderately stiff) problems
     with several steppers 
   */

  const gsl_odeiv2_step_type *steppers[MAXNS];

  /* Required error tolerance for each stepper. */
  double err_target[MAXNS];

  /* initial values for each ode-solver */
  double y[MAXEQ * MAXNS];

  size_t i, k, p;

  /* Number of problems to test */
#define CONST_NONSTIFF_NPROB 4

  /* Problems, their names and number of equations */
  const gsl_odeiv2_system *prob[] =
    { &rhs_func_vl, &rhs_func_vanderpol, &rhs_func_stifftrig,
    &rhs_func_brusselator
  };
  const char *probname[] =
    { "volterra-lotka", "vanderpol", "stifftrig", "brusselator" };
  const size_t sd[] = { 2, 2, 1, 2 };

  /* Integration interval for problems */
  const double start[CONST_NONSTIFF_NPROB] = { 0.0 };
  const double end[] = { 9.0, 100.0, 1.5, 20.0 };

  const double epsabs = 1e-8;
  const double epsrel = 1e-8;
  const double initstepsize = 1e-5;

  /* Steppers */

  steppers[0] = gsl_odeiv2_step_rk4;
  err_target[0] = 1e-6;
  steppers[1] = gsl_odeiv2_step_rk2;
  err_target[1] = 1e-6;
  steppers[2] = gsl_odeiv2_step_rkf45;
  err_target[2] = 1e-6;
  steppers[3] = gsl_odeiv2_step_rkck;
  err_target[3] = 1e-6;
  steppers[4] = gsl_odeiv2_step_rk8pd;
  err_target[4] = 1e-6;
  steppers[5] = gsl_odeiv2_step_rk1imp;
  err_target[5] = 1e-3;
  steppers[6] = gsl_odeiv2_step_rk2imp;
  err_target[6] = 1e-5;
  steppers[7] = gsl_odeiv2_step_rk4imp;
  err_target[7] = 1e-6;
  steppers[8] = gsl_odeiv2_step_bsimp;
  err_target[8] = 1e-6;
  steppers[9] = gsl_odeiv2_step_msadams;
  err_target[9] = 1e-5;
  steppers[10] = gsl_odeiv2_step_msbdf;
  err_target[10] = 1e-5;
  steppers[11] = 0;

  /* Loop over problems */

  for (p = 0; p < CONST_NONSTIFF_NPROB; p++)
    {
      /* Initialize */

      for (i = 0; i < MAXNS * MAXEQ; i++)
        {
          y[i] = 0.0;
        }

      for (i = 0; i < MAXNS; i++)
        {
          switch (p)
            {
            case 0:
              y[i * sd[p]] = 2.725;
              y[i * sd[p] + 1] = 1.0;
              break;
            case 1:
              y[i * sd[p]] = 1.0;
              y[i * sd[p] + 1] = 0.0;
              break;
            case 2:
              y[i * sd[p]] = 0.0;
              break;
            case 3:
              y[i * sd[p]] = 1.5;
              y[i * sd[p] + 1] = 3.0;
              break;
            default:
              gsl_test (GSL_EFAILED,
                        "test_nonstiff_problems: initialization error\n");
              return;
            }
        }

      /* Call each solver for the problem */

      for (i = 0; steppers[i] != 0; i++)
        {
          int s = sys_driver (steppers[i], prob[p], start[p], end[p],
                              initstepsize, &y[sd[p] * i],
                              epsabs, epsrel, probname[p]);

          if (s != GSL_SUCCESS)
            {
              gsl_test (s, "test_nonstiff_problems %s %s",
                        steppers[i]->name, probname[p]);
            }
        }

      /* Compare results */

      for (i = 1; steppers[i] != 0; i++)
        for (k = 0; k < sd[p]; k++)
          {
            const double val1 = y[k];
            const double val2 = y[sd[p] * i + k];
            gsl_test_rel (val1, val2,
                          (GSL_MAX (err_target[0], err_target[i])),
                          "%s/%s %s [%d]",
                          steppers[0]->name, steppers[i]->name,
                          probname[p], k);
          }
    }
}

void
test_stiff_problems (void)
{
  /* Compares output of stiff problems with several steppers */

  const gsl_odeiv2_step_type *steppers[MAXNS];

  /* Required error tolerance for each stepper. */
  double err_target[MAXNS];

  /* initial values for each ode-solver */
  double y[MAXEQ * MAXNS];

  size_t i, k, p;

  /* Number of problems to test */
#define CONST_STIFF_NPROB 3

  /* Problems, their names and number of equations */
  const gsl_odeiv2_system *prob[] =
    { &rhs_func_oregonator, &rhs_func_e5, &rhs_func_robertson };
  const char *probname[] = { "oregonator", "e5", "robertson" };
  const size_t sd[] = { 3, 4, 3 };

  /* Integration interval for problems */
  const double start[CONST_STIFF_NPROB] = { 0.0 };
  const double end[] = { 360.0, 1e4, 1e4 };

  const double epsabs = 1e-40;
  const double epsrel = 1e-7;
  const double initstepsize = 1e-5;

  /* Steppers */

  steppers[0] = gsl_odeiv2_step_bsimp;
  err_target[0] = 1e-6;
  steppers[1] = gsl_odeiv2_step_rk1imp;
  err_target[1] = 5e-3;
  steppers[2] = gsl_odeiv2_step_rk2imp;
  err_target[2] = 5e-5;
  steppers[3] = gsl_odeiv2_step_rk4imp;
  err_target[3] = 5e-5;
  steppers[4] = gsl_odeiv2_step_msbdf;
  err_target[4] = 1e-4;
  steppers[5] = 0;

  /* Loop over problems */

  for (p = 0; p < CONST_STIFF_NPROB; p++)
    {
      /* Initialize */

      for (i = 0; i < MAXNS * MAXEQ; i++)
        {
          y[i] = 0.0;
        }

      for (i = 0; i < MAXNS; i++)
        {
          switch (p)
            {
            case 0:
              y[i * sd[p]] = 1.0;
              y[i * sd[p] + 1] = 2.0;
              y[i * sd[p] + 2] = 3.0;
              break;
            case 1:
              y[i * sd[p]] = 1.76e-3;
              y[i * sd[p] + 1] = 0.0;
              y[i * sd[p] + 2] = 0.0;
              y[i * sd[p] + 3] = 0.0;
              break;
            case 2:
              y[i * sd[p]] = 1.0;
              y[i * sd[p] + 1] = 0.0;
              y[i * sd[p] + 2] = 0.0;
              break;
            default:
              gsl_test (GSL_EFAILED,
                        "test_stiff_problems: initialization error\n");
              return;
            }
        }

      /* Call each solver for the problem */

      for (i = 0; steppers[i] != 0; i++)
        {
          int s = sys_driver (steppers[i], prob[p], start[p], end[p],
                              initstepsize, &y[sd[p] * i],
                              epsabs, epsrel, probname[p]);

          if (s != GSL_SUCCESS)
            {
              gsl_test (s, "test_stiff_problems %s %s",
                        steppers[i]->name, probname[p]);
            }
        }

      /* Compare results */

      for (i = 1; steppers[i] != 0; i++)
        for (k = 0; k < sd[p]; k++)
          {
            const double val1 = y[k];
            const double val2 = y[sd[p] * i + k];
            gsl_test_rel (val1, val2,
                          (GSL_MAX (err_target[0], err_target[i])),
                          "%s/%s %s [%d]",
                          steppers[0]->name, steppers[i]->name,
                          probname[p], k);
          }
    }
}

void
test_extreme_problems (void)
{
  /* Compares output of numerically particularly demanding problems
     with several steppers */

  const gsl_odeiv2_step_type *steppers[MAXNS];

  /* Required error tolerance for each stepper. */
  double err_target[MAXNS];

  /* initial values for each ode-solver */
  double y[MAXEQ * MAXNS];

  size_t i, k, p;

  /* Number of problems to test */
#define CONST_EXTREME_NPROB 3

  /* Problems, their names and number of equations */
  const gsl_odeiv2_system *prob[] =
    { &rhs_func_e5, &rhs_func_robertson, &rhs_func_ringmod };
  const char *probname[] = { "e5_bigt", "robertson_bigt", "ringmod" };
  const size_t sd[] = { 4, 3, NRINGMOD };

  /* Integration interval for problems */
  
  const double start[CONST_EXTREME_NPROB] = { 0.0 };
  const double end[CONST_EXTREME_NPROB] = { 1e11, 1e11, 1e-5 };

  const double epsabs[CONST_EXTREME_NPROB] =
    { 1e1 * GSL_DBL_MIN, 1e1 * GSL_DBL_MIN, 1e-12 };
  const double epsrel[CONST_EXTREME_NPROB] = { 1e-12, 1e-12, 1e-12 };
  const double initstepsize[CONST_EXTREME_NPROB] = { 1e-5, 1e-5, 1e-10 };

  /* Steppers */

  steppers[0] = gsl_odeiv2_step_bsimp;
  err_target[0] = 1e-3;
  steppers[1] = gsl_odeiv2_step_msbdf;
  err_target[1] = 1e-3;
  steppers[2] = 0;

  /* Loop over problems */

  for (p = 0; p < CONST_EXTREME_NPROB; p++)
    {
      /* Initialize */

      for (i = 0; i < MAXNS * MAXEQ; i++)
        {
          y[i] = 0.0;
        }

      for (i = 0; i < MAXNS; i++)
        {
          switch (p)
            {
            case 0:
              y[i * sd[p]] = 1.76e-3;
              y[i * sd[p] + 1] = 0.0;
              y[i * sd[p] + 2] = 0.0;
              y[i * sd[p] + 3] = 0.0;
              break;
            case 1:
              y[i * sd[p]] = 1.0;
              y[i * sd[p] + 1] = 0.0;
              y[i * sd[p] + 2] = 0.0;
              break;
            case 2:
              {
                size_t j;
                for (j = 0; j < NRINGMOD; j++)
                  {
                    y[i * sd[p] + j] = 0.0;
                  }
              }
              break;
            default:
              gsl_test (GSL_EFAILED,
                        "test_extreme_problems: initialization error\n");
              return;
            }
        }

      /* Call each solver for the problem */

      for (i = 0; steppers[i] != 0; i++)
        {
          int s = sys_driver (steppers[i], prob[p], start[p], end[p],
                              initstepsize[p], &y[sd[p] * i],
                              epsabs[p], epsrel[p], probname[p]);

          if (s != GSL_SUCCESS)
            {
              printf ("start=%.5e, initstepsize=%.5e\n", start[p],
                      initstepsize[p]);
              gsl_test (s, "test_extreme_problems %s %s", steppers[i]->name,
                        probname[p]);
            }
        }

      /* Compare results */

      for (i = 1; steppers[i] != 0; i++)
        for (k = 0; k < sd[p]; k++)
          {
            const double val1 = y[k];
            const double val2 = y[sd[p] * i + k];
            gsl_test_rel (val1, val2,
                          (GSL_MAX (err_target[0], err_target[i])),
                          "%s/%s %s [%d]",
                          steppers[0]->name, steppers[i]->name,
                          probname[p], k);
          }
    }
}

void
test_driver (const gsl_odeiv2_step_type * T)
{
  /* Tests for gsl_odeiv2_driver object */

  int s;
  const double tol = 1e-8;
  const double hmax = 1e-2;

  double y[] = { 1.0, 0.0 };
  double t = 0.0;
  const double tinit = 0.0;
  const double t1 = 8.25;
  const double t2 = 100;
  const double t3 = -2.5;
  const size_t minsteps = ceil (t1 / hmax);
  const size_t maxsteps = 20;
  const double hmin = 1e-10;

  const unsigned long int nfsteps = 100000;
  const double hfixed = 0.000025;

  gsl_odeiv2_driver *d = gsl_odeiv2_driver_alloc_y_new (&rhs_func_sin, T,
                                                        1e-3, tol, tol);
  gsl_odeiv2_driver_set_hmax (d, hmax);

  s = gsl_odeiv2_driver_apply (d, &t, t1, y);

  if (s != GSL_SUCCESS)
    {
      gsl_test (s, "%s test_driver apply returned %d",
                gsl_odeiv2_step_name (d->s), s);
    }

  /* Test that result is correct */

  gsl_test_rel (y[0], cos (t1), d->n * tol, "%s test_driver y0",
                gsl_odeiv2_step_name (d->s));
  gsl_test_rel (y[1], sin (t1), d->n * tol, "%s test_driver y1",
                gsl_odeiv2_step_name (d->s));

  /* Test that maximum step length is obeyed */

  if (d->n < minsteps)
    {
      gsl_test (1, "%s test_driver steps %d < minsteps %d \n",
                gsl_odeiv2_step_name (d->s), d->n, minsteps);
    }
  else
    {
      gsl_test (0, "%s test_driver max step length test",
                gsl_odeiv2_step_name (d->s));
    }

  /* Test changing integration direction from forward to backward */

  gsl_odeiv2_driver_reset_hstart (d, -1e-3);

  s = gsl_odeiv2_driver_apply (d, &t, tinit, y);

  if (s != GSL_SUCCESS)
    {
      gsl_test (s, "%s test_driver apply returned %d",
                gsl_odeiv2_step_name (d->s), s);
    }

  gsl_test_rel (y[0], cos (tinit), d->n * tol,
                "%s test_driver y0", gsl_odeiv2_step_name (d->s));
  gsl_test_rel (y[1], sin (tinit), d->n * tol,
                "%s test_driver y1", gsl_odeiv2_step_name (d->s));

  gsl_odeiv2_driver_free (d);

  /* Test that maximum number of steps is obeyed */

  d = gsl_odeiv2_driver_alloc_y_new (&rhs_func_sin, T, 1e-3, tol, tol);
  gsl_odeiv2_driver_set_hmax (d, hmax);
  gsl_odeiv2_driver_set_nmax (d, maxsteps);

  s = gsl_odeiv2_driver_apply (d, &t, t2, y);

  if (d->n != maxsteps + 1)
    {
      gsl_test (1, "%s test_driver steps %d, expected %d",
                gsl_odeiv2_step_name (d->s), d->n, maxsteps + 1);
    }
  else
    {
      gsl_test (0, "%s test_driver max steps test",
                gsl_odeiv2_step_name (d->s));
    }

  gsl_odeiv2_driver_free (d);

  /* Test that minimum step length is obeyed */

  d = gsl_odeiv2_driver_alloc_y_new (&rhs_func_broken, T, 1e-3, tol, tol);

  gsl_odeiv2_driver_set_hmin (d, hmin);
  y[0] = 0.0;
  t = 0.0;

  s = gsl_odeiv2_driver_apply (d, &t, t2, y);

  if (s != GSL_ENOPROG)
    {
      gsl_test (1, "%s test_driver min step test returned %d",
                gsl_odeiv2_step_name (d->s), s);
    }
  else
    {
      gsl_test (0, "%s test_driver min step test",
                gsl_odeiv2_step_name (d->s));
    }

  gsl_odeiv2_driver_free (d);

  /* Test negative integration direction */

  d = gsl_odeiv2_driver_alloc_y_new (&rhs_func_sin, T, -1e-3, tol, tol);

  y[0] = 1.0;
  y[1] = 0.0;
  t = 2.5;

  s = gsl_odeiv2_driver_apply (d, &t, t3, y);

  {
    const double tol = 1e-3;
    const double test = fabs (t - t3);
    const double val = fabs (sin (-5.0) - y[1]);

    if (test > GSL_DBL_EPSILON)
      {
        gsl_test (1,
                  "%s test_driver negative dir diff %e, expected less than %e",
                  gsl_odeiv2_step_name (d->s), test, GSL_DBL_EPSILON);
      }
    else if (val > tol)
      {
        gsl_test (1,
                  "%s test_driver negative dir val %e, expected less than %e",
                  gsl_odeiv2_step_name (d->s), val, tol);
      }
    else
      {
        gsl_test (s, "%s test_driver negative direction test",
                  gsl_odeiv2_step_name (d->s));
      }
  }

  /* Test driver_apply_fixed_step */

  gsl_odeiv2_driver_reset_hstart (d, 1e-3);
  s = gsl_odeiv2_driver_apply_fixed_step (d, &t, hfixed, nfsteps, y);

  if (s != GSL_SUCCESS)
    {
      gsl_test (s, "%s test_driver apply_fixed_step returned %d",
                gsl_odeiv2_step_name (d->s), s);
    }

  {
    const double tol = 1e-3;
    const double val = fabs (sin (-2.5) - y[1]);

    if (fabs (t) > nfsteps * GSL_DBL_EPSILON)
      {
        gsl_test (1,
                  "%s test_driver apply_fixed_step t %e, expected less than %e",
                  gsl_odeiv2_step_name (d->s), fabs (t),
                  nfsteps * GSL_DBL_EPSILON);
      }
    else if (val > tol)
      {
        gsl_test (1,
                  "%s test_driver apply_fixed_step val %e, expected less than %e",
                  gsl_odeiv2_step_name (d->s), val, tol);
      }
    else
      {
        gsl_test (s, "%s test_driver apply_fixed_step test",
                  gsl_odeiv2_step_name (d->s));
      }
  }

  gsl_odeiv2_driver_free (d);
}

void
benchmark_precision (void)
{
  /* Tests steppers with several error tolerances and evaluates their
     performance and precision.
   */

  /* Maximum number of tolerances to be tested */
#define MAXNT 12

  const gsl_odeiv2_step_type *steppers[MAXNS];

  /* Required error tolerance for each stepper. */
  double err_target[MAXNS];

  /* initial values for each ode-solver */
  double y[MAXEQ * MAXNS * MAXNT];

  /* precise results from e.g. analytical solution */
  double yres[MAXEQ];

  size_t i, j, k, p;

  /* Number of problems to test */
#define CONST_BENCHMARK_PRECISION_NPROB 3

  /* Problems, their names and number of equations */
  const gsl_odeiv2_system *prob[] =
    { &rhs_func_sin, &rhs_func_exp, &rhs_func_stiff };
  const char *probname[] =
    { "rhs_func_sin", "rhs_func_exp", "rhs_func_stiff" };
  const size_t sd[] = { 2, 1, 2 };

  /* Integration interval for problems */
  const double start[] = { 0.0, 0.0, 0.0 };
  const double end[] = { 2.4, 8.4, 1.2 };

  const double epsabs[] = { 1e-4, 1e-6, 1e-8, 1e-10, 1e-12, 1e-14, 0 };
  const double epsrel[] = { 1e-4, 1e-6, 1e-8, 1e-10, 1e-12, 1e-14, 0 };
  const double initstepsize = 1e-5;

  /* number of function and jacobian evaluations */
  extern int nfe, nje;
  int nnfe[MAXNS * MAXNT] = { 0.0 };
  int nnje[MAXNS * MAXNT] = { 0.0 };

  /* Steppers */

  steppers[0] = gsl_odeiv2_step_rk4;
  err_target[0] = 1e-6;
  steppers[1] = gsl_odeiv2_step_rk2;
  err_target[1] = 1e-6;
  steppers[2] = gsl_odeiv2_step_rkf45;
  err_target[2] = 1e-6;
  steppers[3] = gsl_odeiv2_step_rkck;
  err_target[3] = 1e-6;
  steppers[4] = gsl_odeiv2_step_rk8pd;
  err_target[4] = 1e-6;
  steppers[5] = gsl_odeiv2_step_rk1imp;
  err_target[5] = 1e-3;
  steppers[6] = gsl_odeiv2_step_rk2imp;
  err_target[6] = 1e-5;
  steppers[7] = gsl_odeiv2_step_rk4imp;
  err_target[7] = 1e-6;
  steppers[8] = gsl_odeiv2_step_bsimp;
  err_target[8] = 1e-6;
  steppers[9] = gsl_odeiv2_step_msadams;
  err_target[9] = 1e-5;
  steppers[10] = gsl_odeiv2_step_msbdf;
  err_target[10] = 1e-5;
  steppers[11] = 0;

  /* Loop over problems */

  for (p = 0; p < CONST_BENCHMARK_PRECISION_NPROB; p++)
    {
      /* Initialize */

      for (i = 0; i < MAXNS * MAXEQ * MAXNT; i++)
        {
          y[i] = 0.0;
        }

      for (i = 0; i < MAXNS * MAXNT; i++)
        {
          switch (p)
            {
            case 0:
              y[i * sd[p]] = 1.0;
              y[i * sd[p] + 1] = 0.0;
              yres[0] = cos (2.4);
              yres[1] = sin (2.4);
              break;
            case 1:
              y[i * sd[p]] = 1.0;
              yres[0] = exp (8.4);
              break;
            case 2:
              y[i * sd[p]] = 1.0;
              y[i * sd[p] + 1] = 0.0;
              yres[0] = 2 * exp (-1.2) - exp (-1000 * 1.2);
              yres[1] = -exp (-1.2) + exp (-1000 * 1.2);
              break;
            default:
              gsl_test (GSL_EFAILED,
                        "benchmark_precision: initialization error\n");
              return;
            }
        }

      /* Call each solver for the problem */

      for (i = 0; steppers[i] != 0; i++)
        for (j = 0; epsrel[j] != 0; j++)
          {
            int s = sys_driver (steppers[i], prob[p], start[p], end[p],
                                initstepsize, &y[sd[p] * (j + i * MAXNT)],
                                epsabs[j], epsrel[j], probname[p]);

            if (s != GSL_SUCCESS)
              {
                for (k = 0; k < sd[p]; k++)
                  {
                    y[sd[p] * (j + i * MAXNT) + k] = GSL_NAN;
                  }
              }

            else
              {
                nnfe[j + i * MAXNT] = nfe;
                nnje[j + i * MAXNT] = nje;
              }
          }

      /* Print results */

      printf
        ("benchmark_precision: diff = (y_true - y) / y_true with %s\n          epsrel:   ",
         probname[p]);

      for (i = 0; epsrel[i] != 0.0; i++)
        {
          printf ("%12.0e ", epsrel[i]);
        }

      printf ("\n");

      for (i = 0; steppers[i] != 0; i++)
        {
          for (k = 0; k < sd[p]; k++)
            {
              printf ("%8s diff[%d]:   ", steppers[i]->name, (int) k);

              for (j = 0; epsrel[j] != 0; j++)
                {
                  const double diff =
                    (yres[k] - y[sd[p] * (j + i * MAXNT) + k]) / yres[k];
                  printf ("%12.5e ", diff);
                }

              printf ("\n");
            }
        }

      printf ("\n");

      /* Print number of function/jacobian evaluations */

      printf
        ("benchmark_precision: number of function/jacobian evaluations with %s\n          epsrel:   ",
         probname[p]);

      for (i = 0; epsrel[i] != 0.0; i++)
        {
          printf ("%12.0e          ", epsrel[i]);
        }

      printf ("\n");

      for (i = 0; steppers[i] != 0; i++)
        {
          printf ("%8s nfe/nje: ", steppers[i]->name);

          for (j = 0; epsrel[j] != 0; j++)
            {
              printf ("%9d %9d | ", nnfe[j + i * MAXNT], nnje[j + i * MAXNT]);
            }

          printf ("\n");
        }
      printf ("\n");

    }
}

void
test_evolve_temp (const gsl_odeiv2_step_type * T, double h, double err)
{
  /* Temporary test */

  double y[3];
  double yfin[3];

  y[0] = 1.0;
  y[1] = 0.0;
  y[2] = 0.0;
  yfin[0] = 0;
  yfin[1] = 0;
  yfin[2] = 0;
  test_evolve_system (T, &rhs_func_stiff, 0.0, 1.0, h, y, yfin, err, "temp");
}

/**********************************************************/
/* Main function                                          */
/**********************************************************/

int
main (void)
{

  /* Benchmark routine to compare stepper performance */
  /* benchmark_precision(); return 0; */

  /* Test single problem, for debugging purposes */
  /* test_evolve_temp (gsl_odeiv2_step_msadams, 1e-3, 1e-8); return 0; */
  int i;

  struct ptype
  {
    const gsl_odeiv2_step_type *type;
    double h;
  }
  p[MAXNS];

  struct ptype explicit_stepper[MAXNS];

  p[0].type = gsl_odeiv2_step_rk4;
  p[0].h = 1.0e-3;
  p[1].type = gsl_odeiv2_step_rk2;
  p[1].h = 1.0e-3;
  p[2].type = gsl_odeiv2_step_rkf45;
  p[2].h = 1.0e-3;
  p[3].type = gsl_odeiv2_step_rkck;
  p[3].h = 1.0e-3;
  p[4].type = gsl_odeiv2_step_rk8pd;
  p[4].h = 1.0e-3;
  p[5].type = gsl_odeiv2_step_rk1imp;
  p[5].h = 1.0e-3;
  p[6].type = gsl_odeiv2_step_rk2imp;
  p[6].h = 1.0e-3;
  p[7].type = gsl_odeiv2_step_rk4imp;
  p[7].h = 1.0e-3;
  p[8].type = gsl_odeiv2_step_bsimp;
  p[8].h = 1.0e-3;
  p[9].type = gsl_odeiv2_step_msadams;
  p[9].h = 1.0e-3;
  p[10].type = gsl_odeiv2_step_msbdf;
  p[10].h = 1.0e-3;
  p[11].type = 0;

  gsl_ieee_env_setup ();

  /* Basic tests for all steppers */

  for (i = 0; p[i].type != 0; i++)
    {
      test_stepper (p[i].type);
      test_evolve_linear (p[i].type, p[i].h, 1e-10);
      test_evolve_exp (p[i].type, p[i].h, 1e-5);
      test_evolve_sin (p[i].type, p[i].h, 1e-8);
      test_evolve_xsin (p[i].type, p[i].h, 1e-8);
      test_evolve_xsin (p[i].type, 0.1, 1e-8);
      test_evolve_stiff1 (p[i].type, p[i].h, 1e-7);
      test_evolve_stiff5 (p[i].type, p[i].h, 1e-7);
      test_evolve_negative_h (p[i].type, p[i].h, 1e-7);
      test_broken (p[i].type, p[i].h, 1e-8);
      test_stepsize_fail (p[i].type, p[i].h);
      test_user_break (p[i].type, p[i].h);
      test_driver (p[i].type);
    }

  /* Derivative test for explicit steppers */

  explicit_stepper[0].type = gsl_odeiv2_step_rk4;
  explicit_stepper[0].h = 1.0e-3;
  explicit_stepper[1].type = gsl_odeiv2_step_rk2;
  explicit_stepper[1].h = 1.0e-3;
  explicit_stepper[2].type = gsl_odeiv2_step_rkf45;
  explicit_stepper[2].h = 1.0e-3;
  explicit_stepper[3].type = gsl_odeiv2_step_rkck;
  explicit_stepper[3].h = 1.0e-3;
  explicit_stepper[4].type = gsl_odeiv2_step_rk8pd;
  explicit_stepper[4].h = 1.0e-3;
  explicit_stepper[5].type = gsl_odeiv2_step_msadams;
  explicit_stepper[5].h = 1.0e-3;
  explicit_stepper[6].type = 0;

  for (i = 0; explicit_stepper[i].type != 0; i++)
    {
      test_stepfn (explicit_stepper[i].type);
      test_stepfn2 (explicit_stepper[i].type);
    }

  /* Special tests */

  test_nonstiff_problems ();

  test_stiff_problems ();

  test_extreme_problems ();

  exit (gsl_test_summary ());
}
