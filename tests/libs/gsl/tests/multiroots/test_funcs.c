/* multiroots/test_funcs.c
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
#include <math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_multiroots.h>

#include "test_funcs.h"

/* For information on testing see the following paper,

   J.J More, B.S. Garbow, K.E. Hillstrom, "Testing Unconstrained
   Optimization Software", ACM Transactions on Mathematical Software,
   Vol 7, No 1, (1981) p 17-41

   */

/* Rosenbrock Function */

gsl_multiroot_function_fdf rosenbrock =
{&rosenbrock_f,
 &rosenbrock_df,
 &rosenbrock_fdf,
 2, 0};

void
rosenbrock_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, -1.2);
  gsl_vector_set (x, 1, 1.0);
}

int
rosenbrock_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);

  double y0 = 1 - x0;
  double y1 = 10 * (x1 - x0 * x0);

  gsl_vector_set (f, 0, y0);
  gsl_vector_set (f, 1, y1);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
rosenbrock_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double x0 = gsl_vector_get (x, 0);

  double df00 = -1;
  double df01 = 0;
  double df10 = -20 * x0;
  double df11 = 10;

  gsl_matrix_set (df, 0, 0, df00);
  gsl_matrix_set (df, 0, 1, df01);
  gsl_matrix_set (df, 1, 0, df10);
  gsl_matrix_set (df, 1, 1, df11);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
rosenbrock_fdf (const gsl_vector * x, void *params,
                gsl_vector * f, gsl_matrix * df)
{
  rosenbrock_f (x, params, f);
  rosenbrock_df (x, params, df);

  return GSL_SUCCESS;
}


/* Freudenstein and Roth function */

gsl_multiroot_function_fdf roth =
{&roth_f,
 &roth_df,
 &roth_fdf,
 2, 0};

void
roth_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, 4.5);  /* changed from the value in the paper */
  gsl_vector_set (x, 1, 3.5);  /* otherwise the problem is too hard */
}

int
roth_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);

  double y0 = -13.0 + x0 + ((5.0 - x1)*x1 - 2.0)*x1;
  double y1 = -29.0 + x0 + ((x1 + 1.0)*x1 - 14.0)*x1;

  gsl_vector_set (f, 0, y0);
  gsl_vector_set (f, 1, y1);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
roth_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double x1 = gsl_vector_get (x, 1);

  double df00 = 1;
  double df01 = -3 * x1 * x1 + 10 * x1 - 2;
  double df10 = 1;
  double df11 = 3 * x1 * x1 + 2 * x1 - 14;

  gsl_matrix_set (df, 0, 0, df00);
  gsl_matrix_set (df, 0, 1, df01);
  gsl_matrix_set (df, 1, 0, df10);
  gsl_matrix_set (df, 1, 1, df11);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
roth_fdf (const gsl_vector * x, void *params,
                gsl_vector * f, gsl_matrix * df)
{
  roth_f (x, params, f);
  roth_df (x, params, df);

  return GSL_SUCCESS;
}



/* Powell badly scaled function */

gsl_multiroot_function_fdf powellscal =
{&powellscal_f,
 &powellscal_df,
 &powellscal_fdf,
 2, 0};

void
powellscal_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, 0.0);
  gsl_vector_set (x, 1, 1.0);
}

int
powellscal_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);

  double y0 = 10000.0 * x0 * x1 - 1.0;
  double y1 = exp (-x0) + exp (-x1) - 1.0001;

  gsl_vector_set (f, 0, y0);
  gsl_vector_set (f, 1, y1);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
powellscal_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);

  double df00 = 10000.0 * x1, df01 = 10000.0 * x0;
  double df10 = -exp (-x0), df11 = -exp (-x1);

  gsl_matrix_set (df, 0, 0, df00);
  gsl_matrix_set (df, 0, 1, df01);
  gsl_matrix_set (df, 1, 0, df10);
  gsl_matrix_set (df, 1, 1, df11);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
powellscal_fdf (const gsl_vector * x, void *params,
                  gsl_vector * f, gsl_matrix * df)
{
  powellscal_f (x, params, f);
  powellscal_df (x, params, df);

  return GSL_SUCCESS;
}


/* Brown badly scaled function */

gsl_multiroot_function_fdf brownscal =
{&brownscal_f,
 &brownscal_df,
 &brownscal_fdf,
 2, 0};

void
brownscal_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, 1.0);
  gsl_vector_set (x, 1, 1.0);
}

int
brownscal_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);

  double y0 = x0 - 1e6;
  double y1 = x0 * x1 - 2;

  gsl_vector_set (f, 0, y0);
  gsl_vector_set (f, 1, y1);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
brownscal_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);

  double df00 = 1.0, df01 = 0.0;
  double df10 = x1, df11 = x0;

  gsl_matrix_set (df, 0, 0, df00);
  gsl_matrix_set (df, 0, 1, df01);
  gsl_matrix_set (df, 1, 0, df10);
  gsl_matrix_set (df, 1, 1, df11);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
brownscal_fdf (const gsl_vector * x, void *params,
                  gsl_vector * f, gsl_matrix * df)
{
  brownscal_f (x, params, f);
  brownscal_df (x, params, df);

  return GSL_SUCCESS;
}


/* Powell Singular Function */

gsl_multiroot_function_fdf powellsing =
{&powellsing_f,
 &powellsing_df,
 &powellsing_fdf,
 4, 0};

void
powellsing_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, 3.0);
  gsl_vector_set (x, 1, -1.0);
  gsl_vector_set (x, 2, 0.0);
  gsl_vector_set (x, 3, 1.0);
}

int
powellsing_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);
  double x2 = gsl_vector_get (x, 2);
  double x3 = gsl_vector_get (x, 3);

  double y0 = x0 + 10 * x1;
  double y1 = sqrt (5.0) * (x2 - x3);
  double y2 = pow (x1 - 2 * x2, 2.0);
  double y3 = sqrt (10.0) * pow (x0 - x3, 2.0);

  gsl_vector_set (f, 0, y0);
  gsl_vector_set (f, 1, y1);
  gsl_vector_set (f, 2, y2);
  gsl_vector_set (f, 3, y3);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
powellsing_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);
  double x2 = gsl_vector_get (x, 2);
  double x3 = gsl_vector_get (x, 3);

  double df00 = 1, df01 = 10, df02 = 0, df03 = 0;
  double df10 = 0, df11 = 0, df12 = sqrt (5.0), df13 = -df12;
  double df20 = 0, df21 = 2 * (x1 - 2 * x2), df22 = -2 * df21, df23 = 0;
  double df30 = 2 * sqrt (10.0) * (x0 - x3), df31 = 0, df32 = 0, df33 = -df30;

  gsl_matrix_set (df, 0, 0, df00);
  gsl_matrix_set (df, 0, 1, df01);
  gsl_matrix_set (df, 0, 2, df02);
  gsl_matrix_set (df, 0, 3, df03);

  gsl_matrix_set (df, 1, 0, df10);
  gsl_matrix_set (df, 1, 1, df11);
  gsl_matrix_set (df, 1, 2, df12);
  gsl_matrix_set (df, 1, 3, df13);

  gsl_matrix_set (df, 2, 0, df20);
  gsl_matrix_set (df, 2, 1, df21);
  gsl_matrix_set (df, 2, 2, df22);
  gsl_matrix_set (df, 2, 3, df23);

  gsl_matrix_set (df, 3, 0, df30);
  gsl_matrix_set (df, 3, 1, df31);
  gsl_matrix_set (df, 3, 2, df32);
  gsl_matrix_set (df, 3, 3, df33);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
powellsing_fdf (const gsl_vector * x, void *params,
                    gsl_vector * f, gsl_matrix * df)
{
  powellsing_f (x, params, f);
  powellsing_df (x, params, df);

  return GSL_SUCCESS;
}


/* Wood function */

gsl_multiroot_function_fdf wood =
{&wood_f,
 &wood_df,
 &wood_fdf,
 4, 0};

void
wood_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, -3.0);
  gsl_vector_set (x, 1, -1.0);
  gsl_vector_set (x, 2, -3.0);
  gsl_vector_set (x, 3, -1.0);
}

int
wood_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);
  double x2 = gsl_vector_get (x, 2);
  double x3 = gsl_vector_get (x, 3);

  double t1 = x1 - x0 * x0;
  double t2 = x3 - x2 * x2;

  double y0 = -200.0 * x0 * t1 - (1 - x0);
  double y1 = 200.0 * t1 + 20.2 * (x1 - 1) + 19.8 * (x3 - 1);
  double y2 = -180.0 * x2 * t2 - (1 - x2);
  double y3 = 180.0 * t2 + 20.2 * (x3 - 1) + 19.8 * (x1 - 1);

  gsl_vector_set (f, 0, y0);
  gsl_vector_set (f, 1, y1);
  gsl_vector_set (f, 2, y2);
  gsl_vector_set (f, 3, y3);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
wood_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);
  double x2 = gsl_vector_get (x, 2);
  double x3 = gsl_vector_get (x, 3);

  double t1 = x1 - 3 * x0 * x0;
  double t2 = x3 - 3 * x2 * x2;

  double df00 = -200.0 * t1 + 1, df01 = -200.0 * x0, df02 = 0, df03 = 0;
  double df10 = -400.0*x0, df11 = 200.0 + 20.2, df12 = 0, df13 = 19.8;
  double df20 = 0, df21 = 0, df22 = -180.0 * t2 + 1, df23 = -180.0 * x2;
  double df30 = 0, df31 = 19.8, df32 = -2 * 180 * x2, df33 = 180.0 + 20.2;

  gsl_matrix_set (df, 0, 0, df00);
  gsl_matrix_set (df, 0, 1, df01);
  gsl_matrix_set (df, 0, 2, df02);
  gsl_matrix_set (df, 0, 3, df03);

  gsl_matrix_set (df, 1, 0, df10);
  gsl_matrix_set (df, 1, 1, df11);
  gsl_matrix_set (df, 1, 2, df12);
  gsl_matrix_set (df, 1, 3, df13);

  gsl_matrix_set (df, 2, 0, df20);
  gsl_matrix_set (df, 2, 1, df21);
  gsl_matrix_set (df, 2, 2, df22);
  gsl_matrix_set (df, 2, 3, df23);

  gsl_matrix_set (df, 3, 0, df30);
  gsl_matrix_set (df, 3, 1, df31);
  gsl_matrix_set (df, 3, 2, df32);
  gsl_matrix_set (df, 3, 3, df33);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
wood_fdf (const gsl_vector * x, void *params,
                    gsl_vector * f, gsl_matrix * df)
{
  wood_f (x, params, f);
  wood_df (x, params, df);

  return GSL_SUCCESS;
}


/* Helical Valley Function */

gsl_multiroot_function_fdf helical =
{&helical_f,
 &helical_df,
 &helical_fdf,
 3, 0};

void
helical_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, -1.0);
  gsl_vector_set (x, 1, 0.0);
  gsl_vector_set (x, 2, 0.0);
}

int
helical_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);
  double x2 = gsl_vector_get (x, 2);

  double t1, t2;
  double y0, y1, y2;

  if (x0 > 0) 
    {
      t1 = atan(x1/x0) / (2.0 * M_PI);
    }
  else if (x0 < 0)
    {
      t1 = 0.5 + atan(x1/x0) / (2.0 * M_PI);
    }
  else
    {
      t1 = 0.25 * (x1 > 0 ? +1 : -1);
    }

  t2 = sqrt(x0*x0 + x1*x1) ;
  
  y0 = 10 * (x2 - 10 * t1);
  y1 = 10 * (t2 - 1);
  y2 = x2 ;

  gsl_vector_set (f, 0, y0);
  gsl_vector_set (f, 1, y1);
  gsl_vector_set (f, 2, y2);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
helical_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);

  double t = x0 * x0 + x1 * x1 ;
  double t1 = 2 * M_PI * t ;
  double t2 = sqrt(t) ;

  double df00 = 100*x1/t1, df01 = -100.0 * x0/t1, df02 = 10.0;
  double df10 = 10*x0/t2, df11 = 10*x1/t2, df12 = 0;
  double df20 = 0, df21 = 0, df22 = 1.0;

  gsl_matrix_set (df, 0, 0, df00);
  gsl_matrix_set (df, 0, 1, df01);
  gsl_matrix_set (df, 0, 2, df02);

  gsl_matrix_set (df, 1, 0, df10);
  gsl_matrix_set (df, 1, 1, df11);
  gsl_matrix_set (df, 1, 2, df12);

  gsl_matrix_set (df, 2, 0, df20);
  gsl_matrix_set (df, 2, 1, df21);
  gsl_matrix_set (df, 2, 2, df22);

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
helical_fdf (const gsl_vector * x, void *params,
                    gsl_vector * f, gsl_matrix * df)
{
  helical_f (x, params, f);
  helical_df (x, params, df);

  return GSL_SUCCESS;
}


/* Discrete Boundary Value Function */

#define N 10

gsl_multiroot_function_fdf dbv =
{&dbv_f,
 &dbv_df,
 &dbv_fdf,
 N, 0};

void
dbv_initpt (gsl_vector * x)
{
  size_t i;
  double h = 1.0 / (N + 1.0);

  for (i = 0; i < N; i++)
    {
      double t = (i + 1) * h;
      double z = t * (t - 1);
      gsl_vector_set (x, i, z);
    }
}

int
dbv_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  size_t i;

  double h = 1.0 / (N + 1.0);

  for (i = 0; i < N; i++)
    {
      double z, ti = (i + 1) * h;
      double xi = 0, xim1 = 0, xip1 = 0;

      xi = gsl_vector_get (x, i);
      
      if (i > 0)
        xim1 = gsl_vector_get (x, i - 1);

      if (i < N - 1)
        xip1 = gsl_vector_get (x, i + 1);

      z = 2 * xi - xim1 - xip1 + h * h * pow(xi + ti + 1, 3.0) / 2.0;

      gsl_vector_set (f, i, z);

    }

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
dbv_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  size_t i, j;

  double h = 1.0 / (N + 1.0);

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      gsl_matrix_set (df, i, j, 0.0);

  for (i = 0; i < N; i++)
    {
      double dz_dxi, ti = (i + 1) * h;

      double xi = gsl_vector_get (x, i);
      
      dz_dxi = 2.0 + (3.0 / 2.0) * h * h * pow(xi + ti + 1, 2.0) ;
      
      gsl_matrix_set (df, i, i, dz_dxi);

      if (i > 0)
        gsl_matrix_set (df, i, i-1, -1.0);

      if (i < N - 1)
        gsl_matrix_set (df, i, i+1, -1.0);

    }

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
dbv_fdf (const gsl_vector * x, void *params,
                    gsl_vector * f, gsl_matrix * df)
{
  dbv_f (x, params, f);
  dbv_df (x, params, df);

  return GSL_SUCCESS;
}

/* Trigonometric Function */

gsl_multiroot_function_fdf trig =
{&trig_f,
 &trig_df,
 &trig_fdf,
 N, 0};

void
trig_initpt (gsl_vector * x)
{
  size_t i;

  for (i = 0; i < N; i++)       /* choose an initial point which converges */
    {
      gsl_vector_set (x, i, 0.05);   
    }
}

int
trig_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  size_t i;
  double sum = 0;

  for (i = 0; i < N; i++)
    {
      sum += cos(gsl_vector_get(x,i));
    }

  for (i = 0; i < N; i++)
    {
      double xi = gsl_vector_get (x,i);
      double z = N - sum + (i + 1) * (1 - cos(xi)) - sin(xi);

      gsl_vector_set (f, i, z);
    }

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
trig_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  size_t i, j;

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
          double dz;
          double xi = gsl_vector_get(x, i);
          double xj = gsl_vector_get(x, j);

          if (j == i)
            dz = sin(xi) + (i + 1) * sin(xi) - cos(xi);
          else
            dz = sin(xj);
          
          gsl_matrix_set(df, i, j, dz);
        }
    }

  params = 0;                   /* avoid warning about unused parameters */

  return GSL_SUCCESS;
}

int
trig_fdf (const gsl_vector * x, void *params,
                    gsl_vector * f, gsl_matrix * df)
{
  trig_f (x, params, f);
  trig_df (x, params, df);

  return GSL_SUCCESS;
}
