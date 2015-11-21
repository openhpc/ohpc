/* multimin/test_funcs.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Fabrice Rossi
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
#include <gsl/gsl_math.h>
#include <gsl/gsl_multimin.h>

#include "test_funcs.h"

gsl_multimin_function_fdf simpleabs =
{&simpleabs_f,
 &simpleabs_df,
 &simpleabs_fdf,
 2, 0};

gsl_multimin_function simpleabs_fmin =
{&simpleabs_f,
 2, 0};

void simpleabs_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, 1.0);
  gsl_vector_set (x, 1, 2.0);
}

void simpleabs_initpt1 (gsl_vector * x)
{
  gsl_vector_set (x, 0, 1.0);
  gsl_vector_set (x, 1, 1.0);
}

double simpleabs_f (const gsl_vector * x, void *params)
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  double a = u - 1;
  double b = v - 2;
  fcount++;
  return fabs(a) + fabs(b);
}

void simpleabs_df (const gsl_vector * x, void *params, gsl_vector * df)
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  gcount++;
  gsl_vector_set(df,0, GSL_SIGN(u-1));
  gsl_vector_set(df,1, GSL_SIGN(v-2));  
}

void simpleabs_fdf (const gsl_vector * x, void *params, double * f,
                     gsl_vector * df) 
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  double a = u - 1;
  double b = v - 2;
  gcount++;
  *f = fabs(a) + fabs(b);
  gsl_vector_set(df,0, GSL_SIGN(u-1));
  gsl_vector_set(df,1, GSL_SIGN(v-2));  
}

gsl_multimin_function_fdf rosenbrock =
{&rosenbrock_f,
 &rosenbrock_df,
 &rosenbrock_fdf,
 2, 0};

gsl_multimin_function rosenbrock_fmin =
{&rosenbrock_f,
 2, 0};

void rosenbrock_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, -1.2);
  gsl_vector_set (x, 1, 1.0);
}

void rosenbrock_initpt1 (gsl_vector * x)
{
  gsl_vector_set (x, 0, 1.0);
  gsl_vector_set (x, 1, 1.0);
}

double rosenbrock_f (const gsl_vector * x, void *params)
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  double a = u - 1;
  double b = u * u - v;
  fcount++;
  return a * a + 10 * b * b;
}

void rosenbrock_df (const gsl_vector * x, void *params, gsl_vector * df)
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  double b = u * u - v;
  gcount++;
  gsl_vector_set(df,0,2 * (u - 1) + 40 * u * b);
  gsl_vector_set(df,1,-20 * b);  
}

void rosenbrock_fdf (const gsl_vector * x, void *params, double * f,
                     gsl_vector * df) 
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  double a = u - 1;
  double b = u * u - v;
  gcount++;
  *f = a * a + 10 * b * b;
  gsl_vector_set(df,0,2 * (u - 1) + 40 * u * b);
  gsl_vector_set(df,1,-20 * b);  
}

gsl_multimin_function_fdf roth =
{&roth_f,
 &roth_df,
 &roth_fdf,
 2, 0};

gsl_multimin_function roth_fmin =
{&roth_f,
 2, 0};

void roth_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, 4.5);
  gsl_vector_set (x, 1, 3.5);
}

double roth_f (const gsl_vector * x, void *params)
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  double a = -13.0 + u + ((5.0 - v)*v - 2.0)*v;
  double b = -29.0 + u + ((v + 1.0)*v - 14.0)*v;
  fcount++;
  return a * a + b * b;
}

void roth_df (const gsl_vector * x, void *params, gsl_vector * df)
{
  double u = gsl_vector_get(x,0);
  double v = gsl_vector_get(x,1);
  double a = -13.0 + u + ((5.0 - v)*v - 2.0)*v;
  double b = -29.0 + u + ((v + 1.0)*v - 14.0)*v;
  double c = -2 + v * (10 - 3 * v);
  double d = -14 + v * (2 + 3 * v);
  gcount++;
  gsl_vector_set(df,0,2 * a + 2 * b);
  gsl_vector_set(df,1,2 * a * c + 2 * b * d);
}

void roth_fdf (const gsl_vector * x, void *params, double * f,
               gsl_vector * df) 
{
  *f = roth_f (x,params);
  roth_df(x,params,df);
}

gsl_multimin_function_fdf wood =
{&wood_f,
 &wood_df,
 &wood_fdf,
 4, 0};

gsl_multimin_function wood_fmin =
{&wood_f,
 4, 0};

void
wood_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, -3.0);
  gsl_vector_set (x, 1, -1.0);
  gsl_vector_set (x, 2, -3.0);
  gsl_vector_set (x, 3, -1.0);
}

double wood_f (const gsl_vector * x, void *params)
{
  double u1 = gsl_vector_get(x,0);
  double u2 = gsl_vector_get(x,1);
  double u3 = gsl_vector_get(x,2);
  double u4 = gsl_vector_get(x,3);

  double t1 = u1 * u1 - u2;
  double t2 = u3 * u3 - u4;
  fcount++;
  return 100 * t1 * t1 + (1 - u1) * (1 - u1)
    + 90 * t2 * t2 + (1 - u3) * (1 - u3)
    + 10.1 * ( (1 - u2) * (1 - u2) + (1 - u4) * (1 - u4) )
    + 19.8 * (1 - u2) * (1 - u4);
}

void wood_df (const gsl_vector * x, void *params, gsl_vector * df)
{
  double u1 = gsl_vector_get(x,0);
  double u2 = gsl_vector_get(x,1);
  double u3 = gsl_vector_get(x,2);
  double u4 = gsl_vector_get(x,3);

  double t1 = u1 * u1 - u2;
  double t2 = u3 * u3 - u4;
  gcount++;
  gsl_vector_set(df,0, 400 * u1 * t1 - 2 * (1 - u1) );
  gsl_vector_set(df,1, -200 * t1 - 20.2 * (1 - u2) - 19.8 * (1 - u4) );
  gsl_vector_set(df,2, 360 * u3 * t2 - 2 * (1 - u3) );
  gsl_vector_set(df,3, -180 * t2 - 20.2 * (1 - u4) - 19.8 * (1 - u2) );
  
}

void wood_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df)
{
  wood_df(x,params,df);
  *f=wood_f(x,params);
}

gsl_multimin_function_fdf Nrosenbrock =
{&rosenbrock_f,
 &Nrosenbrock_df,
 &Nrosenbrock_fdf,
 2, 0};

void Nrosenbrock_df (const gsl_vector * x, void *params, gsl_vector * df)
{
  gsl_multimin_function F ;
  F.f = rosenbrock_f;
  F.params = params;
  F.n = x->size;
  gsl_multimin_diff (&F, x, df);
}

void Nrosenbrock_fdf (const gsl_vector * x, void *params, double * f,
                     gsl_vector * df) 
{
  *f = rosenbrock_f (x, params);
  Nrosenbrock_df (x, params, df);
}

gsl_multimin_function_fdf Nroth =
{&roth_f,
 &Nroth_df,
 &Nroth_fdf,
 2, 0};

void Nroth_df (const gsl_vector * x, void *params, gsl_vector * df)
{
  gsl_multimin_function F ;
  F.f = roth_f;
  F.params = params;
  F.n = x->size;
  gsl_multimin_diff (&F, x, df);
}

void Nroth_fdf (const gsl_vector * x, void *params, double * f,
                     gsl_vector * df) 
{
  *f = roth_f (x, params);
  Nroth_df (x, params, df);
}


gsl_multimin_function_fdf Nwood =
{&wood_f,
 &Nwood_df,
 &Nwood_fdf,
 4, 0};

void Nwood_df (const gsl_vector * x, void *params, gsl_vector * df)
{
  gsl_multimin_function F ;
  F.f = wood_f;
  F.params = params;
  F.n = x->size;
  gsl_multimin_diff (&F, x, df);
}

void Nwood_fdf (const gsl_vector * x, void *params, double * f,
                     gsl_vector * df) 
{
  *f = wood_f (x, params);
  Nwood_df (x, params, df);
}


gsl_multimin_function spring_fmin = { &spring_f,
  3, 0
};

void
spring_initpt (gsl_vector * x)
{
  gsl_vector_set (x, 0, 1.0);
  gsl_vector_set (x, 1, 0.0);
  gsl_vector_set (x, 2, 7.0 * M_PI);
}

double
spring_f (const gsl_vector * x, void *params)
{
  double x0 = gsl_vector_get (x, 0);
  double x1 = gsl_vector_get (x, 1);
  double x2 = gsl_vector_get (x, 2);

  double theta = atan2 (x1, x0);
  double r = sqrt (x0 * x0 + x1 * x1);
  double z = x2;
  while (z > M_PI)
    z -= 2.0 * M_PI;
  while (z < -M_PI)
    z += 2.0 * M_PI;
  {
    double tmz = theta - z;
    double rm1 = r - 1.0;
    double ret = 0.1 * (expm1 (tmz * tmz + rm1 * rm1) + fabs (x2 / 10.0));
    return ret;
  }
}


