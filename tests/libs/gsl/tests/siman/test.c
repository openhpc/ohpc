/* siman/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Mark Galassi
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
#include <string.h>
#include <math.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_siman.h>
#include <gsl/gsl_ieee_utils.h>
#include <stdio.h>

/* set up parameters for this simulated annealing run */
#define N_TRIES 200             /* how many points do we try before stepping */
#define ITERS_FIXED_T 1000      /* how many iterations for each T? */
#define STEP_SIZE 1.0           /* max step size in random walk */
#define K 1.0                   /* Boltzmann constant */
#define T_INITIAL 0.008         /* initial temperature */
#define MU_T 1.003              /* damping factor for temperature */
#define T_MIN 2.0e-6

gsl_siman_params_t params = {N_TRIES, ITERS_FIXED_T, STEP_SIZE,
                             K, T_INITIAL, MU_T, T_MIN};

double square (double x) ;
double square (double x) { return x * x ; } 

double E1(void *xp);
double M1(void *xp, void *yp);
void S1(const gsl_rng * r, void *xp, double step_size);
void P1(void *xp);

/* now some functions to test in one dimension */
double E1(void *xp)
{
  double x = * ((double *) xp);

  return exp(-square(x-1))*sin(8*x) - exp(-square(x-1000))*0.89;
}

double M1(void *xp, void *yp)
{
  double x = *((double *) xp);
  double y = *((double *) yp);

  return fabs(x - y);
}

void S1(const gsl_rng * r, void *xp, double step_size)
{
  double old_x = *((double *) xp);
  double new_x;

  new_x = gsl_rng_uniform(r)*2*step_size - step_size + old_x;

  memcpy(xp, &new_x, sizeof(new_x));
}

void P1(void *xp)
{
  printf(" %12g ", *((double *) xp));
}

int main(void)
{
  double x_min = 1.36312999455315182 ;
  double x ;

  gsl_rng * r = gsl_rng_alloc (gsl_rng_env_setup()) ;

  gsl_ieee_env_setup ();

  /* The function tested here has multiple mimima. 
     The global minimum is at    x = 1.36312999, (f = -0.87287)
     There is a local minimum at x = 0.60146196, (f = -0.84893) */

  x = -10.0 ;
  gsl_siman_solve(r, &x, E1, S1, M1, NULL, NULL, NULL, NULL,
                  sizeof(double), params);
  gsl_test_rel(x, x_min, 1e-3, "f(x)= exp(-(x-1)^2) sin(8x), x0=-10") ;

  x = +10.0 ;
  gsl_siman_solve(r, &x, E1, S1, M1, NULL, NULL, NULL, NULL,
                  sizeof(double), params);
  gsl_test_rel(x, x_min, 1e-3, "f(x)= exp(-(x-1)^2) sin(8x), x0=10") ;

  /* Start at the false minimum */

  x = +0.6 ; 
  gsl_siman_solve(r, &x, E1, S1, M1, NULL, NULL, NULL, NULL,
                  sizeof(double), params);
  gsl_test_rel(x, x_min, 1e-3, "f(x)= exp(-(x-1)^2) sin(8x), x0=0.6") ;

  x = +0.5 ; 
  gsl_siman_solve(r, &x, E1, S1, M1, NULL, NULL, NULL, NULL,
                  sizeof(double), params);
  gsl_test_rel(x, x_min, 1e-3, "f(x)= exp(-(x-1)^2) sin(8x), x0=0.5") ;

  x = +0.4 ; 
  gsl_siman_solve(r, &x, E1, S1, M1, NULL, NULL, NULL, NULL,
                  sizeof(double), params);
  gsl_test_rel(x, x_min, 1e-3, "f(x)= exp(-(x-1)^2) sin(8x), x0=0.4") ;

  gsl_rng_free(r);
  exit (gsl_test_summary ());

#ifdef JUNK 
  x0.D1 = 12.0;
  printf("#one dimensional problem, x0 = %f\n", x0.D1);
  gsl_siman_Usolve(r, &x0, test_E_1D, test_step_1D, distance_1D,
                   print_pos_1D, params);


  x0.D2[0] = 12.0;
  x0.D2[1] = 5.5;
  printf("#two dimensional problem, (x0,y0) = (%f,%f)\n",
         x0.D2[0], x0.D2[1]);
  gsl_siman_Usolve(r, &x0, test_E_2D, test_step_2D, distance_2D,
                   print_pos_2D, params); 

  x0.D3[0] = 12.2;
  x0.D3[1] = 5.5;
  x0.D3[2] = -15.5;
  printf("#three dimensional problem, (x0,y0,z0) = (%f,%f,%f)\n",
         x0.D3[0], x0.D3[1], x0.D3[2]);
  gsl_siman_Usolve(r, &x0, test_E_3D, test_step_3D, distance_3D, 
                   print_pos_3D, params); 

  x0.D2[0] = 12.2;
  x0.D2[1] = 5.5;

  gsl_siman_solve(r, &x0, test_E_2D, test_step_2D, distance_2D, print_pos_2D, params);
  
  x0.D3[0] = 12.2;
  x0.D3[1] = 5.5;
  x0.D3[2] = -15.5;

  gsl_siman_solve(r, &x0, test_E_3D, test_step_3D, distance_3D, print_pos_3D, params);

  return 0;
#endif
}



