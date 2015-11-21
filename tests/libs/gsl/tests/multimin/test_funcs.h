/* multimin/test_funcs.h
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

extern unsigned int fcount, gcount;

typedef void (*initpt_function) (gsl_vector * x);

extern gsl_multimin_function_fdf simpleabs;
extern gsl_multimin_function simpleabs_fmin;
void simpleabs_initpt (gsl_vector * x);
void simpleabs_initpt1 (gsl_vector * x);
double simpleabs_f (const gsl_vector * x, void *params);
void simpleabs_df (const gsl_vector * x, void *params, gsl_vector * df);
void simpleabs_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df);

extern gsl_multimin_function_fdf rosenbrock;
extern gsl_multimin_function rosenbrock_fmin;
void rosenbrock_initpt (gsl_vector * x);
void rosenbrock_initpt1 (gsl_vector * x);
double rosenbrock_f (const gsl_vector * x, void *params);
void rosenbrock_df (const gsl_vector * x, void *params, gsl_vector * df);
void rosenbrock_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df);

extern gsl_multimin_function_fdf wood;
extern gsl_multimin_function wood_fmin;
void wood_initpt (gsl_vector * x);
double wood_f (const gsl_vector * x, void *params);
void wood_df (const gsl_vector * x, void *params, gsl_vector * df);
void wood_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df);

extern gsl_multimin_function_fdf roth;
extern gsl_multimin_function roth_fmin;
void roth_initpt (gsl_vector * x);
double roth_f (const gsl_vector * x, void *params);
void roth_df (const gsl_vector * x, void *params, gsl_vector * df);
void roth_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df);

extern gsl_multimin_function_fdf Nrosenbrock;
void Nrosenbrock_df (const gsl_vector * x, void *params, gsl_vector * df);
void Nrosenbrock_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df);

extern gsl_multimin_function_fdf Nroth;
void Nroth_df (const gsl_vector * x, void *params, gsl_vector * df);
void Nroth_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df);

extern gsl_multimin_function_fdf Nwood;
void Nwood_df (const gsl_vector * x, void *params, gsl_vector * df);
void Nwood_fdf (const gsl_vector * x, void *params, double * f, gsl_vector * df);

extern gsl_multimin_function spring_fmin;
void spring_initpt (gsl_vector * x);
double spring_f (const gsl_vector *x, void *params);
