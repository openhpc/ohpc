/* multiroots/test_funcs.h
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


typedef void (*initpt_function) (gsl_vector * x);

extern gsl_multiroot_function_fdf rosenbrock;
void rosenbrock_initpt (gsl_vector * x);
int rosenbrock_f (const gsl_vector * x, void *params, gsl_vector * f);
int rosenbrock_df (const gsl_vector * x, void *params, gsl_matrix * df);
int rosenbrock_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf roth;
void roth_initpt (gsl_vector * x);
int roth_f (const gsl_vector * x, void *params, gsl_vector * f);
int roth_df (const gsl_vector * x, void *params, gsl_matrix * df);
int roth_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf brownscal;
void brownscal_initpt (gsl_vector * x);
int brownscal_f (const gsl_vector * x, void *params, gsl_vector * f);
int brownscal_df (const gsl_vector * x, void *params, gsl_matrix * df);
int brownscal_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf powellscal;
void powellscal_initpt (gsl_vector * x);
int powellscal_f (const gsl_vector * x, void *params, gsl_vector * f);
int powellscal_df (const gsl_vector * x, void *params, gsl_matrix * df);
int powellscal_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf powellsing;
void powellsing_initpt (gsl_vector * x);
int powellsing_f (const gsl_vector * x, void *params, gsl_vector * f);
int powellsing_df (const gsl_vector * x, void *params, gsl_matrix * df);
int powellsing_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf wood;
void wood_initpt (gsl_vector * x);
int wood_f (const gsl_vector * x, void *params, gsl_vector * f);
int wood_df (const gsl_vector * x, void *params, gsl_matrix * df);
int wood_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf helical;
void helical_initpt (gsl_vector * x);
int helical_f (const gsl_vector * x, void *params, gsl_vector * f);
int helical_df (const gsl_vector * x, void *params, gsl_matrix * df);
int helical_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf dbv;
void dbv_initpt (gsl_vector * x);
int dbv_f (const gsl_vector * x, void *params, gsl_vector * f);
int dbv_df (const gsl_vector * x, void *params, gsl_matrix * df);
int dbv_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

extern gsl_multiroot_function_fdf trig;
void trig_initpt (gsl_vector * x);
int trig_f (const gsl_vector * x, void *params, gsl_vector * f);
int trig_df (const gsl_vector * x, void *params, gsl_matrix * df);
int trig_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * df);

