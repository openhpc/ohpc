/* min/test.h
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

gsl_function create_function (double (*f)(double, void *));

void
test_f_e (const gsl_min_fminimizer_type * T, 
          const char * description, gsl_function *f,
          double lower_bound, double minimum, double upper_bound, 
          double correct_minimum);

void
test_f (const gsl_min_fminimizer_type * T, 
        const char * description, gsl_function *f,
        double lower_bound, double middle, double upper_bound, 
        double correct_minimum);

int
test_bracket (const char * description,gsl_function *f,double lower_bound, 
              double upper_bound, unsigned int max);

double f_cos (double x, void * p);
double func1 (double x, void * p);
double func2 (double x, void * p);
double func3 (double x, void * p);
double func4 (double x, void * p);
