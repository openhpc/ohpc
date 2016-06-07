/* integration/tests.h
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

double f1 (double x, void * params);
double f2 (double x, void * params);
double f3 (double x, void * params);
double f4 (double x, void * params);
double f5 (double x, void * params);

double f6 (double x, void * params);
double f7 (double x, void * params);
double f8 (double x, void * params);
double f9 (double x, void * params);
double f10 (double x, void * params);

double f11 (double x, void * params);
double f12 (double x, void * params);
double f13 (double x, void * params);
double f14 (double x, void * params);
double f15 (double x, void * params);

double f16 (double x, void * params);
double f17 (double x, void * params);

double f454 (double x, void * params);
double f455 (double x, void * params);
double f456 (double x, void * params);
double f457 (double x, void * params);
double f458 (double x, void * params);
double f459 (double x, void * params);

double myfn1 (double x, void * params);
double myfn2 (double x, void * params);

struct monomial_params {
    int degree;
    double constant;
} ;
double f_monomial(double x, void * params);
double integ_f_monomial(double a, double b, struct monomial_params * p);

double f_sin(double x, void * params);
double integ_f_sin(double a, double b);

double cqf1 ( double x , void *params );
double cqf2 ( double x , void *params );
double cqf3 ( double x , void *params );
double cqf4 ( double x , void *params );
double cqf5 ( double x , void *params );
double cqf6 ( double x , void *params );
double cqf7 ( double x , void *params );
double cqf8 ( double x , void *params );
double cqf9 ( double x , void *params );
double cqf10 ( double x , void *params );
double cqf11 ( double x , void *params );
double cqf12 ( double x , void *params );
double cqf13 ( double x , void *params );
double cqf14 ( double x , void *params );
double cqf15 ( double x , void *params );
double cqf16 ( double x , void *params );
double cqf17 ( double x , void *params );
double cqf18 ( double x , void *params );
double cqf19 ( double x , void *params );
double cqf20 ( double x , void *params );
double cqf21 ( double x , void *params );
double cqf22 ( double x , void *params );
double cqf23 ( double x , void *params );
double cqf24 ( double x , void *params );
double cqf25 ( double x , void *params );
