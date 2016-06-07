/* integration/tests.c
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
#include <gsl/gsl_math.h>

#include "tests.h"

/* These are the test functions from table 4.1 of the QUADPACK book */

/* f1(x) = x^alpha * log(1/x) */
/* integ(f1,x,0,1) = 1/(alpha + 1)^2 */

double f1 (double x, void * params) {
  double alpha = *(double *) params ;
  return pow(x,alpha) * log(1/x) ;
}

/* f2(x) = 4^-alpha / ((x-pi/4)^2 + 16^-alpha) */
/* integ(f2,x,0,1) = arctan((4-pi)4^(alpha-1)) + arctan(pi 4^(alpha-1)) */

double f2 (double x, void * params) {
  double alpha = *(double *) params ;
  return pow(4.0,-alpha) / (pow((x-M_PI/4.0),2.0) + pow(16.0,-alpha)) ;
}

/* f3(x) = cos(2^alpha * sin(x)) */
/* integ(f3,x,0,pi) = pi J_0(2^alpha) */

double f3 (double x, void * params) {
  double alpha = *(double *) params ;
  return cos(pow(2.0,alpha) * sin(x)) ;
}

/* Functions 4, 5 and 6 are duplicates of functions  1, 2 and 3 */
/* ....                                                         */

/* f7(x) = |x - 1/3|^alpha */
/* integ(f7,x,0,1) = ((2/3)^(alpha+1) + (1/3)^(alpha+1))/(alpha + 1) */

double f7 (double x, void * params) {
  double alpha = *(double *) params ;
  return pow(fabs(x - (1.0/3.0)),alpha) ;
}

/* f8(x) = |x - pi/4|^alpha */
/* integ(f8,x,0,1) = 
   ((1 - pi/4)^(alpha+1) + (pi/4)^(alpha+1))/(alpha + 1) */

double f8 (double x, void * params) {
  double alpha = *(double *) params ;
  return pow(fabs(x - (M_PI/4.0)),alpha) ;
}

/* f9(x) = sqrt(1 - x^2) / (x + 1 + 2^-alpha) */
/* integ(f9,x,-1,1) = pi/sqrt((1+2^-alpha)^2-1) */

double f9 (double x, void * params) {
  double alpha = *(double *) params ;
  return 1 / ((x + 1 + pow(2.0,-alpha)) * sqrt(1-x*x)) ;
}

/* f10(x) = sin(x)^(alpha - 1) */
/* integ(f10,x,0,pi/2) = 2^(alpha-2) ((Gamma(alpha/2))^2)/Gamma(alpha) */

double f10 (double x, void * params) {
  double alpha = *(double *) params ;
  return pow(sin(x), alpha-1) ;
}

/* f11(x) = log(1/x)^(alpha - 1) */
/* integ(f11,x,0,1) = Gamma(alpha) */

double f11 (double x, void * params) {
  double alpha = *(double *) params ;
  return pow(log(1/x), alpha-1) ;
}

/* f12(x) = exp(20*(x-1)) * sin(2^alpha * x) */
/* integ(f12,x,0,1) = 
   (20 sin(2^alpha) - 2^alpha cos(2^alpha) + 2^alpha exp(-20))
   /(400 + 4^alpha) */

double f12 (double x, void * params) {
  double alpha = *(double *) params ;
  return exp(20*(x-1)) * sin(pow(2.0,alpha) * x) ;
}

/* f13(x) = cos(2^alpha * x)/sqrt(x(1 - x)) */
/* integ(f13,x,0,1) = pi cos(2^(alpha-1)) J_0(2^(alpha-1))  */

double f13 (double x, void * params) {
  double alpha = *(double *) params ;
  return cos(pow(2.0,alpha)*x)/sqrt(x*(1-x)) ;
}

double f14 (double x, void * params) {
  double alpha = *(double *) params ;
  return exp(-pow(2.0,-alpha)*x)*cos(x)/sqrt(x) ;
}

double f15 (double x, void * params) {
  double alpha = *(double *) params ;
  return x*x * exp(-pow(2.0,-alpha)*x) ;
}

double f16 (double x, void * params) {
  double alpha = *(double *) params ;
  if (x==0 && alpha == 1) return 1 ;  /* make the function continuous in x */
  if (x==0 && alpha > 1) return 0 ;   /* avoid problems with pow(0,1) */
  return pow(x,alpha-1)/pow((1+10*x),2.0) ;
}

double f17 (double x, void * params) {
  double alpha = *(double *) params ;
  return pow(2.0,-alpha)/(((x-1)*(x-1)+pow(4.0,-alpha))*(x-2)) ;
}

/* f454(x) = x^3 log|(x^2-1)(x^2-2)| */
/* integ(f454,x,0,inf) = 61 log(2) + (77/4) log(7) - 27 */

double f454 (double x, void * params) {
  double x2 = x * x;
  double x3 = x * x2;
  params = 0 ;
  return x3 * log(fabs((x2 - 1.0) * (x2 - 2.0))) ;
}

/* f455(x) = log(x)/(1+100*x^2) */
/* integ(f455,x,0,inf) = -log(10)/20 */

double f455 (double x, void * params) {
  params = 0 ;
  return log(x) / (1.0 + 100.0 * x * x) ;
}

/* f456(x) = log(x) */
/* integ(f456*sin(10 pi x),x,0,1) = -(gamma + log(10pi) - Ci(10pi))/(10pi) */

double f456 (double x, void * params) {
  params = 0 ;
  if (x == 0.0)
    {
      return 0;
    }
  return log(x) ;
}

/* f457(x) = 1/sqrt(x) */
/* integ(f457*cos(pi x / 2),x,0,+inf) = 1 */

double f457 (double x, void * params) {
  params = 0 ;
  if (x == 0.0)
    {
      return 0;
    }
  return 1/sqrt(x) ;
}

/* f458(x) = 1/(1 + log(x)^2)^2 */
/* integ(log(x) f458(x),x,0,1) = (Ci(1) sin(1) + (pi/2 - Si(1)) cos(1))/pi 
                               = -0.1892752 */

double f458 (double x, void * params) {
  params = 0 ;

  if (x == 0.0) 
    {
      return 0;
    }
  else 
    {
      double u = log(x);
      double v = 1 + u * u;
      
      return 1.0 / (v * v) ;
    }
}

/* f459(x) = 1/(5 x^3 + 6) */
/* integ(f459/(x-0),x,-1,5) = log(125/631)/18 */

double f459 (double x, void * params) {
  params = 0 ;
  return 1.0 / (5.0 * x * x * x + 6.0) ;
}

/* myfn1(x) = exp(-x - x^2) */
/* integ(myfn1,x,-inf,inf) = sqrt(pi) exp(-1/4) */

double myfn1 (double x, void * params) {
  params = 0;
  return exp(-x - x*x) ;
}

/* myfn2(x) = exp(alpha*x) */
/* integ(myfn2,x,-inf,b) = exp(alpha*b)/alpha */

double myfn2 (double x, void * params) {
  double alpha = *(double *) params ;
  return exp(alpha*x) ;
}


/* f_monomial = constant * x^degree */
double f_monomial(double x, void * params)
{
  struct monomial_params * p = (struct monomial_params *) params;

  return p->constant * gsl_pow_int(x, p->degree);
}

/* integ(f_monomial,x,a b)=constant*(b^(degree+1)-a^(degree+1))/(degree+1) */
double integ_f_monomial(double a, double b, struct monomial_params * p)
{
  const int degreep1 = p->degree + 1;
  const double bnp1 = gsl_pow_int(b, degreep1);
  const double anp1 = gsl_pow_int(a, degreep1);
  return (p->constant / degreep1)*(bnp1 - anp1);
}

/* f(x) = sin(x) */
double f_sin(double x, void * params)
{
    return sin(x);
}

/* integ(f_sin,x,a,b) */
double integ_f_sin(double a, double b)
{
    return -cos(b) + cos(a);
}


/* The test functions. */
double cqf1 ( double x , void *params ) {
  return exp(x);
}
    
double cqf2 ( double x , void *params ) {
  return x >= 0.3;
}

double cqf3 ( double x , void *params ) {
  return sqrt(x);
}


double cqf4 ( double x , void *params ) {
  return (23.0/25) * cosh(x) - cos(x);
}

double cqf5 ( double x , void *params ) {
  double x2 = x*x;
  return 1.0 / ( x2 * (x2 + 1) + 0.9);
}

double cqf6 ( double x , void *params ) {
  return x * sqrt( x );
}

double cqf7 ( double x , void *params ) {
  return 1.0 / sqrt(x);
}

double cqf8 ( double x , void *params ) {
  double x2 = x*x;
  return 1.0 / (1 + x2*x2);
}

double cqf9 ( double x , void *params ) {
  return 2.0 / (2 + sin(10*M_PI*x));
}

double cqf10 ( double x , void *params ) {
  return 1.0 / (1 + x);
}

double cqf11 ( double x , void *params ) {
  return 1.0 / (1 + exp(x));
}

double cqf12 ( double x , void *params ) {
  return x / (exp(x) - 1.0);
}

double cqf13 ( double x , void *params ) {
  return sin(100 * M_PI * x) / (M_PI * x);
}

double cqf14 ( double x , void *params ) {
  return sqrt(50.0) * exp(-50*M_PI*x*x);
}

double cqf15 ( double x , void *params ) {
  return 25.0 * exp(-25*x);
}

double cqf16 ( double x , void *params ) {
  return 50 / M_PI * (2500 * x*x + 1);
}

double cqf17 ( double x , void *params ) {
  double t1 = 50 * M_PI * x ,t2;
  t2 = sin(t1) / t1;
  return 50 * t2 * t2;
}

double cqf18 ( double x , void *params ) {
  return cos( cos(x) + 3*sin(x) + 2*cos(2*x) + 3*sin(2*x) + 3*cos(3*x) );
}

double cqf19 ( double x , void *params ) {
  return log(x);
}

double cqf20 ( double x , void *params ) {
  return 1 / (x*x + 1.005);
}

double cqf21 ( double x , void *params ) {
  return 1 / cosh( 10 * (x - 0.2) * 2 ) + 
    1 / cosh( 100 * (x - 0.4) * 4 ) + 
    1 / cosh( 1000 * (x - 0.6) * 8 );
}

double cqf22 ( double x , void *params ) {
  return 4 * M_PI*M_PI * x * sin(20*M_PI*x) * cos(2*M_PI*x);
}

double cqf23 ( double x , void *params ) {
  double t = 230*x - 30;
  return 1 / (1 + t*t);
}

double cqf24 ( double x , void *params ) {
  return floor(exp(x));
}

double cqf25 ( double x , void *params ) {
  return (x < 1) * (x + 1) + 
    (1 <= x && x <= 3) * (3 - x) + 
    (x > 3) * 2;
}
