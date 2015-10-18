/* cdf/test.c
 * 
 * Copyright (C) 2002 Jason H Stover.
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
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

#define TEST(func, args, value, tol) { double res = func args ; gsl_test_rel (res, value, tol, #func #args); } ;

#define TEST_TOL0  (2.0*GSL_DBL_EPSILON)
#define TEST_TOL1  (16.0*GSL_DBL_EPSILON)
#define TEST_TOL2  (256.0*GSL_DBL_EPSILON)
#define TEST_TOL3  (2048.0*GSL_DBL_EPSILON)
#define TEST_TOL4  (16384.0*GSL_DBL_EPSILON)
#define TEST_TOL5  (131072.0*GSL_DBL_EPSILON)
#define TEST_TOL6  (1048576.0*GSL_DBL_EPSILON)

void test_ugaussian (void);
void test_ugaussianinv (void);
void test_exponential (void);
void test_exponentialinv (void);
void test_exppow (void);
void test_tdist (void);
void test_fdist (void);
void test_gamma (void);
void test_chisq (void);
void test_beta (void);
void test_gammainv (void);
void test_chisqinv (void);
void test_tdistinv (void);
void test_betainv (void);
void test_finv (void);

#include "test_auto.c"

struct range { unsigned int min; unsigned int max; } ;
double test_binomial_pdf (unsigned int n);
double test_binomial_cdf_P (unsigned int n);
double test_binomial_cdf_Q (unsigned int n);

struct range
test_binomial_range (void)
{
  struct range r = {0, 5};
  return r;
}

double
test_binomial_pdf (unsigned int k)
{
  return gsl_ran_binomial_pdf (k, 0.3, 5);
}

double
test_binomial_cdf_P (unsigned int k)
{
  return gsl_cdf_binomial_P (k, 0.3, 5);
}

double
test_binomial_cdf_Q (unsigned int k)
{
  return gsl_cdf_binomial_Q (k, 0.3, 5);
}

struct range
test_poisson_range (void)
{
  struct range r = {0, 25};
  return r;
}

double
test_poisson_pdf (unsigned int k)
{
  return gsl_ran_poisson_pdf (k, 2.3);
}

double
test_poisson_cdf_P (unsigned int k)
{
  return gsl_cdf_poisson_P (k, 2.3);
}

double
test_poisson_cdf_Q (unsigned int k)
{
  return gsl_cdf_poisson_Q (k, 2.3);
}

struct range
test_geometric_range (void)
{
  struct range r = {0, 25};
  return r;
}

double
test_geometric_pdf (unsigned int k)
{
  return gsl_ran_geometric_pdf (k, 0.3);
}

double
test_geometric_cdf_P (unsigned int k)
{
  return gsl_cdf_geometric_P (k, 0.3);
}

double
test_geometric_cdf_Q (unsigned int k)
{
  return gsl_cdf_geometric_Q (k, 0.3);
}

struct range
test_negative_binomial_range (void)
{
  struct range r = {0, 15};
  return r;
}

double
test_negative_binomial_pdf (unsigned int k)
{
  return gsl_ran_negative_binomial_pdf (k, 0.3, 5.3);
}

double
test_negative_binomial_cdf_P (unsigned int k)
{
  return gsl_cdf_negative_binomial_P (k, 0.3, 5.3);
}

double
test_negative_binomial_cdf_Q (unsigned int k)
{
  return gsl_cdf_negative_binomial_Q (k, 0.3, 5.3);
}

struct range
test_pascal_range (void)
{
  struct range r = {0, 15};
  return r;
}

double
test_pascal_pdf (unsigned int k)
{
  return gsl_ran_pascal_pdf (k, 0.3, 5);
}

double
test_pascal_cdf_P (unsigned int k)
{
  return gsl_cdf_pascal_P (k, 0.3, 5);
}

double
test_pascal_cdf_Q (unsigned int k)
{
  return gsl_cdf_pascal_Q (k, 0.3, 5);
}

struct range
test_hypergeometric_range (void)
{
  struct range r = {0, 26};
  return r;
}

double
test_hypergeometric_pdf (unsigned int k)
{
  return gsl_ran_hypergeometric_pdf (k, 7, 19, 13);
}

double
test_hypergeometric_cdf_P (unsigned int k)
{
  return gsl_cdf_hypergeometric_P (k, 7, 19, 13);
}

double
test_hypergeometric_cdf_Q (unsigned int k)
{
  return gsl_cdf_hypergeometric_Q (k, 7, 19, 13);
}

struct range
test_hypergeometric2_range (void)
{
  struct range r = {0, 13250474};
  return r;
}

struct range
test_hypergeometric2a_range (void)
{
  struct range r = {3500, 3600};
  return r;
}

struct range
test_hypergeometric2b_range (void)
{
  struct range r = {13247474, 13250474};
  return r;
}

double
test_hypergeometric2_pdf (unsigned int k)
{
  return gsl_ran_hypergeometric_pdf (k, 76200, 13174274, 678090);
}

double
test_hypergeometric2_cdf_P (unsigned int k)
{
  return gsl_cdf_hypergeometric_P (k, 76200, 13174274, 678090);
}

double
test_hypergeometric2_cdf_Q (unsigned int k)
{
  return gsl_cdf_hypergeometric_Q (k, 76200, 13174274, 678090);
}

#ifdef LOGARITHMIC
struct range
test_logarithmic_range (void)
{
  struct range r = {1, 200};
  return r;
}

double
test_logarithmic_pdf (unsigned int k)
{
  return gsl_ran_logarithmic_pdf (k, 0.9);
}

double
test_logarithmic_cdf_P (unsigned int k)
{
  return gsl_cdf_logarithmic_P (k, 0.9);
}

double
test_logarithmic_cdf_Q (unsigned int k)
{
  return gsl_cdf_logarithmic_Q (k, 0.9);
}
#endif

void
test_discrete_cdf_P (double (*pdf)(unsigned int), 
                     double (*cdf_P)(unsigned int), 
                     struct range (*range)(void), 
                     const char * desc)
{
  double sum;
  double tol = TEST_TOL2;
  int i, min, max;

  struct range r = range();

  min = r.min;
  max = r.max;
  sum = 0.0;

  for (i = min; i <= max; i++)
    {
      double pi = pdf(i);
      double Pi = cdf_P(i);
      sum += pi;
      gsl_test_rel (Pi, sum, tol, desc, i);
    }
}

void
test_discrete_cdf_Q (double (*pdf)(unsigned int), 
                     double (*cdf_Q)(unsigned int), 
                     struct range (*range)(void), 
                     const char * desc)
{
  double sum;
  double tol = TEST_TOL2;
  int i, min, max;

  struct range r = range();

  min = r.min;
  max = r.max;
  sum = cdf_Q(max);

  for (i = max; i >= min; i--)
    {
      double pi = pdf(i);
      double Qi = cdf_Q(i);
      gsl_test_rel (Qi, sum, tol, desc, i);
      sum += pi;
    }
}

void
test_discrete_cdf_PQ (double (*cdf_P)(unsigned int), 
                      double (*cdf_Q)(unsigned int), 
                      struct range (*range)(void), 
                      const char * desc)
{
  double sum;
  double tol = GSL_DBL_EPSILON;
  int i, min, max;

  struct range r = range();

  min = r.min;
  max = r.max;

  for (i = min; i <= max; i++)
    {
      double Pi = cdf_P(i);
      double Qi = cdf_Q(i);
      sum = Pi + Qi;
      gsl_test_rel (sum, 1.0, tol, desc, i);
      {
        int s1 = (Pi<0 || Pi>1);     
        int s2 = (Qi<0 || Qi>1);     
        gsl_test(s1, "Pi in range [0,1] (%.18e)", Pi);
        gsl_test(s2, "Qi in range [0,1] (%.18e)", Qi);
      }
    }

}

#define TEST_DISCRETE(name) do {  \
  test_discrete_cdf_P(&test_ ## name ## _pdf, &test_ ## name ## _cdf_P, &test_ ## name ## _range, "test gsl_cdf_" #name "_P (k=%d)") ; \
  test_discrete_cdf_Q(&test_ ## name ## _pdf, &test_ ## name ## _cdf_Q, &test_ ## name ## _range, "test gsl_cdf_" #name "_Q (k=%d)") ; \
  test_discrete_cdf_PQ(&test_ ## name ## _cdf_P, &test_ ## name ## _cdf_Q, &test_ ## name ## _range, "test gsl_cdf_" #name "_P+Q (k=%d)") ; \
} while (0);

int
main (void)
{
  gsl_ieee_env_setup ();

  TEST_DISCRETE(binomial);
  TEST_DISCRETE(poisson);
  TEST_DISCRETE(geometric);
  TEST_DISCRETE(negative_binomial);
  TEST_DISCRETE(pascal);
  TEST_DISCRETE(hypergeometric);
#ifdef HYPERGEOMETRIC2
  TEST_DISCRETE(hypergeometric2);
#endif
#ifdef LOGARITHMIC
  TEST_DISCRETE(logarithmic);
#endif

  test_discrete_cdf_PQ(&test_hypergeometric2_cdf_P, 
                       &test_hypergeometric2_cdf_Q, 
                       &test_hypergeometric2a_range, 
                       "test gsl_cdf_hypergeometric_P+Q (k=%d)") ; 

  test_discrete_cdf_PQ(&test_hypergeometric2_cdf_P, 
                       &test_hypergeometric2_cdf_Q, 
                       &test_hypergeometric2b_range, 
                       "test gsl_cdf_hypergeometric_P+Q (k=%d)") ; 


  /* exit (gsl_test_summary ()); */

  /* Tests for gaussian cumulative distribution function 
     Function values computed with PARI, 28 digits precision */
  
  test_ugaussian ();
  test_exponential ();
  test_exppow ();
  test_tdist (); 
  test_fdist (); 
  test_gamma ();
  test_chisq (); 
  test_beta (); 

  test_ugaussianinv ();
  test_exponentialinv ();
  test_gammainv (); 
  test_chisqinv (); 
  test_tdistinv (); 
  test_betainv ();
  test_finv ();

  test_auto_beta ();
  test_auto_fdist ();
  test_auto_cauchy ();
  test_auto_gaussian ();
  test_auto_laplace ();
  test_auto_rayleigh ();
  test_auto_flat ();
  test_auto_lognormal ();
  test_auto_gamma ();
  test_auto_chisq ();
  test_auto_tdist ();
  test_auto_gumbel1 ();
  test_auto_gumbel2 ();
  test_auto_weibull ();
  test_auto_pareto ();
  test_auto_logistic ();
  test_auto_gammalarge ();
  
  exit (gsl_test_summary ());
}

void test_ugaussian (void)
{
  TEST (gsl_cdf_ugaussian_P, (0.0), 0.5, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (1e-32), 0.5, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (1e-16), 0.5000000000000000398942280401, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (1e-8), 0.5000000039894228040143267129, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (0.5), 0.6914624612740131036377046105, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (0.7), 0.7580363477769269852506495717, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (5.0), 0.9999997133484281208060883262, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (10.0), 0.9999999999999999999999923801, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (30.0), 1.000000000000000000000000000, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (40.0), 1.000000000000000000000000000, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (1e10), 1.000000000000000000000000000, TEST_TOL0);

  TEST (gsl_cdf_ugaussian_P, (-1e-32), 0.5, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (-1e-16), 0.4999999999999999601057719598, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (-1e-8), 0.4999999960105771959856732870, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (-0.5), 0.3085375387259868963622953894, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (-0.7), 0.2419636522230730147493504282, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_P, (-5.0), 0.0000002866515718791939116737523329, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_P, (-10.0), 7.619853024160526065973343257e-24, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_P, (-30.0), 4.906713927148187059533809288e-198, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_P, (-1e10), 0.0, 0.0);

  TEST (gsl_cdf_ugaussian_Q, (0.0), 0.5, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (1e-32), 0.5, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (1e-16), 0.4999999999999999601057719598, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (1e-8), 0.4999999960105771959856732870, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (0.5), 0.3085375387259868963622953894, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (0.7), 0.2419636522230730147493504282, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (5.0), 0.0000002866515718791939116737523329, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_Q, (10.0), 7.619853024160526065973343257e-24, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_Q, (30.0), 4.906713927148187059533809288e-198, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_Q, (1e10), 0.0, 0.0);

  TEST (gsl_cdf_ugaussian_Q, (-1e-32), 0.5, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-1e-16), 0.5000000000000000398942280401, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-1e-8), 0.5000000039894228040143267129, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-0.5), 0.6914624612740131036377046105, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-0.7), 0.7580363477769269852506495717, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-5.0), 0.9999997133484281208060883262, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-10.0), 0.9999999999999999999999923801, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-30.0), 1.000000000000000000000000000, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-40.0), 1.000000000000000000000000000, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Q, (-1e10), 1.000000000000000000000000000, TEST_TOL0);
}
  /* Test values from Abramowitz & Stegun, Handbook of Mathematical
     Functions, Table 26.1.  Error term is given by dx = dP / Z(x) */

void test_ugaussianinv (void) {
  TEST (gsl_cdf_ugaussian_Pinv, (0.9999997133), 5.0, 1e-4);
  TEST (gsl_cdf_ugaussian_Pinv, (0.9999683288), 4.0, 1e-6);
  TEST (gsl_cdf_ugaussian_Pinv, (0.9986501020), 3.0, 1e-8);
  TEST (gsl_cdf_ugaussian_Pinv, (0.977249868051821), 2.0, 1e-14);
  TEST (gsl_cdf_ugaussian_Pinv, (0.841344746068543), 1.0, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_Pinv, (0.691462461274013), 0.5, TEST_TOL2);
  TEST (gsl_cdf_ugaussian_Pinv, (0.655421741610324), 0.4, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (0.617911422188953), 0.3, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (0.579259709439103), 0.2, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (0.539827837277029), 0.1, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (0.5), 0.0, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Pinv, (4.60172162722971e-1), -0.1, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (4.20740290560897e-1), -0.2, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (3.82088577811047e-1), -0.3, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (3.44578258389676e-1), -0.4, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Pinv, (3.08537538725987e-1), -0.5, TEST_TOL2);
  TEST (gsl_cdf_ugaussian_Pinv, (1.58655253931457e-1), -1.0, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_Pinv, (2.2750131948179e-2), -2.0, 1e-14);
  TEST (gsl_cdf_ugaussian_Pinv, (1.349898e-3), -3.0, 1e-8);
  TEST (gsl_cdf_ugaussian_Pinv, (3.16712e-5), -4.0, 1e-6);
  TEST (gsl_cdf_ugaussian_Pinv, (2.86648e-7), -5.0, 1e-4);

  TEST (gsl_cdf_ugaussian_Pinv, (7.61985302416052e-24), -10.0, 1e-4);

  TEST (gsl_cdf_ugaussian_Qinv, (7.61985302416052e-24), 10.0, 1e-4);

  TEST (gsl_cdf_ugaussian_Qinv, (2.86648e-7), 5.0, 1e-4);
  TEST (gsl_cdf_ugaussian_Qinv, (3.16712e-5), 4.0, 1e-6);
  TEST (gsl_cdf_ugaussian_Qinv, (1.349898e-3), 3.0, 1e-8);
  TEST (gsl_cdf_ugaussian_Qinv, (2.2750131948179e-2), 2.0, 1e-14);
  TEST (gsl_cdf_ugaussian_Qinv, (1.58655253931457e-1), 1.0, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_Qinv, (3.08537538725987e-1), 0.5, TEST_TOL2);
  TEST (gsl_cdf_ugaussian_Qinv, (3.44578258389676e-1), 0.4, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (3.82088577811047e-1), 0.3, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (4.20740290560897e-1), 0.2, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (4.60172162722971e-1), 0.1, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (0.5), 0.0, TEST_TOL0);
  TEST (gsl_cdf_ugaussian_Qinv, (0.539827837277029), -0.1, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (0.579259709439103), -0.2, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (0.617911422188953), -0.3, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (0.655421741610324), -0.4, TEST_TOL1);
  TEST (gsl_cdf_ugaussian_Qinv, (0.691462461274013), -0.5, TEST_TOL2);
  TEST (gsl_cdf_ugaussian_Qinv, (0.841344746068543), -1.0, TEST_TOL3);
  TEST (gsl_cdf_ugaussian_Qinv, (0.977249868051821), -2.0, 1e-14);
  TEST (gsl_cdf_ugaussian_Qinv, (0.9986501020), -3.0, 1e-8);
  TEST (gsl_cdf_ugaussian_Qinv, (0.9999683288), -4.0, 1e-6);
  TEST (gsl_cdf_ugaussian_Qinv, (0.9999997133), -5.0, 1e-4);
}


  /* Tests for exponential cumulative distribution function
     Function values computed with PARI, 28 digits precision */

void test_exponential (void)
{
  TEST (gsl_cdf_exponential_P, (0.1, 0.7), 1.33122100249818372e-1, TEST_TOL0);
  TEST (gsl_cdf_exponential_P, (1e-32, 0.7), 1.42857142857142857e-32, TEST_TOL0);
  TEST (gsl_cdf_exponential_P, (1000.0, 0.7), 1.0, TEST_TOL6);

  TEST (gsl_cdf_exponential_Q, (0.1, 0.7), 8.66877899750181628e-1, TEST_TOL0);
  TEST (gsl_cdf_exponential_Q, (1e-32, 0.7), 1.0, TEST_TOL0);
  TEST (gsl_cdf_exponential_Q, (1000.0, 0.7), 0.0, TEST_TOL6);
}

void test_exponentialinv (void) {
  TEST (gsl_cdf_exponential_Pinv, (0.13, 0.7), 9.74834471334553546e-2, TEST_TOL0);
  TEST (gsl_cdf_exponential_Pinv, (1.42e-32, 0.7), 9.94000000000000000e-33, TEST_TOL0);

  TEST (gsl_cdf_exponential_Qinv, (0.86, 0.7), 1.05576022814208545e-1, TEST_TOL0);
  TEST (gsl_cdf_exponential_Qinv, (0.99999, 0.7), 7.00003500023333508e-6, TEST_TOL6);
}



void test_exppow (void)
{
  TEST (gsl_cdf_exppow_P, (-1000.0, 0.7, 1.8), 0.0, TEST_TOL6);
  TEST (gsl_cdf_exppow_P, (-0.1, 0.7, 1.8), 0.4205349082867515493458053850, TEST_TOL0);
  TEST (gsl_cdf_exppow_P, (-1e-32, 0.7, 1.8), 0.4999999999999999999999999999, TEST_TOL0);

  TEST (gsl_cdf_exppow_P, (0.1, 0.7, 1.8), 0.5794650917132484506541946149, TEST_TOL0);
  TEST (gsl_cdf_exppow_P, (1e-32, 0.7, 1.8), 0.5, TEST_TOL0);
  TEST (gsl_cdf_exppow_P, (1000.0, 0.7, 1.8), 0.9999999999999999999999956212, TEST_TOL6);

  TEST (gsl_cdf_exppow_Q, (-1000.0, 0.7, 1.8), 0.9999999999999999999999956212, TEST_TOL6);
  TEST (gsl_cdf_exppow_Q, (-0.1, 0.7, 1.8), 0.5794650917132484506541946149, TEST_TOL0);
  TEST (gsl_cdf_exppow_Q, (-1e-32, 0.7, 1.8), 0.5, TEST_TOL0);

  TEST (gsl_cdf_exppow_Q, (0.1, 0.7, 1.8), 0.4205349082867515493458053850, TEST_TOL0);
  TEST (gsl_cdf_exppow_Q, (1e-32, 0.7, 1.8), 0.4999999999999999999999999999, TEST_TOL0);
  TEST (gsl_cdf_exppow_Q, (1000.0, 0.7, 1.8), 0.0, TEST_TOL6);
}


  /* Tests for student's T distribution */

  /* p(x,nu) = (1/2)*(1+sign(x)*betaI(x^2/(nu+x^2),1/2,nu/2))   
     q(x,nu) = (1/2)*(1-sign(x)*betaI(x^2/(nu+x^2),1/2,nu/2))    */

void test_tdist (void) {
  TEST (gsl_cdf_tdist_P, (0.0, 1.0), 0.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1e-100, 1.0), 0.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.001, 1.0), 5.00318309780080559e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.01, 1.0), 5.03182992764908255e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.1, 1.0), 5.31725517430553569e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.325, 1.0), 6.00023120032852123e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1.0, 1.0), 0.75000000000000000e0, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1.5, 1.0), 8.12832958189001183e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (2.0, 1.0), 8.52416382349566726e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (10.0, 1.0), 9.68274482569446430e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (20.0, 1.0), 9.84097748743823625e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (100.0, 1.0), 9.96817007235091745e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1000.0, 1.0), 9.99681690219919441e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (10000.0, 1.0), 9.99968169011487724e-1, TEST_TOL6);

  TEST (gsl_cdf_tdist_Q, (0.0, 1.0), 0.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1e-100, 1.0), 0.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.001, 1.0), 4.99681690219919441e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.01, 1.0), 4.96817007235091745e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.1, 1.0), 4.68274482569446430e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.325, 1.0), 3.99976879967147876e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1.0, 1.0), 2.5e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1.5, 1.0), 1.87167041810998816e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (2.0, 1.0), 1.47583617650433274e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (10.0, 1.0), 3.17255174305535695e-2, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (20.0, 1.0), 1.59022512561763752e-2, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (100.0, 1.0), 3.18299276490825515e-3, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1000.0, 1.0), 3.18309780080558939e-4, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (10000.0, 1.0), 3.18309885122757724e-5, TEST_TOL6);

  TEST (gsl_cdf_tdist_P, (-1e-100, 1.0), 0.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.001, 1.0), 4.99681690219919441e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.01, 1.0), 4.96817007235091744e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.1, 1.0), 4.68274482569446430e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.325, 1.0), 3.99976879967147876e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1.0, 1.0), 0.25, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1.5, 1.0), 1.87167041810998816e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-2.0, 1.0), 1.47583617650433274e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-10.0, 1.0), 3.17255174305535695e-2, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-20.0, 1.0), 1.59022512561763751e-2, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-100.0, 1.0), 3.18299276490825514e-3, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1000.0, 1.0), 3.18309780080558938e-4, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-10000.0, 1.0), 3.18309885122757724e-5, TEST_TOL6);

  TEST (gsl_cdf_tdist_Q, (-1e-100, 1.0), 0.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.001, 1.0), 5.00318309780080559e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.01, 1.0), 5.03182992764908255e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.1, 1.0), 5.31725517430553570e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.325, 1.0), 6.00023120032852124e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1.0, 1.0), 7.5e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1.5, 1.0), 8.12832958189001184e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-2.0, 1.0), 8.52416382349566726e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-10.0, 1.0), 9.68274482569446430e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-20.0, 1.0), 9.84097748743823625e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-100.0, 1.0), 9.96817007235091745e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1000.0, 1.0), 9.99681690219919441e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-10000.0, 1.0), 9.99968169011487724e-1, TEST_TOL6);

  TEST (gsl_cdf_tdist_P, (0.0, 2.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1e-100, 2.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.001, 2.0), 5.00353553302204959e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.01, 2.0), 5.03535445520899514e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.1, 2.0), 5.35267280792929913e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.325, 2.0), 6.11985772746873767e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1.0, 2.0), 7.88675134594812882e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1.5, 2.0), 8.63803437554499460e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (2.0, 2.0), 9.08248290463863016e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (10.0, 2.0), 9.95073771488337154e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (20.0, 2.0), 9.98754668053816452e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (100.0, 2.0), 9.99950007498750219e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1000.0, 2.0), 9.99999500000749945e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (10000.0, 2.0), 9.999999950000000739e-01, TEST_TOL6);

  TEST (gsl_cdf_tdist_Q, (0.0, 2.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1e-100, 2.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.001, 2.0), 4.99646446697795041e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.01, 2.0), 4.96464554479100486e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.1, 2.0), 4.64732719207070087e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.325, 2.0), 3.88014227253126233e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1.0, 2.0), 2.11324865405187118e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1.5, 2.0), 1.36196562445500540e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (2.0, 2.0), 9.17517095361369836e-2, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (10.0, 2.0), 4.92622851166284542e-3, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (20.0, 2.0), 1.24533194618354849e-3, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (100.0, 2.0), 4.99925012497812894e-5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1000.0, 2.0), 4.99999250001249998e-7, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (10000.0, 2.0), 4.99999992500000125e-9, TEST_TOL6);

  TEST (gsl_cdf_tdist_P, (-1e-100, 2.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.001, 2.0), 4.99646446697795041e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.01, 2.0), 4.96464554479100486e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.1, 2.0), 4.64732719207070087e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.325, 2.0), 3.88014227253126233e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1.0, 2.0), 2.11324865405187118e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1.5, 2.0), 1.36196562445500540e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-2.0, 2.0), 9.17517095361369836e-02, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-10.0, 2.0), 4.92622851166284542e-03, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-20.0, 2.0), 1.24533194618354849e-03, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-100.0, 2.0), 4.99925012497812894e-05, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1000.0, 2.0), 4.99999250001249998e-07, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-10000.0, 2.0), 4.99999992500000125e-09, TEST_TOL6);

  TEST (gsl_cdf_tdist_Q, (-1e-100, 2.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.001, 2.0), 5.00353553302204959e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.01, 2.0), 5.03535445520899514e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.1, 2.0), 5.35267280792929913e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.325, 2.0), 6.11985772746873767e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1.0, 2.0), 7.88675134594812882e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1.5, 2.0), 8.63803437554499460e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-2.0, 2.0), 9.08248290463863016e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-10.0, 2.0), 9.95073771488337155e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-20.0, 2.0), 9.98754668053816452e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-100.0, 2.0), 9.99950007498750219e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1000.0, 2.0), 9.99999500000749999e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-10000.0, 2.0), 9.99999995000000075e-1, TEST_TOL6);

  TEST (gsl_cdf_tdist_P, (0.0, 300.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1e-100, 300.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.001, 300.0), 5.00398609900942949e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.01, 300.0), 5.03986033020559088e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.1, 300.0), 5.39794441177768194e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (0.325, 300.0), 6.27296201542523812e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1.0, 300.0), 8.40941797784686861e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1.5, 300.0), 9.32666983425369137e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (2.0, 300.0), 9.76799239508425455e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (10.0, 300.0), 1.00000000000000000e+00, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (20.0, 300.0), 1.00000000000000000e+00, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (100.0, 300.0), 1.00000000000000000e+00, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (1000.0, 300.0), 1.00000000000000000e+00, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (10000.0, 300.0), 1.00000000000000000e+00, TEST_TOL6);

  TEST (gsl_cdf_tdist_Q, (0.0, 300.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1e-100, 300.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.001, 300.0), 4.99601390099057051e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.01, 300.0), 4.96013966979440912e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.1, 300.0), 4.60205558822231806e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (0.325, 300.0), 3.72703798457476188e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1.0, 300.0), 1.59058202215313138e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1.5, 300.0), 6.73330165746308628e-2, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (2.0, 300.0), 2.32007604915745452e-2, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (10.0, 300.0), 8.279313677e-21, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (20.0, 300.0), 1.93159812815803978e-57, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (100.0, 300.0), 1.02557519997736154e-232, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (1000.0, 300.0), 0.00000000000000000e+00, 0.0);
  TEST (gsl_cdf_tdist_Q, (10000.0, 300.0), 0.00000000000000000e+00, 0.0);

  TEST (gsl_cdf_tdist_P, (-1e-100, 300.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.001, 300.0), 4.99601390099057051e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.01, 300.0), 4.96013966979440912e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.1, 300.0), 4.60205558822231806e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-0.325, 300.0), 3.72703798457476188e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1.0, 300.0), 1.59058202215313138e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1.5, 300.0), 6.73330165746308628e-02, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-2.0, 300.0), 2.32007604915745452e-02, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-10.0, 300.0), 8.279313675556272534e-21, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-20.0, 300.0), 1.93159812815803978e-57, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-100.0, 300.0), 1.02557519997736154e-232, TEST_TOL6);
  TEST (gsl_cdf_tdist_P, (-1000.0, 300.0), 0.0, 0.0);
  TEST (gsl_cdf_tdist_P, (-10000.0, 300.0), 0.0, 0.0);

  TEST (gsl_cdf_tdist_Q, (-1e-100, 300.0), 5.00000000000000000e-01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.001, 300.0), 5.00398609900942949e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.01, 300.0), 5.03986033020559088e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.1, 300.0), 5.39794441177768194e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-0.325, 300.0), 6.27296201542523812e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1.0, 300.0), 8.40941797784686862e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1.5, 300.0), 9.32666983425369137e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-2.0, 300.0), 9.76799239508425455e-1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-10.0, 300.0), 1.000000000000000000e0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-20.0, 300.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-100.0, 300.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-1000.0, 300.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Q, (-10000.0, 300.0), 1.0, TEST_TOL6);
}

  /* Tests for F distribution */

  /* p(x, nu1, nu2) := betaI(1 / (1 + (nu2 / nu1) / x), nu1 / 2, nu2 / 2) */

void test_fdist (void) {
  TEST (gsl_cdf_fdist_P, (0.0, 1.2, 1.3), 0.0, 0.0);
  TEST (gsl_cdf_fdist_P, (1e-100, 1.2, 1.3), 6.98194275525039002e-61, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.001, 1.2, 1.3), 1.10608485860238564e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.01, 1.2, 1.3), 4.38636757068313850e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.1, 1.2, 1.3), 1.68242392712840734e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.325, 1.2, 1.3), 3.14130045246195449e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.0, 1.2, 1.3), 5.09630779074755253e-01, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.5, 1.2, 1.3), 5.83998640641553852e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (2.0, 1.2, 1.3), 6.34733581351938787e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10.0, 1.2, 1.3), 8.48446237879200975e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (20.0, 1.2, 1.3), 9.00987726336875039e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (100.0, 1.2, 1.3), 9.64489127047688435e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1000.0, 1.2, 1.3), 9.92012051694116388e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10000.0, 1.2, 1.3), 9.98210862808842585e-1, TEST_TOL6);

  TEST (gsl_cdf_fdist_Q, (0.0, 1.2, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1e-100, 1.2, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.001, 1.2, 1.3), 9.88939151413976144e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.01, 1.2, 1.3), 9.56136324293168615e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.1, 1.2, 1.3), 8.31757607287159265e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.325, 1.2, 1.3), 6.85869954753804551e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.0, 1.2, 1.3), 4.90369220925244747e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.5, 1.2, 1.3), 4.16001359358446148e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (2.0, 1.2, 1.3), 3.65266418648061213e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (10.0, 1.2, 1.3), 1.51553762120799025e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (20.0, 1.2, 1.3), 9.90122736631249612e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (100.0, 1.2, 1.3), 3.55108729523115643e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1000.0, 1.2, 1.3), 7.98794830588361109e-3, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (10000.0, 1.2, 1.3), 1.7891371911574145e-3, TEST_TOL6);


  /* computed with gp-pari */
     
  TEST (gsl_cdf_fdist_P, (3.479082213465832574, 1, 4040712), 0.93785072763723411967, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (3.002774644786533109, 1, 4040712), 0.91687787379476055771, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (3.000854441173130827, 1, 4040712), 0.91677930719813578619, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (3.000064021622133037, 1, 4040712), 0.9167386972447996480399, TEST_TOL6);

  TEST (gsl_cdf_fdist_P, (0.0, 500.0, 1.3), 0.0, 0.0);
  TEST (gsl_cdf_fdist_P, (1e-100, 500.0, 1.3), 0.0, 0.0);

  TEST (gsl_cdf_fdist_P, (0.001, 500.0, 1.3), 9.83434460393304765e-141, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.01, 500.0, 1.3), 1.45915624888550014e-26, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.1, 500.0, 1.3), 5.89976509619688165e-4, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.325, 500.0, 1.3), 6.86110486051542533e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.0, 500.0, 1.3), 3.38475053806404615e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.5, 500.0, 1.3), 4.52016245247457422e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (2.0, 500.0, 1.3), 5.27339068937388798e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10.0, 500.0, 1.3), 8.16839628578413905e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (20.0, 500.0, 1.3), 8.81784623056911406e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (100.0, 500.0, 1.3), 9.58045057204221295e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1000.0, 500.0, 1.3), 9.90585749380655275e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10000.0, 500.0, 1.3), 9.97891924831461387e-1, TEST_TOL6);

  TEST (gsl_cdf_fdist_Q, (0.0, 500.0, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1e-100, 500.0, 1.3), 1.0, TEST_TOL6);

  TEST (gsl_cdf_fdist_Q, (0.001, 500.0, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.01, 500.0, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.1, 500.0, 1.3), 9.99410023490380312e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.325, 500.0, 1.3), 9.31388951394845747e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.0, 500.0, 1.3), 6.61524946193595385e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.5, 500.0, 1.3), 5.47983754752542572e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (2.0, 500.0, 1.3), 4.72660931062611202e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (10.0, 500.0, 1.3), 1.83160371421586096e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (20.0, 500.0, 1.3), 1.18215376943088595e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (100.0, 500.0, 1.3), 4.19549427957787016e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1000.0, 500.0, 1.3), 9.41425061934473424e-3, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (10000.0, 500.0, 1.3), 2.10807516853862603e-3, TEST_TOL6);

  TEST (gsl_cdf_fdist_P, (0.0, 1.2, 500.0), 0.0, 0.0);
  TEST (gsl_cdf_fdist_P, (1e-100, 1.2, 500.0), 8.23342055585482999e-61, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.001, 1.2, 500.0), 1.30461496441289529e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.01, 1.2, 500.0), 5.18324224608033294e-2, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.1, 1.2, 500.0), 2.02235101716076289e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.325, 1.2, 500.0), 3.90502983219393749e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.0, 1.2, 500.0), 6.67656191574653619e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.5, 1.2, 500.0), 7.75539230271467054e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (2.0, 1.2, 500.0), 8.45209114904613705e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10.0, 1.2, 500.0), 9.99168017659120988e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (20.0, 1.2, 500.0), 9.99998005738371669e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (100.0, 1.2, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1000.0, 1.2, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10000.0, 1.2, 500.0), 1.0, TEST_TOL6);

  TEST (gsl_cdf_fdist_Q, (0.0, 1.2, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1e-100, 1.2, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.001, 1.2, 500.0), 9.86953850355871047e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.01, 1.2, 500.0), 9.48167577539196671e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.1, 1.2, 500.0), 7.97764898283923711e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.325, 1.2, 500.0), 6.09497016780606251e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.0, 1.2, 500.0), 3.32343808425346381e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.5, 1.2, 500.0), 2.24460769728532946e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (2.0, 1.2, 500.0), 1.54790885095386295e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (10.0, 1.2, 500.0), 8.3198234087901168e-4, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (20.0, 1.2, 500.0), 1.99426162833131e-6, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (100.0, 1.2, 500.0), 6.23302662288217117e-25, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1000.0, 1.2, 500.0), 1.14328577259666930e-134, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (10000.0, 1.2, 500.0), 0.0, 0.0);


  TEST (gsl_cdf_fdist_P, (0.0, 200.0, 500.0), 0.0, 0.0);
  TEST (gsl_cdf_fdist_P, (1e-100, 200.0, 500.0), 0.0, 0.0);
  TEST (gsl_cdf_fdist_P, (0.001, 200.0, 500.0), 4.09325080403669893e-251, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.01, 200.0, 500.0), 1.17894325419628688e-151, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.1, 200.0, 500.0), 5.92430940796861258e-57, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (0.325, 200.0, 500.0), 3.18220452357263554e-18, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.0, 200.0, 500.0), 5.06746326121168266e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1.5, 200.0, 500.0), 9.99794175718712438e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (2.0, 200.0, 500.0), 9.99999999528236152e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10.0, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (20.0, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (100.0, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (1000.0, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_P, (10000.0, 200.0, 500.0), 1.0, TEST_TOL6);

  TEST (gsl_cdf_fdist_Q, (0.0, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1e-100, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.001, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.01, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.1, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (0.325, 200.0, 500.0), 9.99999999999999997e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.0, 200.0, 500.0), 4.93253673878831734e-1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1.5, 200.0, 500.0), 2.05824281287561795e-4, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (2.0, 200.0, 500.0), 4.71763848371410786e-10, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (10.0, 200.0, 500.0), 5.98048337181948436e-96, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (20.0, 200.0, 500.0), 2.92099265879979502e-155, TEST_TOL6);
  TEST (gsl_cdf_fdist_Q, (1000.0, 200.0, 500.0), 0.0, 0.0);
  TEST (gsl_cdf_fdist_Q, (10000.0, 200.0, 500.0), 0.0, 0.0);
}

void test_finv (void) {
  TEST (gsl_cdf_fdist_Pinv, (0.0, 1.2, 1.3), 0.0, 0.0);
  TEST (gsl_cdf_fdist_Pinv, ( 6.98194275525039002e-61, 1.2, 1.3), 1e-100, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.10608485860238564e-2, 1.2, 1.3), 0.001, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 4.38636757068313850e-2, 1.2, 1.3), 0.01, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.68242392712840734e-1, 1.2, 1.3), 0.1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 3.14130045246195449e-1, 1.2, 1.3), 0.325, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 5.09630779074755253e-01, 1.2, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 5.83998640641553852e-1, 1.2, 1.3), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 6.34733581351938787e-1, 1.2, 1.3), 2.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 8.48446237879200975e-1, 1.2, 1.3), 10.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.00987726336875039e-1, 1.2, 1.3), 20.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.64489127047688435e-1, 1.2, 1.3), 100.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.92012051694116388e-1, 1.2, 1.3), 1000.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.98210862808842585e-1, 1.2, 1.3), 10000.0, TEST_TOL6);

  TEST (gsl_cdf_fdist_Qinv, ( 1.0, 1.2, 1.3), 0.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 9.88939151413976144e-1, 1.2, 1.3), 0.001, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 9.56136324293168615e-1, 1.2, 1.3), 0.01, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 8.31757607287159265e-1, 1.2, 1.3), 0.1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 6.85869954753804551e-1, 1.2, 1.3), 0.325, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 4.90369220925244747e-1, 1.2, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 4.16001359358446148e-1, 1.2, 1.3), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 3.65266418648061213e-1, 1.2, 1.3), 2.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 1.51553762120799025e-1, 1.2, 1.3), 10.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 9.90122736631249612e-2, 1.2, 1.3), 20.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 3.55108729523115643e-2, 1.2, 1.3), 100.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 7.98794830588361109e-3, 1.2, 1.3), 1000.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 1.7891371911574145e-3, 1.2, 1.3), 10000.0, TEST_TOL6);


  TEST (gsl_cdf_fdist_Pinv, ( 0.0, 500.0, 1.3), 0.0, 0.0);

  TEST (gsl_cdf_fdist_Pinv, ( 9.83434460393304765e-141, 500.0, 1.3), 0.001, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.45915624888550014e-26, 500.0, 1.3), 0.01, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 5.89976509619688165e-4, 500.0, 1.3), 0.1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 6.86110486051542533e-2, 500.0, 1.3), 0.325, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 3.38475053806404615e-1, 500.0, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 4.52016245247457422e-1, 500.0, 1.3), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 5.27339068937388798e-1, 500.0, 1.3), 2.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 8.16839628578413905e-1, 500.0, 1.3), 10.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 8.81784623056911406e-1, 500.0, 1.3), 20.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.58045057204221295e-1, 500.0, 1.3), 100.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.90585749380655275e-1, 500.0, 1.3), 1000.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.97891924831461387e-1, 500.0, 1.3), 10000.0, TEST_TOL6);

  TEST (gsl_cdf_fdist_Qinv, ( 1.0, 500.0, 1.3), 0.0, TEST_TOL6);

  /*
   * The algorithm currently implemented in gsl_cdf_fdist_Qinv and Pinv
   * are not accurate for very large degrees of freedom, so the tests
   * here are commented out. Another algorithm more suitable for
   * these extreme values might pass these tests.
   */

  TEST (gsl_cdf_fdist_Qinv, ( 9.99410023490380312e-1, 500.0, 1.3), 0.1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 9.31388951394845747e-1, 500.0, 1.3), 0.325, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 6.61524946193595385e-1, 500.0, 1.3), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 5.47983754752542572e-1, 500.0, 1.3), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 4.72660931062611202e-1, 500.0, 1.3), 2.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 1.83160371421586096e-1, 500.0, 1.3), 10.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 1.18215376943088595e-1, 500.0, 1.3), 20.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 4.19549427957787016e-2, 500.0, 1.3), 100.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 9.41425061934473424e-3, 500.0, 1.3), 1000.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 2.10807516853862603e-3, 500.0, 1.3), 10000.0, TEST_TOL6);

  TEST (gsl_cdf_fdist_Pinv, ( 0.0, 1.2, 500.0), 0.0, 0.0);
  TEST (gsl_cdf_fdist_Pinv, ( 8.23342055585482999e-61, 1.2, 500.0), 1e-100, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.30461496441289529e-2, 1.2, 500.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 5.18324224608033294e-2, 1.2, 500.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 2.02235101716076289e-1, 1.2, 500.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 3.90502983219393749e-1, 1.2, 500.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 6.67656191574653619e-1, 1.2, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 7.75539230271467054e-1, 1.2, 500.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 8.45209114904613705e-1, 1.2, 500.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.99168017659120988e-1, 1.2, 500.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.99998005738371669e-1, 1.2, 500.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.0, 1.2, 500.0), GSL_POSINF, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.0, 1.2, 500.0), GSL_POSINF, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.0, 1.2, 500.0), GSL_POSINF, TEST_TOL6);

  TEST (gsl_cdf_fdist_Qinv, ( 1.0, 1.2, 500.0), 0.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 9.86953850355871047e-1, 1.2, 500.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 9.48167577539196671e-1, 1.2, 500.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 7.97764898283923711e-1, 1.2, 500.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 6.09497016780606251e-1, 1.2, 500.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 3.32343808425346381e-1, 1.2, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 2.24460769728532946e-1, 1.2, 500.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 1.54790885095386295e-1, 1.2, 500.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 8.3198234087901168e-4, 1.2, 500.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 1.99426162833131e-6, 1.2, 500.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 6.23302662288217117e-25, 1.2, 500.0), 100.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 1.14328577259666930e-134, 1.2, 500.0), 1000.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 0.0, 1.2, 500.0), GSL_POSINF, 0.0);

  TEST (gsl_cdf_fdist_Pinv, ( 0.0, 200.0, 500.0), 0.0, 0.0);
  TEST (gsl_cdf_fdist_Pinv, ( 4.09325080403669893e-251, 200.0, 500.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.17894325419628688e-151, 200.0, 500.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 5.92430940796861258e-57, 200.0, 500.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 3.18220452357263554e-18, 200.0, 500.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 5.06746326121168266e-1, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 9.99794175718712438e-1, 200.0, 500.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Pinv, ( 1.0, 200.0, 500.0), GSL_POSINF, TEST_TOL6);

  TEST (gsl_cdf_fdist_Qinv, ( 1.0, 200.0, 500.0), 0.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 4.93253673878831734e-1, 200.0, 500.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 2.05824281287561795e-4, 200.0, 500.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 4.71763848371410786e-10, 200.0, 500.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 5.98048337181948436e-96, 200.0, 500.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 2.92099265879979502e-155, 200.0, 500.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_fdist_Qinv, ( 0.0, 200.0, 500.0), GSL_POSINF, 0.0);

  TEST (gsl_cdf_fdist_Pinv, (0.95,1.0,261.0), 3.8773340322508720313e+00, TEST_TOL3);
}

  /* Tests for gamma distribution */

  /* p(x, a, b) := gammaP(b, x / a) */

void test_gamma (void)
{
  TEST (gsl_cdf_gamma_P, (0.0, 1.0, 1.0), 0.0, 0.0);
  TEST (gsl_cdf_gamma_P, (1e-100, 1.0, 1.0), 1e-100, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.001, 1.0, 1.0), 9.99500166625008332e-4, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.01, 1.0, 1.0), 9.95016625083194643e-3, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.1, 1.0, 1.0), 9.51625819640404268e-2, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.325, 1.0, 1.0), 2.77472646357927811e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1.0, 1.0, 1.0), 6.32120558828557678e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1.5, 1.0, 1.0), 7.76869839851570171e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (2.0, 1.0, 1.0), 8.64664716763387308e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (10.0, 1.0, 1.0), 9.99954600070237515e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (20.0, 1.0, 1.0), 9.99999997938846378e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (100.0, 1.0, 1.0), 1e0, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1000.0, 1.0, 1.0), 1e0, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (10000.0, 1.0, 1.0), 1e0, TEST_TOL6);

  TEST (gsl_cdf_gamma_Q, (0.0, 1.0, 1.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1e-100, 1.0, 1.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.001, 1.0, 1.0), 9.99000499833374992e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.01, 1.0, 1.0), 9.90049833749168054e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.1, 1.0, 1.0), 9.04837418035959573e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.325, 1.0, 1.0), 7.22527353642072189e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1.0, 1.0, 1.0), 3.67879441171442322e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1.5, 1.0, 1.0), 2.23130160148429829e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (2.0, 1.0, 1.0), 1.35335283236612692e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (10.0, 1.0, 1.0), 4.53999297624848515e-5, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (20.0, 1.0, 1.0), 2.06115362243855783e-9, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (100.0, 1.0, 1.0), 3.72007597602083596e-44, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1000.0, 1.0, 1.0), 0.0, 0.0);
  TEST (gsl_cdf_gamma_Q, (10000.0, 1.0, 1.0), 0.0, 0.0);

  TEST (gsl_cdf_gamma_P, (0.0, 1.0, 10.0), 0.0, 0.0);
  TEST (gsl_cdf_gamma_P, (1e-100, 1.0, 10.0), 1e-101, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.001, 1.0, 10.0), 9.99950001666625001e-5, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.01, 1.0, 10.0), 9.99500166625008332e-4, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.1, 1.0, 10.0), 9.95016625083194643e-3, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.325, 1.0, 10.0), 3.19775501686939529e-2, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1.0, 1.0, 10.0), 9.51625819640404268e-2, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1.5, 1.0, 10.0), 1.39292023574942193e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (2.0, 1.0, 10.0), 1.81269246922018141e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (10.0, 1.0, 10.0), 6.32120558828557678e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (20.0, 1.0, 10.0), 8.64664716763387308e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (100.0, 1.0, 10.0), 9.99954600070237515e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1000.0, 1.0, 10.0), 1e0, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (10000.0, 1.0, 10.0), 1e0, TEST_TOL6);

  TEST (gsl_cdf_gamma_Q, (0.0, 1.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1e-100, 1.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.001, 1.0, 10.0), 9.99900004999833337e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.01, 1.0, 10.0), 9.99000499833374992e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.1, 1.0, 10.0), 9.90049833749168054e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.325, 1.0, 10.0), 9.68022449831306047e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1.0, 1.0, 10.0), 9.04837418035959573e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1.5, 1.0, 10.0), 8.60707976425057807e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (2.0, 1.0, 10.0), 8.18730753077981859e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (10.0, 1.0, 10.0), 3.67879441171442322e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (20.0, 1.0, 10.0), 1.35335283236612692e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (100.0, 1.0, 10.0), 4.53999297624848515e-5, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1000.0, 1.0, 10.0), 3.72007597602083596e-44, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (10000.0, 1.0, 10.0), 0.0, 0.0);

  TEST (gsl_cdf_gamma_P, (0.0, 17.0, 10.0), 0e0, 0.0);
  TEST (gsl_cdf_gamma_P, (1e-100, 17.0, 10.0), 0e0, 0.0);
  TEST (gsl_cdf_gamma_P, (0.001, 17.0, 10.0), 2.81119174040422844e-83, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.01, 17.0, 10.0), 2.80880324651985887e-66, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.1, 17.0, 10.0), 2.78502998087492130e-49, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (0.325, 17.0, 10.0), 1.37283653245125844e-40, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1.0, 17.0, 10.0), 2.55811932292544243e-32, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1.5, 17.0, 10.0), 2.40420441175422372e-29, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (2.0, 17.0, 10.0), 3.05092926217898577e-27, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (10.0, 17.0, 10.0), 1.094920130378183e-15, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (20.0, 17.0, 10.0), 5.60605096173161688e-11, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (100.0, 17.0, 10.0), 2.70416097848011280e-2, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (1000.0, 17.0, 10.0), 1.000000000000000000e0, TEST_TOL6);
  TEST (gsl_cdf_gamma_P, (10000.0, 17.0, 10.0), 1.000000000000000000e0, TEST_TOL6);

  TEST (gsl_cdf_gamma_Q, (0.0, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1e-100, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.001, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.01, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.1, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (0.325, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1.0, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1.5, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (2.0, 17.0, 10.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (10.0, 17.0, 10.0), 9.99999999999998905e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (20.0, 17.0, 10.0), 9.99999999943939490e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (100.0, 17.0, 10.0), 9.72958390215198872e-1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (1000.0, 17.0, 10.0), 2.11200951633948570e-25, TEST_TOL6);
  TEST (gsl_cdf_gamma_Q, (10000.0, 17.0, 10.0), 0.0, 0.0);
}

void test_chisq (void) {
  TEST (gsl_cdf_chisq_P, (0.0, 13.0), 0.0, 0.0);
  TEST (gsl_cdf_chisq_P, (1e-100, 13.0), 0.0, 0.0);
  TEST (gsl_cdf_chisq_P, (0.001, 13.0), 1.86631102655845996e-25, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (0.01, 13.0), 5.87882248504529790e-19, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (0.1, 13.0), 1.78796983358555410e-12, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (0.325, 13.0), 3.44611313779905183e-9, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (1.0, 13.0), 3.83473473513595154e-6, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (1.5, 13.0), 4.31718389201041932e-5, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (2.0, 13.0), 2.26250084656047180e-4, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (10.0, 13.0), 3.06065632019251110e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (20.0, 13.0), 9.04789743921908487e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (100.0, 13.0), 9.99999999999998341e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (1000.0, 13.0), 1e0, TEST_TOL6);
  TEST (gsl_cdf_chisq_P, (10000.0, 13.0), 1e0, TEST_TOL6);

  TEST (gsl_cdf_chisq_Q, (0.0, 13.0), 1e0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (1e-100, 13.0), 1e0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (0.001, 13.0), 1e0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (0.01, 13.0), 9.99999999999999999e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (0.1, 13.0), 9.99999999998212030e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (0.325, 13.0), 9.99999996553886862e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (1.0, 13.0), 9.99996165265264864e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (1.5, 13.0), 9.99956828161079896e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (2.0, 13.0), 9.99773749915343953e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (10.0, 13.0), 6.93934367980748890e-1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (20.0, 13.0), 9.52102560780915127e-2, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (100.0, 13.0), 1.65902608070858809e-15, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (1000.0, 13.0), 1.74851191544860225e-205, TEST_TOL6);
  TEST (gsl_cdf_chisq_Q, (10000.0, 13.0), 0.0, 0.0);
}


  /* Beta distribution */

void test_beta (void) {
  TEST (gsl_cdf_beta_P, (0.0, 1.2, 1.3), 0.0, 0.0);
  TEST (gsl_cdf_beta_P, (1e-100, 1.2, 1.3), 1.34434944656489596e-120, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.001, 1.2, 1.3), 3.37630042504535813e-4, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.01, 1.2, 1.3), 5.34317264038929473e-3, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.1, 1.2, 1.3), 8.33997828306748346e-2, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.325, 1.2, 1.3), 3.28698654180583916e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.5, 1.2, 1.3), 5.29781429451299081e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.9, 1.2, 1.3), 9.38529397224430659e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.99, 1.2, 1.3), 9.96886438341254380e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (0.999, 1.2, 1.3), 9.99843792833067634e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_P, (1.0, 1.2, 1.3), 1.0, TEST_TOL6);

  TEST (gsl_cdf_beta_Q, (0.0, 1.2, 1.3), 1.0, 0.0);
  TEST (gsl_cdf_beta_Q, (1e-100, 1.2, 1.3), 1e0, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.001, 1.2, 1.3), 9.99662369957495464e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.01, 1.2, 1.3), 9.94656827359610705e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.1, 1.2, 1.3), 9.16600217169325165e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.325, 1.2, 1.3), 6.71301345819416084e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.5, 1.2, 1.3), 4.70218570548700919e-1, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.9, 1.2, 1.3), 6.14706027755693408e-2, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.99, 1.2, 1.3), 3.11356165874561958e-3, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (0.999, 1.2, 1.3), 1.56207166932365759e-4, TEST_TOL6);
  TEST (gsl_cdf_beta_Q, (1.0, 1.2, 1.3), 0.0, TEST_TOL6);
}

void test_betainv (void) {
  TEST (gsl_cdf_beta_Pinv, (0.0, 1.2, 1.3), 0.0, 0.0);
  TEST (gsl_cdf_beta_Pinv, ( 1.34434944656489596e-120, 1.2, 1.3), 1e-100, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 3.37630042504535813e-4, 1.2, 1.3), 0.001, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 5.34317264038929473e-3, 1.2, 1.3), 0.01, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 8.33997828306748346e-2, 1.2, 1.3), 0.1, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 3.28698654180583916e-1, 1.2, 1.3), 0.325, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 5.29781429451299081e-1, 1.2, 1.3), 0.5, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 9.38529397224430659e-1, 1.2, 1.3), 0.9, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 9.96886438341254380e-1, 1.2, 1.3), 0.99, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 9.99843792833067634e-1, 1.2, 1.3), 0.999, TEST_TOL6);
  TEST (gsl_cdf_beta_Pinv, ( 1.0, 1.2, 1.3), 1.0, TEST_TOL6);

  TEST (gsl_cdf_beta_Qinv, ( 1.0, 1.2, 1.3), 0.0, 0.0);
  TEST (gsl_cdf_beta_Qinv, ( 1e0, 1.2, 1.3), 0.0, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 9.99662369957495464e-1, 1.2, 1.3), 0.001, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 9.94656827359610705e-1, 1.2, 1.3), 0.01, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 9.16600217169325165e-1, 1.2, 1.3), 0.1, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 6.71301345819416084e-1, 1.2, 1.3), 0.325, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 4.70218570548700919e-1, 1.2, 1.3), 0.5, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 6.14706027755693408e-2, 1.2, 1.3), 0.9, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 3.11356165874561958e-3, 1.2, 1.3), 0.99, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 1.56207166932365759e-4, 1.2, 1.3), 0.999, TEST_TOL6);
  TEST (gsl_cdf_beta_Qinv, ( 0.0, 1.2, 1.3), 1.0, TEST_TOL6);

  TEST (gsl_cdf_beta_Pinv, ( 0.025, 2133.0, 7868.0),  0.20530562929915865457928654, TEST_TOL6);
}

void test_gammainv (void) {
  TEST (gsl_cdf_gamma_Pinv, (0.0, 1.0, 1.0), 0.0, 0.0);
  TEST (gsl_cdf_gamma_Pinv, (1e-100, 1.0, 1.0), 1e-100, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (9.99500166625008332e-4, 1.0, 1.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (9.95016625083194643e-3, 1.0, 1.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (9.51625819640404268e-2, 1.0, 1.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (2.77472646357927811e-1, 1.0, 1.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (6.32120558828557678e-1, 1.0, 1.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (7.76869839851570171e-1, 1.0, 1.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (8.64664716763387308e-1, 1.0, 1.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (9.99954600070237515e-1, 1.0, 1.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (9.99999997938846378e-1, 1.0, 1.0), 20.0, 100 * TEST_TOL6);
  TEST (gsl_cdf_gamma_Pinv, (1.0, 1.0, 1.0), GSL_POSINF, 0.0);

  /* Test case from Benjamin Redelings <benjamin_redelings@ncsu.edu> */
  /* fails on x86_64,  FIXME test value is from octave -- get high precision value */
  TEST (gsl_cdf_gamma_Pinv, (0.1, 11.887411491530846,1.0), 7.73788447848618e+00, TEST_TOL1);

  TEST (gsl_cdf_gamma_Qinv, (0.0, 1.0, 1.0), GSL_POSINF, 0.0);
  TEST (gsl_cdf_gamma_Qinv, (2.06115362243855783e-9, 1.0, 1.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (4.53999297624848515e-5, 1.0, 1.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (1.35335283236612692e-1, 1.0, 1.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (2.23130160148429829e-1, 1.0, 1.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (3.67879441171442322e-1, 1.0, 1.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (7.22527353642072189e-1, 1.0, 1.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (9.04837418035959573e-1, 1.0, 1.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (9.90049833749168054e-1, 1.0, 1.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (9.99000499833374992e-1, 1.0, 1.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_gamma_Qinv, (1.0, 1.0, 1.0), 0.0, 0.0);
}

void test_chisqinv (void) {
  TEST (gsl_cdf_chisq_Pinv, (0.0, 13.0), 0.0, 0.0);
  TEST (gsl_cdf_chisq_Pinv, (1.86631102655845996e-25, 13.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (5.87882248504529790e-19, 13.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (1.78796983358555410e-12, 13.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (3.44611313779905183e-9, 13.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (3.83473473513595154e-6, 13.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (4.31718389201041932e-5, 13.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (2.26250084656047180e-4, 13.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (3.06065632019251110e-1, 13.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (9.04789743921908487e-1, 13.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (9.99999999999998341e-1, 13.0), 100.0, 0.01);
  TEST (gsl_cdf_chisq_Pinv, (1e0, 13.0), GSL_POSINF, 0.0);

  TEST (gsl_cdf_chisq_Pinv, (1.93238145206123590e-01, 1.5), 0.211980092931799521729407, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (4.83e-8, 19.19), 1.632280186860266704532868343, TEST_TOL6);

  /* Test cases for bug 24704 */
  TEST (gsl_cdf_chisq_Pinv, (0.05, 1263131.0), 1260517.771133388726131469059, TEST_TOL6);
  TEST (gsl_cdf_chisq_Pinv, (0.05, 2526262.0), 2522565.864973351096735720202, TEST_TOL6);

#if 0 /* XXX - bug #39057 */
  /* Test case reported by Yan Zhou <zhouyan@me.com> */
  TEST (gsl_cdf_chisq_Pinv, (0.5, 0.01), 0.99477710813146, TEST_TOL6);
#endif

  TEST (gsl_cdf_chisq_Qinv, (0.0, 13.0), GSL_POSINF, TEST_TOL6);
  TEST (gsl_cdf_chisq_Qinv, (1.65902608070858809e-15, 13.0), 100.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Qinv, (9.52102560780915127e-2, 13.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Qinv, (6.93934367980748892e-1, 13.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Qinv, (9.99773749915343954e-1, 13.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Qinv, (9.99956828161079894e-1, 13.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_chisq_Qinv, (9.99996165265264863e-1, 13.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_chisq_Qinv, (9.99999996553886862e-1, 13.0), 0.325, 1e-6);
  TEST (gsl_cdf_chisq_Qinv, (9.99999999998212031e-1, 13.0), 0.1, 1e-5);
  TEST (gsl_cdf_chisq_Qinv, (1.0, 13.0), 0.0, 0.0);
}

void test_tdistinv (void) {
  TEST (gsl_cdf_tdist_Pinv, (0.5, 1.0), 0.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (5.00318309780080559e-1, 1.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (5.03182992764908255e-1, 1.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (5.31725517430553569e-1, 1.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (6.00023120032852123e-1, 1.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (0.75000000000000000e0, 1.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (8.12832958189001183e-1, 1.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (8.52416382349566726e-1, 1.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.68274482569446430e-1, 1.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.84097748743823625e-1, 1.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.96817007235091745e-1, 1.0), 100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.99681690219919441e-1, 1.0), 1000.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.99968169011487724e-1, 1.0), 10000.0, TEST_TOL6);

  TEST (gsl_cdf_tdist_Qinv, (0.5, 1.0), 0.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (4.99681690219919441e-1, 1.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (4.96817007235091745e-1, 1.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (4.68274482569446430e-1, 1.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (3.99976879967147876e-1, 1.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (2.5e-1, 1.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (1.87167041810998816e-1, 1.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (1.47583617650433274e-1, 1.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (3.17255174305535695e-2, 1.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (1.59022512561763752e-2, 1.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (3.18299276490825515e-3, 1.0), 100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (3.18309780080558939e-4, 1.0), 1000.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (3.18309885122757724e-5, 1.0), 10000.0, TEST_TOL6);

  TEST (gsl_cdf_tdist_Pinv, (4.99681690219919441e-1, 1.0), -0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.96817007235091744e-1, 1.0), -0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.68274482569446430e-1, 1.0), -0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (3.99976879967147876e-1, 1.0), -0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (0.25, 1.0), -1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.87167041810998816e-1, 1.0), -1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.47583617650433274e-1, 1.0), -2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (3.17255174305535695e-2, 1.0), -10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.59022512561763751e-2, 1.0), -20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (3.18299276490825514e-3, 1.0), -100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (3.18309780080558938e-4, 1.0), -1000.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (3.18309885122757724e-5, 1.0), -10000.0, TEST_TOL6);

  TEST (gsl_cdf_tdist_Qinv, (5.00318309780080559e-1, 1.0), -0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (5.03182992764908255e-1, 1.0), -0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (5.31725517430553570e-1, 1.0), -0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (6.00023120032852124e-1, 1.0), -0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (7.5e-1, 1.0), -1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (8.12832958189001184e-1, 1.0), -1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (8.52416382349566726e-1, 1.0), -2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.68274482569446430e-1, 1.0), -10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.84097748743823625e-1, 1.0), -20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.96817007235091745e-1, 1.0), -100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.99681690219919441e-1, 1.0), -1000.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.99968169011487724e-1, 1.0), -10000.0, TEST_TOL6);

  TEST (gsl_cdf_tdist_Pinv, (4.99646446697795041e-01, 2.0), -0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.96464554479100486e-01, 2.0), -0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.64732719207070087e-01, 2.0), -0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (3.88014227253126233e-01, 2.0), -0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (2.11324865405187118e-01, 2.0), -1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.36196562445500540e-01, 2.0), -1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.17517095361369836e-02, 2.0), -2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.92622851166284542e-03, 2.0), -10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.24533194618354849e-03, 2.0), -20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.99925012497812894e-05, 2.0), -100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.99999250001249998e-07, 2.0), -1000.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.99999992500000125e-09, 2.0), -10000.0, TEST_TOL6);

  TEST (gsl_cdf_tdist_Qinv, (5.00353553302204959e-1, 2.0), -0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (5.03535445520899514e-1, 2.0), -0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (5.35267280792929913e-1, 2.0), -0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (6.11985772746873767e-1, 2.0), -0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (7.88675134594812882e-1, 2.0), -1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (8.63803437554499460e-1, 2.0), -1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.08248290463863016e-1, 2.0), -2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.95073771488337155e-1, 2.0), -10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.98754668053816452e-1, 2.0), -20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.99950007498750219e-1, 2.0), -100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.99999500000749999e-1, 2.0), -1000.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.99999995000000075e-1, 2.0), -10000.0, 1e-6);

  TEST (gsl_cdf_tdist_Pinv, (5.00000000000000000e-01, 300.0), 0.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (5.00398609900942949e-01, 300.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (5.03986033020559088e-01, 300.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (5.39794441177768194e-01, 300.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (6.27296201542523812e-01, 300.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (8.40941797784686861e-01, 300.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.32666983425369137e-01, 300.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (9.76799239508425455e-01, 300.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.00000000000000000e+00, 300.0), GSL_POSINF, 0.0);

  TEST (gsl_cdf_tdist_Qinv, (5.00000000000000000e-01, 300.0), 0.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (4.99601390099057051e-1, 300.0), 0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (4.96013966979440912e-1, 300.0), 0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (4.60205558822231806e-1, 300.0), 0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (3.72703798457476188e-1, 300.0), 0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (1.59058202215313138e-1, 300.0), 1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (6.73330165746308628e-2, 300.0), 1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (2.32007604915745452e-2, 300.0), 2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (8.279313677e-21, 300.0), 10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (1.93159812815803978e-57, 300.0), 20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (1.02557519997736154e-232, 300.0), 100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (0.00000000000000000e+00, 300.0), GSL_POSINF, 0.0);

  TEST (gsl_cdf_tdist_Pinv, (4.99601390099057051e-01, 300.0), -0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.96013966979440912e-01, 300.0), -0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (4.60205558822231806e-01, 300.0), -0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (3.72703798457476188e-01, 300.0), -0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.59058202215313138e-01, 300.0), -1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (6.73330165746308628e-02, 300.0), -1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (2.32007604915745452e-02, 300.0), -2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (8.279313675556272534e-21, 300.0), -10.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.93159812815803978e-57, 300.0), -20.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (1.02557519997736154e-232, 300.0), -100.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Pinv, (0.0, 300.0), GSL_NEGINF, 0.0);

  TEST (gsl_cdf_tdist_Qinv, (5.00398609900942949e-1, 300.0), -0.001, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (5.03986033020559088e-1, 300.0), -0.01, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (5.39794441177768194e-1, 300.0), -0.1, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (6.27296201542523812e-1, 300.0), -0.325, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (8.40941797784686862e-1, 300.0), -1.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.32666983425369137e-1, 300.0), -1.5, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (9.76799239508425455e-1, 300.0), -2.0, TEST_TOL6);
  TEST (gsl_cdf_tdist_Qinv, (1.000000000000000000e0, 300.0), GSL_NEGINF, TEST_TOL6);
}



