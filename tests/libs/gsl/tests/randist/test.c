/* randist/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007, 2010 James Theiler, Brian Gough
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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_integration.h>

#define N 100000

/* Convient test dimension for multivariant distributions */
#define MULTI_DIM 10


void testMoments (double (*f) (void), const char *name,
                  double a, double b, double p);
void testPDF (double (*f) (void), double (*pdf) (double), const char *name);
void testDiscretePDF (double (*f) (void), double (*pdf) (unsigned int),
                      const char *name);

void test_shuffle (void);
void test_choose (void);
double test_beta (void);
double test_beta_pdf (double x);
double test_bernoulli (void);
double test_bernoulli_pdf (unsigned int n);

double test_binomial (void);
double test_binomial_pdf (unsigned int n);
double test_binomial_large (void);
double test_binomial_large_pdf (unsigned int n);
double test_binomial_huge (void);
double test_binomial_huge_pdf (unsigned int n);
double test_binomial_max (void);
double test_binomial_max_pdf (unsigned int n);
double test_binomial0 (void);
double test_binomial0_pdf (unsigned int n);
double test_binomial1 (void);
double test_binomial1_pdf (unsigned int n);



double test_binomial_knuth (void);
double test_binomial_knuth_pdf (unsigned int n);
double test_binomial_large_knuth (void);
double test_binomial_large_knuth_pdf (unsigned int n);
double test_binomial_huge_knuth (void);
double test_binomial_huge_knuth_pdf (unsigned int n);

double test_cauchy (void);
double test_cauchy_pdf (double x);
double test_chisq (void);
double test_chisq_pdf (double x);
double test_chisqnu2 (void);
double test_chisqnu2_pdf (double x);
double test_dirichlet (void);
double test_dirichlet_pdf (double x);
double test_dirichlet_small (void);
double test_dirichlet_small_pdf (double x);
void test_dirichlet_moments (void);
double test_discrete1 (void);
double test_discrete1_pdf (unsigned int n);
double test_discrete2 (void);
double test_discrete2_pdf (unsigned int n);
double test_discrete3 (void);
double test_discrete3_pdf (unsigned int n);
double test_erlang (void);
double test_erlang_pdf (double x);
double test_exponential (void);
double test_exponential_pdf (double x);
double test_exppow0 (void);
double test_exppow0_pdf (double x);
double test_exppow1 (void);
double test_exppow1_pdf (double x);
double test_exppow1a (void);
double test_exppow1a_pdf (double x);
double test_exppow2 (void);
double test_exppow2_pdf (double x);
double test_exppow2a (void);
double test_exppow2a_pdf (double x);
double test_exppow2b (void);
double test_exppow2b_pdf (double x);
double test_fdist (void);
double test_fdist_pdf (double x);
double test_fdist_large (void);
double test_fdist_large_pdf (double x);
double test_flat (void);
double test_flat_pdf (double x);
double test_gamma (void);
double test_gamma_pdf (double x);
double test_gamma1 (void);
double test_gamma1_pdf (double x);
double test_gamma_int (void);
double test_gamma_int_pdf (double x);
double test_gamma_large (void);
double test_gamma_large_pdf (double x);
double test_gamma_vlarge (void);
double test_gamma_vlarge_pdf (double x);
double test_gamma_small (void);
double test_gamma_small_pdf (double x);
double test_gamma_mt (void);
double test_gamma_mt_pdf (double x);
double test_gamma_mt1 (void);
double test_gamma_mt1_pdf (double x);
double test_gamma_mt_int (void);
double test_gamma_mt_int_pdf (double x);
double test_gamma_mt_large (void);
double test_gamma_mt_large_pdf (double x);
double test_gamma_mt_small (void);
double test_gamma_mt_small_pdf (double x);
double test_gamma_knuth_vlarge (void);
double test_gamma_knuth_vlarge_pdf (double x);
double test_gaussian (void);
double test_gaussian_pdf (double x);
double test_gaussian_ratio_method (void);
double test_gaussian_ratio_method_pdf (double x);
double test_gaussian_ziggurat (void);
double test_gaussian_ziggurat_pdf (double x);
double test_gaussian_tail (void);
double test_gaussian_tail_pdf (double x);
double test_gaussian_tail1 (void);
double test_gaussian_tail1_pdf (double x);
double test_gaussian_tail2 (void);
double test_gaussian_tail2_pdf (double x);
double test_ugaussian (void);
double test_ugaussian_pdf (double x);
double test_ugaussian_ratio_method (void);
double test_ugaussian_ratio_method_pdf (double x);
double test_ugaussian_tail (void);
double test_ugaussian_tail_pdf (double x);
double test_bivariate_gaussian1 (void);
double test_bivariate_gaussian1_pdf (double x);
double test_bivariate_gaussian2 (void);
double test_bivariate_gaussian2_pdf (double x);
double test_bivariate_gaussian3 (void);
double test_bivariate_gaussian3_pdf (double x);
double test_bivariate_gaussian4 (void);
double test_bivariate_gaussian4_pdf (double x);
double test_gumbel1 (void);
double test_gumbel1_pdf (double x);
double test_gumbel2 (void);
double test_gumbel2_pdf (double x);
double test_geometric (void);
double test_geometric_pdf (unsigned int x);
double test_geometric1 (void);
double test_geometric1_pdf (unsigned int x);
double test_hypergeometric1 (void);
double test_hypergeometric1_pdf (unsigned int x);
double test_hypergeometric2 (void);
double test_hypergeometric2_pdf (unsigned int x);
double test_hypergeometric3 (void);
double test_hypergeometric3_pdf (unsigned int x);
double test_hypergeometric4 (void);
double test_hypergeometric4_pdf (unsigned int x);
double test_hypergeometric5 (void);
double test_hypergeometric5_pdf (unsigned int x);
double test_hypergeometric6 (void);
double test_hypergeometric6_pdf (unsigned int x);
double test_landau (void);
double test_landau_pdf (double x);
double test_levy1 (void);
double test_levy1_pdf (double x);
double test_levy2 (void);
double test_levy2_pdf (double x);
double test_levy1a (void);
double test_levy1a_pdf (double x);
double test_levy2a (void);
double test_levy2a_pdf (double x);
double test_levy_skew1 (void);
double test_levy_skew1_pdf (double x);
double test_levy_skew2 (void);
double test_levy_skew2_pdf (double x);
double test_levy_skew1a (void);
double test_levy_skew1a_pdf (double x);
double test_levy_skew2a (void);
double test_levy_skew2a_pdf (double x);
double test_levy_skew1b (void);
double test_levy_skew1b_pdf (double x);
double test_levy_skew2b (void);
double test_levy_skew2b_pdf (double x);
double test_logistic (void);
double test_logistic_pdf (double x);
double test_lognormal (void);
double test_lognormal_pdf (double x);
double test_logarithmic (void);
double test_logarithmic_pdf (unsigned int n);
double test_multinomial (void);
double test_multinomial_pdf (unsigned int n);
double test_multinomial_large (void);
double test_multinomial_large_pdf (unsigned int n);
void test_multinomial_moments (void);
double test_negative_binomial (void);
double test_negative_binomial_pdf (unsigned int n);
double test_pascal (void);
double test_pascal_pdf (unsigned int n);
double test_pareto (void);
double test_pareto_pdf (double x);
double test_poisson (void);
double test_poisson_pdf (unsigned int x);
double test_poisson_large (void);
double test_poisson_large_pdf (unsigned int x);
double test_dir2d (void);
double test_dir2d_pdf (double x);
double test_dir2d_trig_method (void);
double test_dir2d_trig_method_pdf (double x);
double test_dir3dxy (void);
double test_dir3dxy_pdf (double x);
double test_dir3dyz (void);
double test_dir3dyz_pdf (double x);
double test_dir3dzx (void);
double test_dir3dzx_pdf (double x);
double test_rayleigh (void);
double test_rayleigh_pdf (double x);
double test_rayleigh_tail (void);
double test_rayleigh_tail_pdf (double x);
double test_tdist1 (void);
double test_tdist1_pdf (double x);
double test_tdist2 (void);
double test_tdist2_pdf (double x);
double test_laplace (void);
double test_laplace_pdf (double x);
double test_weibull (void);
double test_weibull_pdf (double x);
double test_weibull1 (void);
double test_weibull1_pdf (double x);

gsl_rng *r_global;

static gsl_ran_discrete_t *g1 = NULL;
static gsl_ran_discrete_t *g2 = NULL;
static gsl_ran_discrete_t *g3 = NULL;

int
main (void)
{
  gsl_ieee_env_setup ();

  gsl_rng_env_setup ();
  r_global = gsl_rng_alloc (gsl_rng_default);

#define FUNC(x)  test_ ## x,                     "test gsl_ran_" #x
#define FUNC2(x) test_ ## x, test_ ## x ## _pdf, "test gsl_ran_" #x

  test_shuffle ();
  test_choose ();

  testMoments (FUNC (ugaussian), 0.0, 100.0, 0.5);
  testMoments (FUNC (ugaussian), -1.0, 1.0, 0.6826895);
  testMoments (FUNC (ugaussian), 3.0, 3.5, 0.0011172689);
  testMoments (FUNC (ugaussian_tail), 3.0, 3.5, 0.0011172689 / 0.0013498981);
  testMoments (FUNC (exponential), 0.0, 1.0, 1 - exp (-0.5));
  testMoments (FUNC (cauchy), 0.0, 10000.0, 0.5);

  testMoments (FUNC (discrete1), -0.5, 0.5, 0.59);
  testMoments (FUNC (discrete1), 0.5, 1.5, 0.40);
  testMoments (FUNC (discrete1), 1.5, 3.5, 0.01);

  testMoments (FUNC (discrete2), -0.5,  0.5, 1.0/45.0 );
  testMoments (FUNC (discrete2),  8.5,  9.5, 0 );
  
  testMoments (FUNC (discrete3), -0.5, 0.5, 0.05 );
  testMoments (FUNC (discrete3),  0.5, 1.5, 0.05 );
  testMoments (FUNC (discrete3), -0.5, 9.5, 0.5 );

  test_dirichlet_moments ();
  test_multinomial_moments ();

  testPDF (FUNC2 (beta));
  testPDF (FUNC2 (cauchy));
  testPDF (FUNC2 (chisq));
  testPDF (FUNC2 (chisqnu2));
  testPDF (FUNC2 (dirichlet));
  testPDF (FUNC2 (dirichlet_small));
  testPDF (FUNC2 (erlang));
  testPDF (FUNC2 (exponential));

  testPDF (FUNC2 (exppow0));
  testPDF (FUNC2 (exppow1));
  testPDF (FUNC2 (exppow1a));
  testPDF (FUNC2 (exppow2));
  testPDF (FUNC2 (exppow2a));
  testPDF (FUNC2 (exppow2b));

  testPDF (FUNC2 (fdist));
  testPDF (FUNC2 (fdist_large));
  testPDF (FUNC2 (flat));
  testPDF (FUNC2 (gamma));
  testPDF (FUNC2 (gamma1));
  testPDF (FUNC2 (gamma_int));
  testPDF (FUNC2 (gamma_large));
  testPDF (FUNC2 (gamma_vlarge));
  testPDF (FUNC2 (gamma_knuth_vlarge));
  testPDF (FUNC2 (gamma_small));
  testPDF (FUNC2 (gamma_mt));
  testPDF (FUNC2 (gamma_mt1));
  testPDF (FUNC2 (gamma_mt_int));
  testPDF (FUNC2 (gamma_mt_large));
  testPDF (FUNC2 (gamma_mt_small));
  testPDF (FUNC2 (gaussian));
  testPDF (FUNC2 (gaussian_ratio_method));
  testPDF (FUNC2 (gaussian_ziggurat));
  testPDF (FUNC2 (ugaussian));
  testPDF (FUNC2 (ugaussian_ratio_method));
  testPDF (FUNC2 (gaussian_tail));
  testPDF (FUNC2 (gaussian_tail1));
  testPDF (FUNC2 (gaussian_tail2));
  testPDF (FUNC2 (ugaussian_tail));

  testPDF (FUNC2 (bivariate_gaussian1));
  testPDF (FUNC2 (bivariate_gaussian2));
  testPDF (FUNC2 (bivariate_gaussian3));
  testPDF (FUNC2 (bivariate_gaussian4));

  testPDF (FUNC2 (gumbel1));
  testPDF (FUNC2 (gumbel2));
  testPDF (FUNC2 (landau));
  testPDF (FUNC2 (levy1));
  testPDF (FUNC2 (levy2));
  testPDF (FUNC2 (levy1a));
  testPDF (FUNC2 (levy2a));
  testPDF (FUNC2 (levy_skew1));
  testPDF (FUNC2 (levy_skew2));
  testPDF (FUNC2 (levy_skew1a));
  testPDF (FUNC2 (levy_skew2a));
  testPDF (FUNC2 (levy_skew1b));
  testPDF (FUNC2 (levy_skew2b));
  testPDF (FUNC2 (logistic));
  testPDF (FUNC2 (lognormal));
  testPDF (FUNC2 (pareto));
  testPDF (FUNC2 (rayleigh));
  testPDF (FUNC2 (rayleigh_tail));
  testPDF (FUNC2 (tdist1));
  testPDF (FUNC2 (tdist2));
  testPDF (FUNC2 (laplace));
  testPDF (FUNC2 (weibull));
  testPDF (FUNC2 (weibull1));

  testPDF (FUNC2 (dir2d));
  testPDF (FUNC2 (dir2d_trig_method));
  testPDF (FUNC2 (dir3dxy));
  testPDF (FUNC2 (dir3dyz));
  testPDF (FUNC2 (dir3dzx));

  testDiscretePDF (FUNC2 (discrete1));
  testDiscretePDF (FUNC2 (discrete2));
  testDiscretePDF (FUNC2 (discrete3));
  testDiscretePDF (FUNC2 (poisson));
  testDiscretePDF (FUNC2 (poisson_large));
  testDiscretePDF (FUNC2 (bernoulli));
  testDiscretePDF (FUNC2 (binomial));
  testDiscretePDF (FUNC2 (binomial0));
  testDiscretePDF (FUNC2 (binomial1));
  testDiscretePDF (FUNC2 (binomial_knuth));
  testDiscretePDF (FUNC2 (binomial_large));
  testDiscretePDF (FUNC2 (binomial_large_knuth));
  testDiscretePDF (FUNC2 (binomial_huge));
  testDiscretePDF (FUNC2 (binomial_huge_knuth));
  testDiscretePDF (FUNC2 (binomial_max));
  testDiscretePDF (FUNC2 (geometric));
  testDiscretePDF (FUNC2 (geometric1));
  testDiscretePDF (FUNC2 (hypergeometric1));
  testDiscretePDF (FUNC2 (hypergeometric2));
  testDiscretePDF (FUNC2 (hypergeometric3));
  testDiscretePDF (FUNC2 (hypergeometric4));
  testDiscretePDF (FUNC2 (hypergeometric5));
  testDiscretePDF (FUNC2 (hypergeometric6));
  testDiscretePDF (FUNC2 (logarithmic));
  testDiscretePDF (FUNC2 (multinomial));
  testDiscretePDF (FUNC2 (multinomial_large));
  testDiscretePDF (FUNC2 (negative_binomial));
  testDiscretePDF (FUNC2 (pascal));

  gsl_rng_free (r_global);
  gsl_ran_discrete_free (g1);
  gsl_ran_discrete_free (g2);
  gsl_ran_discrete_free (g3);

  exit (gsl_test_summary ());
}

void
test_shuffle (void)
{
  double count[10][10];
  int x[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  int i, j, status = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 10; j++)
        {
          count[i][j] = 0;
        }
    }

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < 10; j++)
        x[j] = j;

      gsl_ran_shuffle (r_global, x, 10, sizeof (int));

      for (j = 0; j < 10; j++)
        count[x[j]][j]++;
    }

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 10; j++)
        {
          double expected = N / 10.0;
          double d = fabs (count[i][j] - expected);
          double sigma = d / sqrt (expected);
          if (sigma > 5 && d > 1)
            {
              status = 1;
              gsl_test (status,
                        "gsl_ran_shuffle %d,%d (%g observed vs %g expected)",
                        i, j, count[i][j] / N, 0.1);
            }
        }
    }

  gsl_test (status, "gsl_ran_shuffle on {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}");

}

void
test_choose (void)
{
  double count[10];
  int x[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  int y[3] = { 0, 1, 2 };
  int i, j, status = 0;

  for (i = 0; i < 10; i++)
    {
      count[i] = 0;
    }

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < 10; j++)
        x[j] = j;

      gsl_ran_choose (r_global, y, 3, x, 10, sizeof (int));

      for (j = 0; j < 3; j++)
        count[y[j]]++;
    }

  for (i = 0; i < 10; i++)
    {
      double expected = 3.0 * N / 10.0;
      double d = fabs (count[i] - expected);
      double sigma = d / sqrt (expected);
      if (sigma > 5 && d > 1)
        {
          status = 1;
          gsl_test (status,
                    "gsl_ran_choose %d (%g observed vs %g expected)",
                    i, count[i] / N, 0.1);
        }
    }

  gsl_test (status, "gsl_ran_choose (3) on {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}");

}




void
testMoments (double (*f) (void), const char *name,
             double a, double b, double p)
{
  int i;
  double count = 0, expected, sigma;
  int status;

  for (i = 0; i < N; i++)
    {
      double r = f ();
      if (r < b && r > a)
        count++;
    }

  expected = p * N;
  sigma = (expected > 0) ? fabs (count - expected) / sqrt (expected) : fabs(count - expected);

  status = (sigma > 3);

  gsl_test (status, "%s [%g,%g] (%g observed vs %g expected)",
            name, a, b, count / N, p);
}

#define BINS 100

typedef double pdf_func(double);

/* Keep track of invalid values during integration */
static int pdf_errors = 0;
static double pdf_errval = 0.0;

double 
wrapper_function (double x, void *params)
{
  pdf_func * pdf = (pdf_func *)params;
  double P = pdf(x);
  if (!gsl_finite(P)) {  
    pdf_errors++;
    pdf_errval = P;
    P = 0; /* skip invalid value now, but return pdf_errval at the end */
  }
  return P;
}

double
integrate (pdf_func * pdf, double a, double b)
{
  double result, abserr;
  size_t n = 1000;
  gsl_function f;  
  gsl_integration_workspace * w = gsl_integration_workspace_alloc (n);
  f.function = &wrapper_function;
  f.params = (void *)pdf;
  pdf_errors = 0;
  gsl_integration_qags (&f, a, b, 1e-16, 1e-4, n, w, &result, &abserr);
  gsl_integration_workspace_free (w);
  if (pdf_errors) return pdf_errval;
  return result;
}


void
testPDF (double (*f) (void), double (*pdf) (double), const char *name)
{
  double count[BINS], edge[BINS], p[BINS];
  double a = -5.0, b = +5.0;
  double dx = (b - a) / BINS;
  double bin;
  double total = 0, mean;
  int i, j, status = 0, status_i = 0, attempts = 0;
  long int n0 = 0, n = N;

  for (i = 0; i < BINS; i++)
    {
      /* Compute the integral of p(x) from x to x+dx */

      double x = a + i * dx;

      if (fabs (x) < 1e-10)     /* hit the origin exactly */
        x = 0.0;

      p[i]  = integrate (pdf, x, x+dx);
    }


  for (i = 0; i < BINS; i++)
    {
      count[i] = 0;
      edge[i] = 0;
    }

 trial:
  attempts++;

  for (i = n0; i < n; i++)
    {
      double r = f ();
      total += r;

      if (r < b && r > a)
        {
          double u = (r - a) / dx;
          double f = modf(u, &bin);
          j = (int)bin;

          if (f == 0)
            edge[j]++;
          else 
            count[j]++;
        }
    }

  /* Sort out where the hits on the edges should go */

  for (i = 0; i < BINS; i++)
    {
      /* If the bin above is empty, its lower edge hits belong in the
         lower bin */

      if (i + 1 < BINS && count[i+1] == 0) {
        count[i] += edge[i+1];
        edge[i+1] = 0;
      }

      count[i] += edge[i];
      edge[i] = 0;
    }

  mean = (total / n);

  status = !gsl_finite(mean);
  if (status) {
    gsl_test (status, "%s, finite mean, observed %g", name, mean);
    return;
  }

  for (i = 0; i < BINS; i++)
    {
      double x = a + i * dx;
      double d = fabs (count[i] - n * p[i]);
      if (!gsl_finite(p[i])) 
        {
          status_i = 1;
        }
      else if (p[i] != 0)
        {
          double s = d / sqrt (n * p[i]);
          status_i = (s > 5) && (d > 2);
        }
      else
        {
          status_i = (count[i] != 0);
        }
      
      /* Extend the sample if there is an outlier on the first attempt
         to avoid spurious failures when running large numbers of tests. */
      if (status_i && attempts < 50) 
        { 
          n0 = n; 
          n = 2.0*n;
          goto trial;
        }

      status |= status_i;
      if (status_i)
        gsl_test (status_i, "%s [%g,%g) (%g/%d=%g observed vs %g expected)",
                  name, x, x + dx, count[i], n, count[i] / n, p[i]);
    }

  if (status == 0)
    gsl_test (status, "%s, sampling against pdf over range [%g,%g) ",
              name, a, b);
}

void
testDiscretePDF (double (*f) (void), double (*pdf) (unsigned int),
                 const char *name)
{
  double count[BINS], p[BINS];
  unsigned int i;
  int status = 0, status_i = 0;

  for (i = 0; i < BINS; i++)
    count[i] = 0;

  for (i = 0; i < N; i++)
    {
      int r = (int) (f ());
      if (r >= 0 && r < BINS)
        count[r]++;
    }

  for (i = 0; i < BINS; i++)
    p[i] = pdf (i);

  for (i = 0; i < BINS; i++)
    {
      double d = fabs (count[i] - N * p[i]);
      if (p[i] != 0)
        {
          double s = d / sqrt (N * p[i]);
          status_i = (s > 5) && (d > 1);
        }
      else
        {
          status_i = (count[i] != 0);
        }
      status |= status_i;
      if (status_i)
        gsl_test (status_i, "%s i=%d (%g observed vs %g expected)",
                  name, i, count[i] / N, p[i]);
    }

  if (status == 0)
    gsl_test (status, "%s, sampling against pdf over range [%d,%d) ",
              name, 0, BINS);
}



double
test_beta (void)
{
  return gsl_ran_beta (r_global, 2.0, 3.0);
}

double
test_beta_pdf (double x)
{
  return gsl_ran_beta_pdf (x, 2.0, 3.0);
}

double
test_bernoulli (void)
{
  return gsl_ran_bernoulli (r_global, 0.3);
}

double
test_bernoulli_pdf (unsigned int n)
{
  return gsl_ran_bernoulli_pdf (n, 0.3);
}

double
test_binomial (void)
{
  return gsl_ran_binomial (r_global, 0.3, 5);
}

double
test_binomial_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 0.3, 5);
}

double
test_binomial0 (void)
{
  return gsl_ran_binomial (r_global, 0, 8);
}

double
test_binomial0_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 0, 8);
}

double
test_binomial1 (void)
{
  return gsl_ran_binomial (r_global, 1, 8);
}

double
test_binomial1_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 1, 8);
}

double
test_binomial_knuth (void)
{
  return gsl_ran_binomial_knuth (r_global, 0.3, 5);
}

double
test_binomial_knuth_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 0.3, 5);
}


double
test_binomial_large (void)
{
  return gsl_ran_binomial (r_global, 0.3, 55);
}

double
test_binomial_large_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 0.3, 55);
}

double
test_binomial_large_knuth (void)
{
  return gsl_ran_binomial_knuth (r_global, 0.3, 55);
}

double
test_binomial_large_knuth_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 0.3, 55);
}


double
test_binomial_huge (void)
{
  return gsl_ran_binomial (r_global, 0.3, 5500);
}

double
test_binomial_huge_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 0.3, 5500);
}

double
test_binomial_huge_knuth (void)
{
  return gsl_ran_binomial_knuth (r_global, 0.3, 5500);
}

double
test_binomial_huge_knuth_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 0.3, 5500);
}

double
test_binomial_max (void)
{
  return gsl_ran_binomial (r_global, 1e-8, 1<<31);
}

double
test_binomial_max_pdf (unsigned int n)
{
  return gsl_ran_binomial_pdf (n, 1e-8, 1<<31);
}

double
test_cauchy (void)
{
  return gsl_ran_cauchy (r_global, 2.0);
}

double
test_cauchy_pdf (double x)
{
  return gsl_ran_cauchy_pdf (x, 2.0);
}

double
test_chisq (void)
{
  return gsl_ran_chisq (r_global, 13.0);
}

double
test_chisq_pdf (double x)
{
  return gsl_ran_chisq_pdf (x, 13.0);
}

double
test_chisqnu2 (void)
{
  return gsl_ran_chisq (r_global, 2.0);
}

double
test_chisqnu2_pdf (double x)
{
  return gsl_ran_chisq_pdf (x, 2.0);
}


double
test_dir2d (void)
{
  double x = 0, y = 0, theta;
  gsl_ran_dir_2d (r_global, &x, &y);
  theta = atan2 (x, y);
  return theta;
}

double
test_dir2d_pdf (double x)
{
  if (x > -M_PI && x <= M_PI)
    {
      return 1 / (2 * M_PI);
    }
  else
    {
      return 0;
    }
}

double
test_dir2d_trig_method (void)
{
  double x = 0, y = 0, theta;
  gsl_ran_dir_2d_trig_method (r_global, &x, &y);
  theta = atan2 (x, y);
  return theta;
}

double
test_dir2d_trig_method_pdf (double x)
{
  if (x > -M_PI && x <= M_PI)
    {
      return 1 / (2 * M_PI);
    }
  else
    {
      return 0;
    }
}

double
test_dir3dxy (void)
{
  double x = 0, y = 0, z = 0, theta;
  gsl_ran_dir_3d (r_global, &x, &y, &z);
  theta = atan2 (x, y);
  return theta;
}

double
test_dir3dxy_pdf (double x)
{
  if (x > -M_PI && x <= M_PI)
    {
      return 1 / (2 * M_PI);
    }
  else
    {
      return 0;
    }
}

double
test_dir3dyz (void)
{
  double x = 0, y = 0, z = 0, theta;
  gsl_ran_dir_3d (r_global, &x, &y, &z);
  theta = atan2 (y, z);
  return theta;
}

double
test_dir3dyz_pdf (double x)
{
  if (x > -M_PI && x <= M_PI)
    {
      return 1 / (2 * M_PI);
    }
  else
    {
      return 0;
    }
}

double
test_dir3dzx (void)
{
  double x = 0, y = 0, z = 0, theta;
  gsl_ran_dir_3d (r_global, &x, &y, &z);
  theta = atan2 (z, x);
  return theta;
}

double
test_dir3dzx_pdf (double x)
{
  if (x > -M_PI && x <= M_PI)
    {
      return 1 / (2 * M_PI);
    }
  else
    {
      return 0;
    }
}

double
test_dirichlet (void)
{
  /* This is a bit of a lame test, since when K=2, the Dirichlet distribution
     becomes a beta distribution */
  size_t K = 2;
  double alpha[2] = { 2.5, 5.0 };
  double theta[2] = { 0.0, 0.0 };

  gsl_ran_dirichlet (r_global, K, alpha, theta);

  return theta[0];
}

double
test_dirichlet_pdf (double x)
{
  size_t K = 2;
  double alpha[2] = { 2.5, 5.0 };
  double theta[2];

  if (x <= 0.0 || x >= 1.0)
    return 0.0;                 /* Out of range */

  theta[0] = x;
  theta[1] = 1.0 - x;

  return gsl_ran_dirichlet_pdf (K, alpha, theta);
}


double
test_dirichlet_small (void)
{
  size_t K = 2;
  double alpha[2] = { 2.5e-3, 5.0e-3};
  double theta[2] = { 0.0, 0.0 };

  gsl_ran_dirichlet (r_global, K, alpha, theta);

  return theta[0];
}

double
test_dirichlet_small_pdf (double x)
{
  size_t K = 2;
  double alpha[2] = { 2.5e-3, 5.0e-3 };
  double theta[2];

  if (x <= 0.0 || x >= 1.0)
    return 0.0;                 /* Out of range */

  theta[0] = x;
  theta[1] = 1.0 - x;

  return gsl_ran_dirichlet_pdf (K, alpha, theta);
}


/* Check that the observed means of the Dirichlet variables are
   within reasonable statistical errors of their correct values. */

#define DIRICHLET_K 10

void
test_dirichlet_moments (void)
{
  double alpha[DIRICHLET_K];
  double theta[DIRICHLET_K];
  double theta_sum[DIRICHLET_K];

  double alpha_sum = 0.0;
  double mean, obs_mean, sd, sigma;
  int status, k, n;

  for (k = 0; k < DIRICHLET_K; k++)
    {
      alpha[k] = gsl_ran_exponential (r_global, 0.1);
      alpha_sum += alpha[k];
      theta_sum[k] = 0.0;
    }

  for (n = 0; n < N; n++)
    {
      gsl_ran_dirichlet (r_global, DIRICHLET_K, alpha, theta);
      for (k = 0; k < DIRICHLET_K; k++)
        theta_sum[k] += theta[k];
    }

  for (k = 0; k < DIRICHLET_K; k++)
    {
      mean = alpha[k] / alpha_sum;
      sd =
        sqrt ((alpha[k] * (1. - alpha[k] / alpha_sum)) /
              (alpha_sum * (alpha_sum + 1.)));
      obs_mean = theta_sum[k] / N;
      sigma = sqrt ((double) N) * fabs (mean - obs_mean) / sd;

      status = (sigma > 3.0);

      gsl_test (status,
                "test gsl_ran_dirichlet: mean (%g observed vs %g expected)",
                obs_mean, mean);
    }
}


/* Check that the observed means of the multinomial variables are
   within reasonable statistical errors of their correct values. */

void
test_multinomial_moments (void)
{
  const unsigned int sum_n = 100;

  const double p[MULTI_DIM] ={ 0.2, 0.20, 0.17, 0.14, 0.12,
                               0.07, 0.05, 0.02, 0.02, 0.01 };

  unsigned int  x[MULTI_DIM];
  double x_sum[MULTI_DIM];

  double mean, obs_mean, sd, sigma;
  int status, k, n;

  for (k = 0; k < MULTI_DIM; k++)
    x_sum[k] =0.0;

  for (n = 0; n < N; n++)
    {
      gsl_ran_multinomial (r_global, MULTI_DIM, sum_n, p, x);
      for (k = 0; k < MULTI_DIM; k++)
        x_sum[k] += x[k];
    }

  for (k = 0; k < MULTI_DIM; k++)
    {
      mean = p[k] * sum_n;
      sd = p[k] * (1.-p[k]) * sum_n;

      obs_mean = x_sum[k] / N;
      sigma = sqrt ((double) N) * fabs (mean - obs_mean) / sd;

      status = (sigma > 3.0);

      gsl_test (status,
                "test gsl_ran_multinomial: mean (%g observed vs %g expected)",
                obs_mean, mean);
    }
}


double
test_discrete1 (void)
{
  static double P[3] = { 0.59, 0.4, 0.01 };
  if (g1 == NULL)
    {
      g1 = gsl_ran_discrete_preproc (3, P);
    }
  return gsl_ran_discrete (r_global, g1);
}

double
test_discrete1_pdf (unsigned int n)
{
  return gsl_ran_discrete_pdf ((size_t) n, g1);
}

double
test_discrete2 (void)
{
  static double P[10] = { 1, 9, 3, 4, 5, 8, 6, 7, 2, 0 };
  if (g2 == NULL)
    {
      g2 = gsl_ran_discrete_preproc (10, P);
    }
  return gsl_ran_discrete (r_global, g2);
}

double
test_discrete2_pdf (unsigned int n)
{
  return gsl_ran_discrete_pdf ((size_t) n, g2);
}
double
test_discrete3 (void)
{
  static double P[20];
  if (g3 == NULL)
    { int i;
      for (i=0; i<20; ++i) P[i]=1.0/20;
      g3 = gsl_ran_discrete_preproc (20, P);
    }
  return gsl_ran_discrete (r_global, g3);
}

double
test_discrete3_pdf (unsigned int n)
{
  return gsl_ran_discrete_pdf ((size_t) n, g3);
}


double
test_erlang (void)
{
  return gsl_ran_erlang (r_global, 3.0, 4.0);
}

double
test_erlang_pdf (double x)
{
  return gsl_ran_erlang_pdf (x, 3.0, 4.0);
}

double
test_exponential (void)
{
  return gsl_ran_exponential (r_global, 2.0);
}

double
test_exponential_pdf (double x)
{
  return gsl_ran_exponential_pdf (x, 2.0);
}

double
test_exppow0 (void)
{
  return gsl_ran_exppow (r_global, 3.7, 0.3);
}

double
test_exppow0_pdf (double x)
{
  return gsl_ran_exppow_pdf (x, 3.7, 0.3);
}

double
test_exppow1 (void)
{
  return gsl_ran_exppow (r_global, 3.7, 1.0);
}

double
test_exppow1_pdf (double x)
{
  return gsl_ran_exppow_pdf (x, 3.7, 1.0);
}

double
test_exppow1a (void)
{
  return gsl_ran_exppow (r_global, 3.7, 1.9);
}

double
test_exppow1a_pdf (double x)
{
  return gsl_ran_exppow_pdf (x, 3.7, 1.9);
}

double
test_exppow2 (void)
{
  return gsl_ran_exppow (r_global, 3.7, 2.0);
}

double
test_exppow2_pdf (double x)
{
  return gsl_ran_exppow_pdf (x, 3.7, 2.0);
}


double
test_exppow2a (void)
{
  return gsl_ran_exppow (r_global, 3.7, 3.5);
}

double
test_exppow2a_pdf (double x)
{
  return gsl_ran_exppow_pdf (x, 3.7, 3.5);
}

double
test_exppow2b (void)
{
  return gsl_ran_exppow (r_global, 3.7, 7.5);
}

double
test_exppow2b_pdf (double x)
{
  return gsl_ran_exppow_pdf (x, 3.7, 7.5);
}

double
test_fdist (void)
{
  return gsl_ran_fdist (r_global, 3.0, 4.0);
}

double
test_fdist_pdf (double x)
{
  return gsl_ran_fdist_pdf (x, 3.0, 4.0);
}

/* Test case for bug #28500: overflow in gsl_ran_fdist_pdf */

double
test_fdist_large (void)
{
  return gsl_ran_fdist (r_global, 8.0, 249.0);
}

double
test_fdist_large_pdf (double x)
{
  return gsl_ran_fdist_pdf (x, 8.0, 249.0);
}

double
test_flat (void)
{
  return gsl_ran_flat (r_global, 3.0, 4.0);
}

double
test_flat_pdf (double x)
{
  return gsl_ran_flat_pdf (x, 3.0, 4.0);
}

double
test_gamma (void)
{
  return gsl_ran_gamma (r_global, 2.5, 2.17);
}

double
test_gamma_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 2.5, 2.17);
}

double
test_gamma1 (void)
{
  return gsl_ran_gamma (r_global, 1.0, 2.17);
}

double
test_gamma1_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 1.0, 2.17);
}


double
test_gamma_int (void)
{
  return gsl_ran_gamma (r_global, 10.0, 2.17);
}

double
test_gamma_int_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 10.0, 2.17);
}


double
test_gamma_large (void)
{
  return gsl_ran_gamma (r_global, 20.0, 2.17);
}

double
test_gamma_large_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 20.0, 2.17);
}

double
test_gamma_small (void)
{
  return gsl_ran_gamma (r_global, 0.92, 2.17);
}

double
test_gamma_small_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 0.92, 2.17);
}

double
test_gamma_vlarge (void)
{
  /* Scale the distribution to get it into the range [-5,5] */
  double c = 2.71828181565;
  double b = 6.32899304917e-10;
  double d = 1e4;
  return (gsl_ran_gamma (r_global, 4294967296.0, b) - c) * d;
}

double
test_gamma_vlarge_pdf (double x)
{
  double c = 2.71828181565;
  double b = 6.32899304917e-10;
  double d = 1e4;
  return gsl_ran_gamma_pdf ((x / d) + c, 4294967296.0, b) / d;
}

double
test_gamma_mt (void)
{
  return gsl_ran_gamma_mt (r_global, 2.5, 2.17);
}

double
test_gamma_mt_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 2.5, 2.17);
}

double
test_gamma_mt1 (void)
{
  return gsl_ran_gamma_mt (r_global, 1.0, 2.17);
}

double
test_gamma_mt1_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 1.0, 2.17);
}


double
test_gamma_mt_int (void)
{
  return gsl_ran_gamma_mt (r_global, 10.0, 2.17);
}

double
test_gamma_mt_int_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 10.0, 2.17);
}


double
test_gamma_mt_large (void)
{
  return gsl_ran_gamma_mt (r_global, 20.0, 2.17);
}

double
test_gamma_mt_large_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 20.0, 2.17);
}


double
test_gamma_mt_small (void)
{
  return gsl_ran_gamma_mt (r_global, 0.92, 2.17);
}

double
test_gamma_mt_small_pdf (double x)
{
  return gsl_ran_gamma_pdf (x, 0.92, 2.17);
}


double
test_gamma_knuth_vlarge (void)
{
  /* Scale the distribution to get it into the range [-5,5] */
  double c = 2.71828181565;
  double b = 6.32899304917e-10;
  double d = 1e4;
  return (gsl_ran_gamma_knuth (r_global, 4294967296.0, b) - c) * d;
}

double
test_gamma_knuth_vlarge_pdf (double x)
{
  double c = 2.71828181565;
  double b = 6.32899304917e-10;
  double d = 1e4;
  return gsl_ran_gamma_pdf ((x / d) + c, 4294967296.0, b) / d;
}

double
test_gaussian (void)
{
  return gsl_ran_gaussian (r_global, 3.0);
}

double
test_gaussian_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, 3.0);
}

double
test_gaussian_ratio_method (void)
{
  return gsl_ran_gaussian_ratio_method (r_global, 3.0);
}

double
test_gaussian_ratio_method_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, 3.0);
}

double
test_gaussian_ziggurat (void)
{
  return gsl_ran_gaussian_ziggurat (r_global, 3.12);
}

double
test_gaussian_ziggurat_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, 3.12);
}

double
test_gaussian_tail (void)
{
  return gsl_ran_gaussian_tail (r_global, 1.7, 0.25);
}

double
test_gaussian_tail_pdf (double x)
{
  return gsl_ran_gaussian_tail_pdf (x, 1.7, 0.25);
}

double
test_gaussian_tail1 (void)
{
  return gsl_ran_gaussian_tail (r_global, -1.7, 5.0);
}

double
test_gaussian_tail1_pdf (double x)
{
  return gsl_ran_gaussian_tail_pdf (x, -1.7, 5.0);
}

double
test_gaussian_tail2 (void)
{
  return gsl_ran_gaussian_tail (r_global, 0.1, 2.0);
}

double
test_gaussian_tail2_pdf (double x)
{
  return gsl_ran_gaussian_tail_pdf (x, 0.1, 2.0);
}


double
test_ugaussian (void)
{
  return gsl_ran_ugaussian (r_global);
}

double
test_ugaussian_pdf (double x)
{
  return gsl_ran_ugaussian_pdf (x);
}

double
test_ugaussian_ratio_method (void)
{
  return gsl_ran_ugaussian_ratio_method (r_global);
}

double
test_ugaussian_ratio_method_pdf (double x)
{
  return gsl_ran_ugaussian_pdf (x);
}

double
test_ugaussian_tail (void)
{
  return gsl_ran_ugaussian_tail (r_global, 3.0);
}

double
test_ugaussian_tail_pdf (double x)
{
  return gsl_ran_ugaussian_tail_pdf (x, 3.0);
}

double
test_bivariate_gaussian1 (void)
{
  double x = 0, y = 0;
  gsl_ran_bivariate_gaussian (r_global, 3.0, 2.0, 0.3, &x, &y);
  return x;
}

double
test_bivariate_gaussian1_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, 3.0);
}

double
test_bivariate_gaussian2 (void)
{
  double x = 0, y = 0;
  gsl_ran_bivariate_gaussian (r_global, 3.0, 2.0, 0.3, &x, &y);
  return y;
}

double
test_bivariate_gaussian2_pdf (double y)
{
  int i, n = 10;
  double sum = 0;
  double a = -10, b = 10, dx = (b - a) / n;
  for (i = 0; i < n; i++)
    {
      double x = a + i * dx;
      sum += gsl_ran_bivariate_gaussian_pdf (x, y, 3.0, 2.0, 0.3) * dx;
    }
  return sum;
}


double
test_bivariate_gaussian3 (void)
{
  double x = 0, y = 0;
  gsl_ran_bivariate_gaussian (r_global, 3.0, 2.0, 0.3, &x, &y);
  return x + y;
}

double
test_bivariate_gaussian3_pdf (double x)
{
  double sx = 3.0, sy = 2.0, r = 0.3;
  double su = (sx + r * sy);
  double sv = sy * sqrt (1 - r * r);
  double sigma = sqrt (su * su + sv * sv);

  return gsl_ran_gaussian_pdf (x, sigma);
}

double
test_bivariate_gaussian4 (void)
{
  double x = 0, y = 0;
  gsl_ran_bivariate_gaussian (r_global, 3.0, 2.0, -0.5, &x, &y);
  return x + y;
}

double
test_bivariate_gaussian4_pdf (double x)
{
  double sx = 3.0, sy = 2.0, r = -0.5;
  double su = (sx + r * sy);
  double sv = sy * sqrt (1 - r * r);
  double sigma = sqrt (su * su + sv * sv);

  return gsl_ran_gaussian_pdf (x, sigma);
}


double
test_geometric (void)
{
  return gsl_ran_geometric (r_global, 0.5);
}

double
test_geometric_pdf (unsigned int n)
{
  return gsl_ran_geometric_pdf (n, 0.5);
}

double
test_geometric1 (void)
{
  return gsl_ran_geometric (r_global, 1.0);
}

double
test_geometric1_pdf (unsigned int n)
{
  return gsl_ran_geometric_pdf (n, 1.0);
}

double
test_hypergeometric1 (void)
{
  return gsl_ran_hypergeometric (r_global, 5, 7, 4);
}

double
test_hypergeometric1_pdf (unsigned int n)
{
  return gsl_ran_hypergeometric_pdf (n, 5, 7, 4);
}


double
test_hypergeometric2 (void)
{
  return gsl_ran_hypergeometric (r_global, 5, 7, 11);
}

double
test_hypergeometric2_pdf (unsigned int n)
{
  return gsl_ran_hypergeometric_pdf (n, 5, 7, 11);
}

double
test_hypergeometric3 (void)
{
  return gsl_ran_hypergeometric (r_global, 5, 7, 1);
}

double
test_hypergeometric3_pdf (unsigned int n)
{
  return gsl_ran_hypergeometric_pdf (n, 5, 7, 1);
}

double
test_hypergeometric4 (void)
{
  return gsl_ran_hypergeometric (r_global, 5, 7, 20);
}

double
test_hypergeometric4_pdf (unsigned int n)
{
  return gsl_ran_hypergeometric_pdf (n, 5, 7, 20);
}

double
test_hypergeometric5 (void)
{
  return gsl_ran_hypergeometric (r_global, 2, 7, 5);
}

double
test_hypergeometric5_pdf (unsigned int n)
{
  return gsl_ran_hypergeometric_pdf (n, 2, 7, 5);
}


double
test_hypergeometric6 (void)
{
  return gsl_ran_hypergeometric (r_global, 2, 10, 3);
}

double
test_hypergeometric6_pdf (unsigned int n)
{
  return gsl_ran_hypergeometric_pdf (n, 2, 10, 3);
}




double
test_gumbel1 (void)
{
  return gsl_ran_gumbel1 (r_global, 3.12, 4.56);
}

double
test_gumbel1_pdf (double x)
{
  return gsl_ran_gumbel1_pdf (x, 3.12, 4.56);
}

double
test_gumbel2 (void)
{
  return gsl_ran_gumbel2 (r_global, 3.12, 4.56);
}

double
test_gumbel2_pdf (double x)
{
  return gsl_ran_gumbel2_pdf (x, 3.12, 4.56);
}

double
test_landau (void)
{
  return gsl_ran_landau (r_global);
}

double
test_landau_pdf (double x)
{
  return gsl_ran_landau_pdf (x);
}

double
test_levy1 (void)
{
  return gsl_ran_levy (r_global, 5.0, 1.0);
}

double
test_levy1_pdf (double x)
{
  return gsl_ran_cauchy_pdf (x, 5.0);
}

double
test_levy2 (void)
{
  return gsl_ran_levy (r_global, 5.0, 2.0);
}

double
test_levy2_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, sqrt (2.0) * 5.0);
}

double
test_levy1a (void)
{
  return gsl_ran_levy (r_global, 5.0, 1.01);
}

double
test_levy1a_pdf (double x)
{
  return gsl_ran_cauchy_pdf (x, 5.0);
}

double
test_levy2a (void)
{
  return gsl_ran_levy (r_global, 5.0, 1.99);
}

double
test_levy2a_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, sqrt (2.0) * 5.0);
}


double
test_levy_skew1 (void)
{
  return gsl_ran_levy_skew (r_global, 5.0, 1.0, 0.0);
}

double
test_levy_skew1_pdf (double x)
{
  return gsl_ran_cauchy_pdf (x, 5.0);
}

double
test_levy_skew2 (void)
{
  return gsl_ran_levy_skew (r_global, 5.0, 2.0, 0.0);
}

double
test_levy_skew2_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, sqrt (2.0) * 5.0);
}

double
test_levy_skew1a (void)
{
  return gsl_ran_levy_skew (r_global, 5.0, 1.01, 0.0);
}

double
test_levy_skew1a_pdf (double x)
{
  return gsl_ran_cauchy_pdf (x, 5.0);
}

double
test_levy_skew2a (void)
{
  return gsl_ran_levy_skew (r_global, 5.0, 1.99, 0.0);
}

double
test_levy_skew2a_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, sqrt (2.0) * 5.0);
}

double
test_levy_skew1b (void)
{
  return gsl_ran_levy_skew (r_global, 5.0, 1.01, 0.001);
}

double
test_levy_skew1b_pdf (double x)
{
  return gsl_ran_cauchy_pdf (x, 5.0);
}

double
test_levy_skew2b (void)
{
  return gsl_ran_levy_skew (r_global, 5.0, 1.99, 0.001);
}

double
test_levy_skew2b_pdf (double x)
{
  return gsl_ran_gaussian_pdf (x, sqrt (2.0) * 5.0);
}


double
test_logistic (void)
{
  return gsl_ran_logistic (r_global, 3.1);
}

double
test_logistic_pdf (double x)
{
  return gsl_ran_logistic_pdf (x, 3.1);
}

double
test_logarithmic (void)
{
  return gsl_ran_logarithmic (r_global, 0.4);
}

double
test_logarithmic_pdf (unsigned int n)
{
  return gsl_ran_logarithmic_pdf (n, 0.4);
}


double
test_lognormal (void)
{
  return gsl_ran_lognormal (r_global, 2.7, 1.3);
}

double
test_lognormal_pdf (double x)
{
  return gsl_ran_lognormal_pdf (x, 2.7, 1.3);
}

double
test_multinomial (void)
{
  const size_t K = 3;
  const unsigned int sum_n = BINS;
  unsigned int n[3];
  /* Test use of weights instead of probabilities. */
  const double p[] = { 2., 7., 1.};

  gsl_ran_multinomial ( r_global, K, sum_n, p, n);

  return n[0];
}

double
test_multinomial_pdf (unsigned int n_0)
{
  /* The margional distribution of just 1 variate  is binomial. */
  size_t K = 2;
  /* Test use of weights instead of probabilities */
  double p[] = { 0.4, 1.6};
  const unsigned int sum_n = BINS;
  unsigned int n[2];

  n[0] = n_0;
  n[1] =sum_n - n_0;

  return gsl_ran_multinomial_pdf (K, p, n);
}


double
test_multinomial_large (void)
{
  const unsigned int sum_n = BINS;
  unsigned int n[MULTI_DIM];
  const double p[MULTI_DIM] = { 0.2, 0.20, 0.17, 0.14, 0.12,
                                0.07, 0.05, 0.04, 0.01, 0.00  };

  gsl_ran_multinomial ( r_global, MULTI_DIM, sum_n, p, n);

  return n[0];
}

double
test_multinomial_large_pdf (unsigned int n_0)
{
  return test_multinomial_pdf(n_0);
}

double
test_negative_binomial (void)
{
  return gsl_ran_negative_binomial (r_global, 0.3, 20.0);
}

double
test_negative_binomial_pdf (unsigned int n)
{
  return gsl_ran_negative_binomial_pdf (n, 0.3, 20.0);
}

double
test_pascal (void)
{
  return gsl_ran_pascal (r_global, 0.8, 3);
}

double
test_pascal_pdf (unsigned int n)
{
  return gsl_ran_pascal_pdf (n, 0.8, 3);
}


double
test_pareto (void)
{
  return gsl_ran_pareto (r_global, 1.9, 2.75);
}

double
test_pareto_pdf (double x)
{
  return gsl_ran_pareto_pdf (x, 1.9, 2.75);
}

double
test_rayleigh (void)
{
  return gsl_ran_rayleigh (r_global, 1.9);
}

double
test_rayleigh_pdf (double x)
{
  return gsl_ran_rayleigh_pdf (x, 1.9);
}

double
test_rayleigh_tail (void)
{
  return gsl_ran_rayleigh_tail (r_global, 2.7, 1.9);
}

double
test_rayleigh_tail_pdf (double x)
{
  return gsl_ran_rayleigh_tail_pdf (x, 2.7, 1.9);
}


double
test_poisson (void)
{
  return gsl_ran_poisson (r_global, 5.0);
}

double
test_poisson_pdf (unsigned int n)
{
  return gsl_ran_poisson_pdf (n, 5.0);
}

double
test_poisson_large (void)
{
  return gsl_ran_poisson (r_global, 30.0);
}

double
test_poisson_large_pdf (unsigned int n)
{
  return gsl_ran_poisson_pdf (n, 30.0);
}


double
test_tdist1 (void)
{
  return gsl_ran_tdist (r_global, 1.75);
}

double
test_tdist1_pdf (double x)
{
  return gsl_ran_tdist_pdf (x, 1.75);
}

double
test_tdist2 (void)
{
  return gsl_ran_tdist (r_global, 12.75);
}

double
test_tdist2_pdf (double x)
{
  return gsl_ran_tdist_pdf (x, 12.75);
}


double
test_laplace (void)
{
  return gsl_ran_laplace (r_global, 2.75);
}

double
test_laplace_pdf (double x)
{
  return gsl_ran_laplace_pdf (x, 2.75);
}

double
test_weibull (void)
{
  return gsl_ran_weibull (r_global, 3.14, 2.75);
}

double
test_weibull_pdf (double x)
{
  return gsl_ran_weibull_pdf (x, 3.14, 2.75);
}


double
test_weibull1 (void)
{
  return gsl_ran_weibull (r_global, 2.97, 1.0);
}

double
test_weibull1_pdf (double x)
{
  return gsl_ran_weibull_pdf (x, 2.97, 1.0);
}
