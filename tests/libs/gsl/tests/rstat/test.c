/* rstat/test.c
 * 
 * Copyright (C) 2015, 2016, 2017, 2018, 2019, 2020, 2021 Patrick Alken
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
#include <math.h>
#include <string.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_rstat.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_ieee_utils.h>

int
random_data(const size_t n, double data[], gsl_rng *r)
{
  size_t i;

  for (i = 0; i < n; ++i)
    data[i] = 2.0 * gsl_rng_uniform(r) - 1.0;

  return 0;
}

void
test_basic(const size_t n, const double data[], const double tol, const char * desc)
{
  gsl_rstat_workspace *rstat_workspace_p = gsl_rstat_alloc();
  const double expected_mean = gsl_stats_mean(data, 1, n);
  const double expected_skew = gsl_stats_skew(data, 1, n);
  const double expected_kurtosis = gsl_stats_kurtosis(data, 1, n);
  double expected_rms = 0.0;
  double expected_norm;
  double mean, var, sd, sd_mean, rms, skew, kurtosis, norm;
  size_t i, num;
  int status;

  /* compute expected rms */
  for (i = 0; i < n; ++i)
    expected_rms += data[i] * data[i];

  expected_norm = sqrt(expected_rms);
  expected_rms = sqrt(expected_rms / n);

  /* add data to rstat workspace */
  for (i = 0; i < n; ++i)
    gsl_rstat_add(data[i], rstat_workspace_p);

  mean     = gsl_rstat_mean(rstat_workspace_p);
  rms      = gsl_rstat_rms(rstat_workspace_p);
  norm     = gsl_rstat_norm(rstat_workspace_p);
  skew     = gsl_rstat_skew(rstat_workspace_p);
  kurtosis = gsl_rstat_kurtosis(rstat_workspace_p);
  num      = gsl_rstat_n(rstat_workspace_p);

  gsl_test_int(num, n, "%s n n=%zu", desc, n);
  gsl_test_rel(mean, expected_mean, tol, "%s mean n=%zu", desc, n);
  gsl_test_rel(rms, expected_rms, tol, "%s rms n=%zu", desc, n);
  gsl_test_rel(norm, expected_norm, tol, "%s norm n=%zu", desc, n);
  gsl_test_rel(skew, expected_skew, tol, "%s skew n=%zu", desc, n);
  gsl_test_rel(kurtosis, expected_kurtosis, tol, "%s kurtosis n=%zu", desc, n);
  
  if (n > 1)
    {
      const double expected_var = gsl_stats_variance(data, 1, n);
      const double expected_sd = gsl_stats_sd(data, 1, n);
      const double expected_sd_mean = expected_sd / sqrt((double) n);

      var      = gsl_rstat_variance(rstat_workspace_p);
      sd       = gsl_rstat_sd(rstat_workspace_p);
      sd_mean  = gsl_rstat_sd_mean(rstat_workspace_p);

      gsl_test_rel(var, expected_var, tol, "%s variance n=%zu", desc, n);
      gsl_test_rel(sd, expected_sd, tol, "%s stddev n=%zu", desc, n);
      gsl_test_rel(sd_mean, expected_sd_mean, tol, "%s stddev_mean n=%zu", desc, n);
    }

  if (n <= 5)
    {
      /* median should be exact for n <= 5 */
      double * data_copy = malloc(n * sizeof(double));
      double expected_median, median;

      memcpy(data_copy, data, n * sizeof(double));
      expected_median = gsl_stats_median(data_copy, 1, n);
      median = gsl_rstat_median(rstat_workspace_p);

      gsl_test_rel(median, expected_median, tol, "%s median n=%zu", desc, n);

      free(data_copy);
    }

  status = gsl_rstat_reset(rstat_workspace_p);
  gsl_test_int(status, GSL_SUCCESS, "%s: rstat returned success", desc);
  num = gsl_rstat_n(rstat_workspace_p);

  gsl_test_int(num, 0, "n n=%zu" , n);

  gsl_rstat_free(rstat_workspace_p);
}

void
test_quantile(const double p, const double data[], const size_t n,
              const double expected, const double tol, const char *desc)
{
  gsl_rstat_quantile_workspace *w = gsl_rstat_quantile_alloc(p);
  double result;
  size_t i;

  for (i = 0; i < n; ++i)
    gsl_rstat_quantile_add(data[i], w);

  result = gsl_rstat_quantile_get(w);

  if (fabs(expected) < 1.0e-4)
    gsl_test_abs(result, expected, tol, "%s p=%g", desc, p);
  else
    gsl_test_rel(result, expected, tol, "%s p=%g", desc, p);

  gsl_rstat_quantile_free(w);
}

int
main()
{
  gsl_rng *r = gsl_rng_alloc(gsl_rng_default);
  const double tol1 = 1.0e-8;
  const double tol2 = 1.0e-3;

  gsl_ieee_env_setup();

  {
    const size_t N = 2000000;
    double *data = malloc(N * sizeof(double));
    double data2[5];
    size_t i, j;
    char buf[64];

    /* test1: test on small datasets n <= 5 (median will be exact in this case) */
    for (i = 0; i < 100; ++i)
      {
        random_data(5, data2, r);

        for (j = 1; j <= 5; ++j)
          {
            sprintf(buf, "test1 j=%zu", j);
            test_basic(j, data2, tol1, buf);
          }
      }

    /* test2: test on large datasets */

    random_data(N, data, r);

    for (i = 1; i <= 10; ++i)
      test_basic(i, data, tol1, "test2");

    test_basic(100, data, tol1, "test2");
    test_basic(1000, data, tol1, "test2");
    test_basic(10000, data, tol1, "test2");
    test_basic(50000, data, tol1, "test2");
    test_basic(80000, data, tol1, "test2");
    test_basic(1500000, data, tol1, "test2");
    test_basic(2000000, data, tol1, "test2");

    /* test3: add large constant */

    for (i = 0; i < 5; ++i)
      data2[i] += 1.0e9;

    test_basic(5, data2, 1.0e-6, "test3");

    free(data);
  }

  {
    /* dataset from Jain and Chlamtac paper */
    const size_t n_jain = 20;
    const double data_jain[] = {  0.02,  0.15,  0.74,  3.39,  0.83,
                                  22.37, 10.15, 15.43, 38.62, 15.92,
                                  34.60, 10.28,  1.47,  0.40,  0.05,
                                  11.39,  0.27,  0.42,  0.09, 11.37 };
    double expected_jain = 4.44063435326;
  
    test_quantile(0.5, data_jain, n_jain, expected_jain, tol1, "jain");
  }

  {
    size_t n = 1000000;
    double *data = malloc(n * sizeof(double));
    double *sorted_data = malloc(n * sizeof(double));
    gsl_rstat_workspace *rstat_workspace_p = gsl_rstat_alloc();
    double p;
    size_t i;

    for (i = 0; i < n; ++i)
      {
        data[i] = gsl_ran_gaussian_tail(r, 1.3, 1.0);
        gsl_rstat_add(data[i], rstat_workspace_p);
      }

    memcpy(sorted_data, data, n * sizeof(double));
    gsl_sort(sorted_data, 1, n);

    /* test quantile calculation */
    for (p = 0.1; p <= 0.9; p += 0.1)
      {
        double expected = gsl_stats_quantile_from_sorted_data(sorted_data, 1, n, p);
        test_quantile(p, data, n, expected, tol2, "gauss");
      }

    /* test mean, variance */
    {
      const double expected_mean = gsl_stats_mean(data, 1, n);
      const double expected_var = gsl_stats_variance(data, 1, n);
      const double expected_sd = gsl_stats_sd(data, 1, n);
      const double expected_skew = gsl_stats_skew(data, 1, n);
      const double expected_kurtosis = gsl_stats_kurtosis(data, 1, n);
      const double expected_median = gsl_stats_quantile_from_sorted_data(sorted_data, 1, n, 0.5);

      const double mean = gsl_rstat_mean(rstat_workspace_p);
      const double var = gsl_rstat_variance(rstat_workspace_p);
      const double sd = gsl_rstat_sd(rstat_workspace_p);
      const double skew = gsl_rstat_skew(rstat_workspace_p);
      const double kurtosis = gsl_rstat_kurtosis(rstat_workspace_p);
      const double median = gsl_rstat_median(rstat_workspace_p);

      gsl_test_rel(mean, expected_mean, tol1, "mean");
      gsl_test_rel(var, expected_var, tol1, "variance");
      gsl_test_rel(sd, expected_sd, tol1, "stddev");
      gsl_test_rel(skew, expected_skew, tol1, "skew");
      gsl_test_rel(kurtosis, expected_kurtosis, tol1, "kurtosis");
      gsl_test_abs(median, expected_median, tol2, "median");
    }

    free(data);
    free(sorted_data);
    gsl_rstat_free(rstat_workspace_p);
  }

  gsl_rng_free(r);

  exit (gsl_test_summary());
}
