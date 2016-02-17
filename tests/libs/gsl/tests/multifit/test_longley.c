size_t longley_n = 16;
size_t longley_p = 7;

double longley_x [] = {
  1,  83.0,   234289,   2356,     1590,    107608,  1947,
  1,  88.5,   259426,   2325,     1456,    108632,  1948,
  1,  88.2,   258054,   3682,     1616,    109773,  1949,
  1,  89.5,   284599,   3351,     1650,    110929,  1950,
  1,  96.2,   328975,   2099,     3099,    112075,  1951,
  1,  98.1,   346999,   1932,     3594,    113270,  1952,
  1,  99.0,   365385,   1870,     3547,    115094,  1953,
  1, 100.0,   363112,   3578,     3350,    116219,  1954,
  1, 101.2,   397469,   2904,     3048,    117388,  1955,
  1, 104.6,   419180,   2822,     2857,    118734,  1956,
  1, 108.4,   442769,   2936,     2798,    120445,  1957,
  1, 110.8,   444546,   4681,     2637,    121950,  1958,
  1, 112.6,   482704,   3813,     2552,    123366,  1959,
  1, 114.2,   502601,   3931,     2514,    125368,  1960,
  1, 115.7,   518173,   4806,     2572,    127852,  1961,
  1, 116.9,   554894,   4007,     2827,    130081,  1962 } ;

double longley_y[] = {60323, 61122, 60171, 61187, 63221, 63639, 64989, 63761,
                       66019, 67857, 68169, 66513, 68655, 69564, 69331, 70551};

static int
test_longley_results(const char *str,
                     const gsl_vector *c, const gsl_vector *expected_c,
                     const gsl_vector *cov_diag, const gsl_vector *expected_sd,
                     const double chisq, const double chisq_res,
                     const double expected_chisq)
{
  size_t i;

  /* test coefficient vector */
  for (i = 0; i < longley_p; ++i)
    {
      gsl_test_rel (gsl_vector_get(c,i),
                    gsl_vector_get(expected_c, i),
                    1.0e-10,
                    "%s c[%zu]", str, i) ;

      if (cov_diag && expected_sd)
        {
          gsl_test_rel (sqrt(gsl_vector_get(cov_diag, i)),
                        gsl_vector_get(expected_sd, i),
                        1e-10,
                        "%s cov[%zu,%zu]", str, i, i);
        }
    }

  gsl_test_rel (chisq, expected_chisq, 1.0e-10, "%s chisq", str);
  gsl_test_rel (chisq_res, expected_chisq, 1.0e-10, "%s chisq residuals", str);

  return GSL_SUCCESS;
}

void 
test_longley ()
{     
  gsl_multifit_linear_workspace * work = 
    gsl_multifit_linear_alloc (longley_n, longley_p);

  gsl_multifit_robust_workspace * work_rob =
    gsl_multifit_robust_alloc (gsl_multifit_robust_ols, longley_n, longley_p);

  gsl_matrix_view X = gsl_matrix_view_array (longley_x, longley_n, longley_p);
  gsl_vector_view y = gsl_vector_view_array (longley_y, longley_n);
  gsl_vector * c = gsl_vector_alloc (longley_p);
  gsl_vector * r = gsl_vector_alloc (longley_n);
  gsl_matrix * cov = gsl_matrix_alloc (longley_p, longley_p);

  double chisq, chisq_res;

  double expected_c[7] = {  -3482258.63459582,
                            15.0618722713733,
                            -0.358191792925910E-01,
                            -2.02022980381683,
                            -1.03322686717359,
                            -0.511041056535807E-01,
                            1829.15146461355 };

  double expected_sd[7]  = {  890420.383607373,      
                              84.9149257747669,      
                              0.334910077722432E-01, 
                              0.488399681651699,     
                              0.214274163161675,     
                              0.226073200069370,     
                              455.478499142212 } ;  

  double expected_chisq = 836424.055505915;

  gsl_vector_view diag = gsl_matrix_diagonal (cov);
  gsl_vector_view exp_c = gsl_vector_view_array(expected_c, longley_p);
  gsl_vector_view exp_sd = gsl_vector_view_array(expected_sd, longley_p);

  /* test unweighted least squares */
  gsl_multifit_linear (&X.matrix, &y.vector, c, cov, &chisq, work);
  gsl_multifit_linear_residuals(&X.matrix, &y.vector, c, r);
  gsl_blas_ddot(r, r, &chisq_res);

  test_longley_results("longley gsl_multifit_linear",
                       c, &exp_c.vector,
                       &diag.vector, &exp_sd.vector,
                       chisq, chisq_res, expected_chisq);

  /* test robust least squares */
  gsl_multifit_robust (&X.matrix, &y.vector, c, cov, work_rob);

  test_longley_results("longley gsl_multifit_robust",
                       c, &exp_c.vector,
                       &diag.vector, &exp_sd.vector,
                       1.0, 1.0, 1.0);

  /* test weighted least squares */
  {
    size_t i, j;

    gsl_vector * w = gsl_vector_alloc (longley_n);

    double expected_cov[7][7] = { { 8531122.56783558,
-166.727799925578, 0.261873708176346, 3.91188317230983,
1.1285582054705, -0.889550869422687, -4362.58709870581},

{-166.727799925578, 0.0775861253030891, -1.98725210399982e-05,
-0.000247667096727256, -6.82911920718824e-05, 0.000136160797527761,
0.0775255245956248},

{0.261873708176346, -1.98725210399982e-05, 1.20690316701888e-08,
1.66429546772984e-07, 3.61843600487847e-08, -6.78805814483582e-08,
-0.00013158719037715},

{3.91188317230983, -0.000247667096727256, 1.66429546772984e-07,
2.56665052544717e-06, 6.96541409215597e-07, -9.00858307771567e-07,
-0.00197260370663974},

{1.1285582054705, -6.82911920718824e-05, 3.61843600487847e-08,
6.96541409215597e-07, 4.94032602583969e-07, -9.8469143760973e-08,
-0.000576921112208274},

{-0.889550869422687, 0.000136160797527761, -6.78805814483582e-08,
-9.00858307771567e-07, -9.8469143760973e-08, 5.49938542664952e-07,
0.000430074434198215},

{-4362.58709870581, 0.0775255245956248, -0.00013158719037715,
-0.00197260370663974, -0.000576921112208274, 0.000430074434198215,
2.23229587481535 }} ;

    gsl_vector_set_all (w, 1.0);

    gsl_multifit_wlinear (&X.matrix, w, &y.vector, c, cov, &chisq, work);
    gsl_multifit_linear_residuals(&X.matrix, &y.vector, c, r);
    gsl_blas_ddot(r, r, &chisq_res);

    test_longley_results("longley gsl_multifit_wlinear",
                         c, &exp_c.vector,
                         NULL, NULL,
                         chisq, chisq_res, expected_chisq);

    for (i = 0; i < longley_p; i++) 
      {
        for (j = 0; j < longley_p; j++)
          {
            gsl_test_rel (gsl_matrix_get(cov,i,j), expected_cov[i][j], 1e-7, 
                          "longley gsl_multifit_wlinear cov(%d,%d)", i, j) ;
          }
      }

    gsl_vector_free(w);
  }

  gsl_vector_free(c);
  gsl_vector_free(r);
  gsl_matrix_free(cov);
  gsl_multifit_linear_free (work);
  gsl_multifit_robust_free (work_rob);
} /* test_longley() */
