#define penalty1_N         11 /* p + 1 */
#define penalty1_P         10

#define penalty1_NTRIES    4

static double penalty1_x0[penalty1_P] = { 1.0, 2.0, 3.0, 4.0, 5.0,
                                          6.0, 7.0, 8.0, 9.0, 10.0 };

static double penalty1_epsrel = 1.0e-12;

static void
penalty1_checksol(const double x[], const double sumsq,
                  const double epsrel, const char *sname,
                  const char *pname)
{
  const double sumsq_exact = 7.08765146709037993e-05;

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);
}

static int
penalty1_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  const double alpha = 1.0e-5;
  const double sqrt_alpha = sqrt(alpha);
  size_t i;
  double sum = 0.0;

  for (i = 0; i < penalty1_P; ++i)
    {
      double xi = gsl_vector_get(x, i);

      gsl_vector_set(f, i, sqrt_alpha*(xi - 1.0));

      sum += xi * xi;
    }

  gsl_vector_set(f, penalty1_P, sum - 0.25);

  return GSL_SUCCESS;
}

static int
penalty1_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  const double alpha = 1.0e-5;
  const double sqrt_alpha = sqrt(alpha);
  size_t i;
  gsl_matrix_view m = gsl_matrix_submatrix(J, 0, 0, penalty1_P, penalty1_P);
  gsl_vector_view diag = gsl_matrix_diagonal(&m.matrix);

  gsl_matrix_set_zero(&m.matrix);
  gsl_vector_set_all(&diag.vector, sqrt_alpha);

  for (i = 0; i < penalty1_P; ++i)
    {
      double xi = gsl_vector_get(x, i);
      gsl_matrix_set(J, penalty1_P, i, 2.0 * xi);
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf penalty1_func =
{
  &penalty1_f,
  &penalty1_df,
  NULL,
  penalty1_N,
  penalty1_P,
  NULL,
  0,
  0
};

static test_fdf_problem penalty1_problem =
{
  "penalty1",
  penalty1_x0,
  NULL,
  &penalty1_epsrel,
  penalty1_NTRIES,
  &penalty1_checksol,
  &penalty1_func
};
