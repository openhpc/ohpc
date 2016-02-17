#define vardim_N         7 /* p + 2 */
#define vardim_P         5

#define vardim_NTRIES    4

static double vardim_x0[vardim_P] = { 0.8, 0.6, 0.4, 0.2, 0.0 };

static double vardim_epsrel = 1.0e-12;

static void
vardim_checksol(const double x[], const double sumsq,
                const double epsrel, const char *sname,
                const char *pname)
{
  size_t i;
  const double sumsq_exact = 0.0;

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < vardim_P; ++i)
    {
      gsl_test_rel(x[i], 1.0, epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
vardim_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  size_t i;
  double sum = 0.0;

  for (i = 0; i < vardim_P; ++i)
    {
      double xi = gsl_vector_get(x, i);

      gsl_vector_set(f, i, xi - 1.0);

      sum += (i + 1.0) * (xi - 1.0);
    }

  gsl_vector_set(f, vardim_P, sum);
  gsl_vector_set(f, vardim_P + 1, sum*sum);

  return GSL_SUCCESS;
}

static int
vardim_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  size_t i;
  double sum = 0.0;
  gsl_matrix_view m = gsl_matrix_submatrix(J, 0, 0, vardim_P, vardim_P);

  gsl_matrix_set_identity(&m.matrix);

  for (i = 0; i < vardim_P; ++i)
    {
      double xi = gsl_vector_get(x, i);
      sum += (i + 1.0) * (xi - 1.0);
    }

  for (i = 0; i < vardim_P; ++i)
    {
      gsl_matrix_set(J, vardim_P, i, i + 1.0);
      gsl_matrix_set(J, vardim_P + 1, i, 2*(i + 1.0)*sum);
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf vardim_func =
{
  &vardim_f,
  &vardim_df,
  NULL,
  vardim_N,
  vardim_P,
  NULL,
  0,
  0
};

static test_fdf_problem vardim_problem =
{
  "vardim",
  vardim_x0,
  NULL,
  &vardim_epsrel,
  vardim_NTRIES,
  &vardim_checksol,
  &vardim_func
};
