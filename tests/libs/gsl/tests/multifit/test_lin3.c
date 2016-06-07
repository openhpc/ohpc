#define lin3_N         50  /* can be anything >= p */
#define lin3_P         10  /* >= 3 */

#define lin3_NTRIES    3

static double lin3_x0[lin3_P] = { 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 };
static double lin3_epsrel = 1.0e-10;

static void
lin3_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double n = (double) lin3_N;
  const double sumsq_exact = 0.5 * (n*n + 3*n - 6.0) / (2*n - 3.0);
  const double sum_exact = 3.0 / (2.0*n - 3.0);
  double sum = 0.0;

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 1; i < lin3_P - 1; ++i)
    sum += (i + 1.0) * x[i];

  gsl_test_rel(sum, sum_exact, epsrel, "%s/%s coeff sum",
               sname, pname);
}

static int
lin3_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  size_t i, j;

  gsl_vector_set(f, 0, -1.0);
  gsl_vector_set(f, lin3_N - 1, -1.0);

  for (i = 1; i < lin3_N - 1; ++i)
    {
      double fi = 0.0;

      for (j = 1; j < lin3_P - 1; ++j)
        {
          double xj = gsl_vector_get(x, j);
          fi += (j + 1) * xj;
        }

      fi = i * fi - 1.0;
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
lin3_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  size_t i, j;

  gsl_matrix_set_zero(J);

  for (i = 1; i < lin3_N - 1; ++i)
    {
      for (j = 1; j < lin3_P - 1; ++j)
        {
          gsl_matrix_set(J, i, j, i * (j + 1.0));
        }
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf lin3_func =
{
  &lin3_f,
  &lin3_df,
  NULL,
  lin3_N,
  lin3_P,
  NULL,
  0,
  0
};

static test_fdf_problem lin3_problem =
{
  "linear_rank1zeros",
  lin3_x0,
  NULL,
  &lin3_epsrel,
  lin3_NTRIES,
  &lin3_checksol,
  &lin3_func
};
