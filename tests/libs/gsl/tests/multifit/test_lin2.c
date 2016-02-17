#define lin2_N         20  /* can be anything >= p */
#define lin2_P         5

#define lin2_NTRIES    3

static double lin2_x0[lin2_P] = { 1.0, 1.0, 1.0, 1.0, 1.0 };
static double lin2_epsrel = 1.0e-11;

static void
lin2_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double n = (double) lin2_N;
  const double sumsq_exact = 0.5 * (n*(n - 1.0)) / (2.0*n + 1.0);
  const double sum_exact = 3.0 / (2.0*n + 1.0);
  double sum = 0.0;

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < lin2_P; ++i)
    sum += (i + 1.0) * x[i];

  gsl_test_rel(sum, sum_exact, epsrel, "%s/%s coeff sum",
               sname, pname);
}

static int
lin2_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  size_t i, j;

  for (i = 0; i < lin2_N; ++i)
    {
      double fi = 0.0;

      for (j = 0; j < lin2_P; ++j)
        {
          double xj = gsl_vector_get(x, j);
          fi += (j + 1) * xj;
        }

      fi = (i + 1) * fi - 1.0;
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
lin2_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  size_t i, j;

  for (i = 0; i < lin2_N; ++i)
    {
      for (j = 0; j < lin2_P; ++j)
        {
          gsl_matrix_set(J, i, j, (i + 1.0) * (j + 1.0));
        }
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf lin2_func =
{
  &lin2_f,
  &lin2_df,
  NULL,
  lin2_N,
  lin2_P,
  NULL,
  0,
  0
};

static test_fdf_problem lin2_problem =
{
  "linear_rank1",
  lin2_x0,
  NULL,
  &lin2_epsrel,
  lin2_NTRIES,
  &lin2_checksol,
  &lin2_func
};
