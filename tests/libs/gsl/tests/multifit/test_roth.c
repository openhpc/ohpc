#define roth_N         2
#define roth_P         2

#define roth_NTRIES    3

static double roth_x0[roth_P] = { 0.5, -2.0 };
static double roth_epsrel = 1.0e-8;

static void
roth_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double sumsq_exact1 = 0.0;
  const double roth_x1[roth_P] = { 5.0, 4.0 };
  const double sumsq_exact2 = 48.9842536792400;
  const double roth_x2[roth_P] = { 11.4127789869021, -0.896805253274477 };
  const double *roth_x;
  double sumsq_exact;

  if (fabs(sumsq) < 0.1)
    {
      sumsq_exact = sumsq_exact1;
      roth_x = roth_x1;
    }
  else
    {
      sumsq_exact = sumsq_exact2;
      roth_x = roth_x2;
    }

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < roth_P; ++i)
    {
      gsl_test_rel(x[i], roth_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
roth_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);

  gsl_vector_set(f, 0, x1 - x2*(2.0 - x2*(5.0 - x2)) - 13.0);
  gsl_vector_set(f, 1, x1 - x2*(14.0 - x2*(1.0 + x2)) - 29.0);

  return GSL_SUCCESS;
}

static int
roth_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x2 = gsl_vector_get(x, 1);

  gsl_matrix_set(J, 0, 0, 1.0);
  gsl_matrix_set(J, 0, 1, -2.0 + x2*(10.0 - 3.0*x2));
  gsl_matrix_set(J, 1, 0, 1.0);
  gsl_matrix_set(J, 1, 1, -14.0 + x2*(2.0 + 3.0*x2));

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf roth_func =
{
  &roth_f,
  &roth_df,
  NULL,
  roth_N,
  roth_P,
  NULL,
  0,
  0
};

static test_fdf_problem roth_problem =
{
  "roth_freudenstein",
  roth_x0,
  NULL,
  &roth_epsrel,
  roth_NTRIES,
  &roth_checksol,
  &roth_func
};
