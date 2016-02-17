#define jennrich_N         10
#define jennrich_P         2

#define jennrich_NTRIES    1

static double jennrich_x0[jennrich_P] = { 0.3, 0.4 };
static double jennrich_epsrel = 1.0e-8;

static void
jennrich_checksol(const double x[], const double sumsq,
                  const double epsrel, const char *sname,
                  const char *pname)
{
  size_t i;
  const double sumsq_exact = 1.243621823556148e+02;
  const double jennrich_x[jennrich_P] = { 2.578252139935855e-01,
                                          2.578252133471426e-01 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < jennrich_P; ++i)
    {
      gsl_test_rel(x[i], jennrich_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
jennrich_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  size_t i;

  for (i = 0; i < jennrich_N; ++i)
    {
      double ip1 = i + 1.0;
      double fi = 2.0*(i + 2.0) - (exp(x1*ip1) + exp(x2*ip1));
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
jennrich_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  size_t i;

  for (i = 0; i < jennrich_N; ++i)
    {
      double ip1 = i + 1.0;

      gsl_matrix_set(J, i, 0, -ip1*exp(ip1*x1));
      gsl_matrix_set(J, i, 1, -ip1*exp(ip1*x2));
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf jennrich_func =
{
  &jennrich_f,
  &jennrich_df,
  NULL,
  jennrich_N,
  jennrich_P,
  NULL,
  0,
  0
};

static test_fdf_problem jennrich_problem =
{
  "jennrich",
  jennrich_x0,
  NULL,
  &jennrich_epsrel,
  jennrich_NTRIES,
  &jennrich_checksol,
  &jennrich_func
};
