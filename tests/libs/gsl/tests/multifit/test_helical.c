#define helical_N         3
#define helical_P         3

#define helical_NTRIES    4

static double helical_x0[helical_P] = { -1.0, 0.0, 0.0 };
static double helical_x[helical_P] = { 1.0, 0.0, 0.0 };

static double helical_epsrel = 1.0e-12;

static void
helical_checksol(const double x[], const double sumsq,
                 const double epsrel, const char *sname,
                 const char *pname)
{
  size_t i;
  const double sumsq_exact = 0.0;

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < helical_P; ++i)
    {
      gsl_test_rel(x[i], helical_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
helical_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  double theta = (x1 >= 0.0) ? 0.0 : 5.0;
  double nx = gsl_hypot(x1, x2);

  gsl_vector_set(f, 0, 10.0 * (x3 - 5.0/M_PI*atan(x2 / x1) - theta));
  gsl_vector_set(f, 1, 10.0*(nx - 1.0));
  gsl_vector_set(f, 2, x3);

  return GSL_SUCCESS;
}

static int
helical_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double nx = gsl_hypot(x1, x2);
  double nx_sq = nx * nx;
  double term1 = 50.0 / (M_PI * nx_sq);
  double term2 = 10.0 / nx;

  gsl_matrix_set(J, 0, 0, term1*x2);
  gsl_matrix_set(J, 0, 1, -term1*x1);
  gsl_matrix_set(J, 0, 2, 10.0);

  gsl_matrix_set(J, 1, 0, term2*x1);
  gsl_matrix_set(J, 1, 1, term2*x2);
  gsl_matrix_set(J, 1, 2, 0.0);

  gsl_matrix_set(J, 2, 0, 0.0);
  gsl_matrix_set(J, 2, 1, 0.0);
  gsl_matrix_set(J, 2, 2, 1.0);

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf helical_func =
{
  &helical_f,
  &helical_df,
  NULL,
  helical_N,
  helical_P,
  NULL,
  0,
  0
};

static test_fdf_problem helical_problem =
{
  "helical",
  helical_x0,
  NULL,
  &helical_epsrel,
  helical_NTRIES,
  &helical_checksol,
  &helical_func
};
