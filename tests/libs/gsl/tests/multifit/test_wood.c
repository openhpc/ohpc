#define wood_N         6
#define wood_P         4

#define wood_NTRIES    3

static double wood_x0[wood_P] = { -3.0, -1.0, -3.0, -1.0 };
static double wood_epsrel = 1.0e-12;

static void
wood_checksol(const double x[], const double sumsq,
              const double epsrel, const char *sname,
              const char *pname)
{
  size_t i;
  const double sumsq_exact = 0.0;
  const double wood_x[wood_P] = { 1.0, 1.0, 1.0, 1.0 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < wood_P; ++i)
    {
      gsl_test_rel(x[i], wood_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
wood_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  double x4 = gsl_vector_get(x, 3);

  gsl_vector_set(f, 0, 10.0*(x2 - x1*x1));
  gsl_vector_set(f, 1, 1.0 - x1);
  gsl_vector_set(f, 2, sqrt(90.0)*(x4 - x3*x3));
  gsl_vector_set(f, 3, 1.0 - x3);
  gsl_vector_set(f, 4, sqrt(10.0)*(x2 + x4 - 2.0));
  gsl_vector_set(f, 5, (x2 - x4) / sqrt(10.0));

  return GSL_SUCCESS;
}

static int
wood_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x1 = gsl_vector_get(x, 0);
  double x3 = gsl_vector_get(x, 2);
  double s90 = sqrt(90.0);
  double s10 = sqrt(10.0);

  gsl_matrix_set_zero(J);

  gsl_matrix_set(J, 0, 0, -20.0*x1);
  gsl_matrix_set(J, 0, 1, 10.0);
  gsl_matrix_set(J, 1, 0, -1.0);
  gsl_matrix_set(J, 2, 2, -2.0*s90*x3);
  gsl_matrix_set(J, 2, 3, s90);
  gsl_matrix_set(J, 3, 2, -1.0);
  gsl_matrix_set(J, 4, 1, s10);
  gsl_matrix_set(J, 4, 3, s10);
  gsl_matrix_set(J, 5, 1, 1.0/s10);
  gsl_matrix_set(J, 5, 3, -1.0/s10);

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf wood_func =
{
  &wood_f,
  &wood_df,
  NULL,
  wood_N,
  wood_P,
  NULL,
  0,
  0
};

static test_fdf_problem wood_problem =
{
  "wood",
  wood_x0,
  NULL,
  &wood_epsrel,
  wood_NTRIES,
  &wood_checksol,
  &wood_func
};
