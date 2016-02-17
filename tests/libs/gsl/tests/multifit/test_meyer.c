#define meyer_N         16
#define meyer_P         3

#define meyer_NTRIES    1

static double meyer_x0[meyer_P] = { 0.02, 4000.0, 250.0 };
static double meyer_epsrel = 1.0e-8;

static double meyer_Y[meyer_N] = {
34780., 28610., 23650., 19630., 16370., 13720., 11540.,
9744.,  8261.,  7030.,  6005., 5147., 4427., 3820.,
3307.,  2872.
};

static void
meyer_checksol(const double x[], const double sumsq,
               const double epsrel, const char *sname,
               const char *pname)
{
  size_t i;
  const double sumsq_exact = 8.794585517053883e+01;
  const double meyer_x[meyer_P] = { 5.609636471049458e-03,
                                    6.181346346283188e+03,
                                    3.452236346240292e+02 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < meyer_P; ++i)
    {
      gsl_test_rel(x[i], meyer_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}

static int
meyer_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  size_t i;

  for (i = 0; i < meyer_N; ++i)
    {
      double ti = 45.0 + 5.0*(i + 1.0);
      double yi = meyer_Y[i];
      double fi = x1 * exp(x2 / (ti + x3)) - yi;
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
meyer_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  size_t i;

  for (i = 0; i < meyer_N; ++i)
    {
      double ti = 45.0 + 5.0*(i + 1.0);
      double term1 = ti + x3;
      double term2 = exp(x2 / term1);

      gsl_matrix_set(J, i, 0, term2);
      gsl_matrix_set(J, i, 1, x1*term2/term1);
      gsl_matrix_set(J, i, 2, -x1*x2*term2/(term1*term1));
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf meyer_func =
{
  &meyer_f,
  &meyer_df,
  NULL,
  meyer_N,
  meyer_P,
  NULL,
  0,
  0
};

static test_fdf_problem meyer_problem =
{
  "meyer",
  meyer_x0,
  NULL,
  &meyer_epsrel,
  meyer_NTRIES,
  &meyer_checksol,
  &meyer_func
};
