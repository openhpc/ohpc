#define osborne_N         33
#define osborne_P         5

#define osborne_NTRIES    3

static double osborne_x0[osborne_P] = { 0.5, 1.5, -1.0, 0.01, 0.02 };
static double osborne_epsrel = 1.0e-8;

static double osborne_Y[osborne_N] = {
0.844, 0.908, 0.932, 0.936, 0.925, 0.908, 0.881,
0.850, 0.818, 0.784, 0.751, 0.718, 0.685, 0.658,
0.628, 0.603, 0.580, 0.558, 0.538, 0.522, 0.506,
0.490, 0.478, 0.467, 0.457, 0.448, 0.438, 0.431,
0.424, 0.420, 0.414, 0.411, 0.406
};

static void
osborne_checksol(const double x[], const double sumsq,
                 const double epsrel, const char *sname,
                 const char *pname)
{
  const double sumsq_exact = 5.464894697482687e-05;
  const double osborne_x[osborne_P] = {
    3.754100521058740e-01, GSL_NAN, GSL_NAN, GSL_NAN, GSL_NAN };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  /* only the first model parameter is uniquely constrained */
  gsl_test_rel(x[0], osborne_x[0], epsrel, "%s/%s i=0",
               sname, pname);
}

static int
osborne_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double x1 = gsl_vector_get(x, 0);
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  double x4 = gsl_vector_get(x, 3);
  double x5 = gsl_vector_get(x, 4);
  size_t i;

  for (i = 0; i < osborne_N; ++i)
    {
      double ti = 10.0*i;
      double yi = osborne_Y[i];
      double fi = yi - (x1 + x2*exp(-x4*ti) + x3*exp(-x5*ti));
      gsl_vector_set(f, i, fi);
    }

  return GSL_SUCCESS;
}

static int
osborne_df (const gsl_vector * x, void *params, gsl_matrix * J)
{
  double x2 = gsl_vector_get(x, 1);
  double x3 = gsl_vector_get(x, 2);
  double x4 = gsl_vector_get(x, 3);
  double x5 = gsl_vector_get(x, 4);
  size_t i;

  for (i = 0; i < osborne_N; ++i)
    {
      double ti = 10.0*i;
      double term1 = exp(-x4*ti);
      double term2 = exp(-x5*ti);

      gsl_matrix_set(J, i, 0, -1.0);
      gsl_matrix_set(J, i, 1, -term1);
      gsl_matrix_set(J, i, 2, -term2);
      gsl_matrix_set(J, i, 3, ti*x2*term1);
      gsl_matrix_set(J, i, 4, ti*x3*term2);
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf osborne_func =
{
  &osborne_f,
  &osborne_df,
  NULL,
  osborne_N,
  osborne_P,
  NULL,
  0,
  0
};

static test_fdf_problem osborne_problem =
{
  "osborne",
  osborne_x0,
  NULL,
  &osborne_epsrel,
  osborne_NTRIES,
  &osborne_checksol,
  &osborne_func
};
