#define boxbod_N       6
#define boxbod_P       2

#define boxbod_NTRIES  1

static double boxbod_x0[boxbod_P] = { 100.0, 0.75 };
static double boxbod_epsrel = 1.0e-7;

static double boxbod_sigma[boxbod_P] = {
  1.2354515176E+01, 1.0455993237E-01
};

static double boxbod_X[boxbod_N] = { 1.0, 2.0, 3.0, 5.0, 7.0, 10.0 };

static double boxbod_F[boxbod_N] = { 109.0, 149.0, 149.0, 191.0,
                                     213.0, 224.0 };

static void
boxbod_checksol(const double x[], const double sumsq,
                 const double epsrel, const char *sname,
                 const char *pname)
{
  size_t i;
  const double sumsq_exact = 1.1680088766E+03;
  const double boxbod_x[boxbod_P] = { 2.1380940889E+02,
                                      5.4723748542E-01 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < boxbod_P; ++i)
    {
      gsl_test_rel(x[i], boxbod_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}


static int
boxbod_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double b[boxbod_P];
  size_t i;

  for (i = 0; i < boxbod_P; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < boxbod_N; i++)
    {
      double xi = boxbod_X[i];
      double yi;

      yi = b[0] * (1.0 - exp(-b[1] * xi));
      gsl_vector_set (f, i, yi - boxbod_F[i]);
    }

  return GSL_SUCCESS;
}

static int
boxbod_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double b[boxbod_P];
  size_t i;

  for (i = 0; i < boxbod_P; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < boxbod_N; i++)
    {
      double xi = boxbod_X[i];
      double term = exp(-b[1] * xi);

      gsl_matrix_set (df, i, 0, 1.0 - term);
      gsl_matrix_set (df, i, 1, b[0] * term * xi);
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf boxbod_func =
{
  &boxbod_f,
  &boxbod_df,
  NULL,
  boxbod_N,
  boxbod_P,
  NULL,
  0,
  0
};

static test_fdf_problem boxbod_problem =
{
  "nist-boxbod",
  boxbod_x0,
  boxbod_sigma,
  &boxbod_epsrel,
  boxbod_NTRIES,
  &boxbod_checksol,
  &boxbod_func
};
