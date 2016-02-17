#define thurber_N       37
#define thurber_P       7

#define thurber_NTRIES  1

static double thurber_x0[thurber_P] = { 1000.0, 1000.0, 400.0, 40.0,
                                        0.7, 0.3, 0.03 };

static double thurber_epsrel = 1.0e-6;

static double thurber_sigma[thurber_P] = {
  4.6647963344E+00, 3.9571156086E+01, 2.8698696102E+01,
  5.5675370270E+00, 3.1333340687E-02, 1.4984928198E-02,
  6.5842344623E-03
};

static double thurber_X[thurber_N] = {
  -3.067, -2.981, -2.921, -2.912, -2.840, -2.797, -2.702,
  -2.699, -2.633, -2.481, -2.363, -2.322, -1.501, -1.460,
  -1.274, -1.212, -1.100, -1.046, -0.915, -0.714, -0.566,
  -0.545, -0.400, -0.309, -0.109, -0.103, 0.010, 0.119,
  0.377, 0.790, 0.963, 1.006, 1.115, 1.572, 1.841,
  2.047, 2.200
};

static double thurber_F[thurber_N] = {
  80.574, 84.248, 87.264, 87.195, 89.076, 89.608, 89.868, 90.101,
  92.405, 95.854, 100.696, 101.060, 401.672, 390.724, 567.534,
  635.316, 733.054, 759.087, 894.206, 990.785, 1090.109, 1080.914,
  1122.643, 1178.351, 1260.531, 1273.514, 1288.339, 1327.543, 1353.863,
  1414.509, 1425.208, 1421.384, 1442.962, 1464.350, 1468.705,
  1447.894, 1457.628
};

static void
thurber_checksol(const double x[], const double sumsq,
                 const double epsrel, const char *sname,
                 const char *pname)
{
  size_t i;
  const double sumsq_exact = 5.6427082397E+03;
  const double thurber_x[thurber_P] = {
    1.2881396800E+03, 1.4910792535E+03, 5.8323836877E+02,
    7.5416644291E+01, 9.6629502864E-01, 3.9797285797E-01,
    4.9727297349E-02 };

  gsl_test_rel(sumsq, sumsq_exact, epsrel, "%s/%s sumsq",
               sname, pname);

  for (i = 0; i < thurber_P; ++i)
    {
      gsl_test_rel(x[i], thurber_x[i], epsrel, "%s/%s i=%zu",
                   sname, pname, i);
    }
}


static int
thurber_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double b[thurber_P];
  size_t i;

  for (i = 0; i < thurber_P; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < thurber_N; i++)
    {
      double xi = thurber_X[i];
      double yi;

      yi = b[0] + b[1]*xi + b[2]*xi*xi + b[3]*xi*xi*xi;
      yi /= 1.0 + b[4]*xi + b[5]*xi*xi + b[6]*xi*xi*xi;

      gsl_vector_set (f, i, yi - thurber_F[i]);
    }

  return GSL_SUCCESS;
}

static int
thurber_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double b[thurber_P];
  size_t i;

  for (i = 0; i < thurber_P; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < thurber_N; i++)
    {
      double xi = thurber_X[i];
      double d, n, d_sq;

      n = b[0] + b[1]*xi + b[2]*xi*xi + b[3]*xi*xi*xi;
      d = 1.0 + b[4]*xi + b[5]*xi*xi + b[6]*xi*xi*xi;
      d_sq = d * d;

      gsl_matrix_set (df, i, 0, 1.0 / d);
      gsl_matrix_set (df, i, 1, xi / d);
      gsl_matrix_set (df, i, 2, (xi * xi) / d);
      gsl_matrix_set (df, i, 3, (xi * xi * xi) / d);
      gsl_matrix_set (df, i, 4, -xi * n / d_sq);
      gsl_matrix_set (df, i, 5, -xi * xi * n / d_sq);
      gsl_matrix_set (df, i, 6, -xi * xi * xi * n / d_sq);
    }

  return GSL_SUCCESS;
}

static gsl_multifit_function_fdf thurber_func =
{
  &thurber_f,
  &thurber_df,
  NULL,
  thurber_N,
  thurber_P,
  NULL,
  0,
  0
};

static test_fdf_problem thurber_problem =
{
  "nist-thurber",
  thurber_x0,
  thurber_sigma,
  &thurber_epsrel,
  thurber_NTRIES,
  &thurber_checksol,
  &thurber_func
};
