/*
 * test_shaw.c
 *
 * Test L-curve (Tikhonov) regression routines using Shaw
 * problem. See example 1.10 of
 *
 * [1] R.C. Aster, B. Borchers and C. H. Thurber,
 *     Parameter Estimation and Inverse Problems (2nd ed), 2012.
 */

#include <gsl/gsl_sf_trig.h>

/* construct design matrix and rhs vector for Shaw problem */
static int
shaw_system(gsl_matrix * X, gsl_vector * y)
{
  int s = GSL_SUCCESS;
  const size_t n = X->size1;
  const size_t p = X->size2;
  const double dtheta = M_PI / (double) p;
  size_t i, j;
  gsl_vector *m = gsl_vector_alloc(p);

  /* build the design matrix */
  for (i = 0; i < n; ++i)
    {
      double si = (i + 0.5) * M_PI / n - M_PI / 2.0;
      double csi = cos(si);
      double sni = sin(si);

      for (j = 0; j < p; ++j)
        {
          double thetaj = (j + 0.5) * M_PI / p - M_PI / 2.0;
          double term1 = csi + cos(thetaj);
          double term2 = gsl_sf_sinc(sni + sin(thetaj));
          double Xij = term1 * term1 * term2 * term2 * dtheta;

          gsl_matrix_set(X, i, j, Xij);
        }
    }

  /* construct coefficient vector */
  {
    const double a1 = 2.0;
    const double a2 = 1.0;
    const double c1 = 6.0;
    const double c2 = 2.0;
    const double t1 = 0.8;
    const double t2 = -0.5;

    for (j = 0; j < p; ++j)
      {
        double tj = -M_PI / 2.0 + (j + 0.5) * dtheta;
        double mj = a1 * exp(-c1 * (tj - t1) * (tj - t1)) +
                    a2 * exp(-c2 * (tj - t2) * (tj - t2));
        gsl_vector_set(m, j, mj);
      }
  }

  /* construct rhs vector */
  gsl_blas_dgemv(CblasNoTrans, 1.0, X, m, 0.0, y);

  gsl_vector_free(m);

  return s;
}

static int
test_shaw_system(gsl_rng *rng_p, const size_t n, const size_t p,
                 const double lambda_expected,
                 gsl_vector *rhs)
{
  const size_t npoints = 1000; /* number of points on L-curve */
  const double tol1 = 1.0e-12;
  const double tol2 = 1.0e-10;
  const double tol3 = 1.0e-5;
  gsl_vector * reg_param = gsl_vector_alloc(npoints);
  gsl_vector * rho = gsl_vector_alloc(npoints);
  gsl_vector * eta = gsl_vector_alloc(npoints);

  gsl_matrix * X = gsl_matrix_alloc(n, p);
  gsl_matrix * cov = gsl_matrix_alloc(p, p);
  gsl_vector * c = gsl_vector_alloc(p);
  gsl_vector * ytmp = gsl_vector_alloc(n);
  gsl_vector * y;
  gsl_vector * r = gsl_vector_alloc(n);
  gsl_multifit_linear_workspace * work = 
    gsl_multifit_linear_alloc (n, p);

  size_t reg_idx, i;
  double lambda, rnorm, snorm;

  /* build design matrix */
  shaw_system(X, ytmp);

  if (rhs)
    y = rhs;
  else
    {
      y = ytmp;

      /* add random noise to exact rhs vector */
      test_random_vector_noise(rng_p, y);
    }

  /* SVD decomposition */
  gsl_multifit_linear_svd(X, work);

  /* calculate L-curve */
  gsl_multifit_linear_lcurve(y, reg_param, rho, eta, work);

  /* test rho and eta vectors */
  for (i = 0; i < npoints; ++i)
    {
      double rhoi = gsl_vector_get(rho, i);
      double etai = gsl_vector_get(eta, i);
      double lami = gsl_vector_get(reg_param, i);

      /* solve regularized system and check for consistent rho/eta values */
      gsl_multifit_linear_solve(lami, X, y, c, &rnorm, &snorm, work);
      gsl_test_rel(rhoi, rnorm, tol3, "shaw rho n=%zu p=%zu lambda=%e",
                   n, p, lami);
      gsl_test_rel(etai, snorm, tol1, "shaw eta n=%zu p=%zu lambda=%e",
                   n, p, lami);
    }

  /* calculate corner of L-curve */
  gsl_multifit_linear_lcorner(rho, eta, &reg_idx);

  lambda = gsl_vector_get(reg_param, reg_idx);

  /* test against known lambda value if given */
  if (lambda_expected > 0.0)
    {
      gsl_test_rel(lambda, lambda_expected, tol1,
                   "shaw: n=%zu p=%zu L-curve corner lambda",
                   n, p);
    }

  /* compute regularized solution with optimal lambda */
  gsl_multifit_linear_solve(lambda, X, y, c, &rnorm, &snorm, work);

  /* compute residual norm ||y - X c|| */
  gsl_vector_memcpy(r, y);
  gsl_blas_dgemv(CblasNoTrans, 1.0, X, c, -1.0, r);

  /* test rnorm value */
  gsl_test_rel(rnorm, gsl_blas_dnrm2(r), tol2,
               "shaw: n=%zu p=%zu rnorm", n, p);

  /* test snorm value */
  gsl_test_rel(snorm, gsl_blas_dnrm2(c), tol2,
               "shaw: n=%zu p=%zu snorm", n, p);

  gsl_matrix_free(X);
  gsl_matrix_free(cov);
  gsl_vector_free(reg_param);
  gsl_vector_free(rho);
  gsl_vector_free(eta);
  gsl_vector_free(r);
  gsl_vector_free(c);
  gsl_vector_free(ytmp);
  gsl_multifit_linear_free(work);

  return 0;
} /* test_shaw_system() */

void
test_shaw(void)
{
  gsl_rng * r = gsl_rng_alloc(gsl_rng_default);

  {
    double shaw20_y[] = {
      8.7547455124379323e-04, 5.4996835885761936e-04,
      1.7527999407005367e-06, 1.9552372913117047e-03,
      1.4411645433785081e-02, 5.2800013336393704e-02,
      1.3609152023257112e-01, 2.7203484587635818e-01,
      4.3752225136193390e-01, 5.7547667319875240e-01,
      6.2445052213539942e-01, 5.6252658286441348e-01,
      4.2322239923561566e-01, 2.6768469219560631e-01,
      1.4337901162734543e-01, 6.5614569346074361e-02,
      2.6013851831752945e-02, 9.2336933089481269e-03,
      3.2269066658993694e-03, 1.3999201459261811e-03 };
    gsl_vector_view rhs = gsl_vector_view_array(shaw20_y, 20);

    /* lambda and rhs values from [1] */
    test_shaw_system(r, 20, 20, 5.793190958069266e-06, &rhs.vector);
  }

  {
    size_t n, p;

    for (n = 10; n <= 50; n += 2)
      {
        for (p = n - 6; p <= n; p += 2)
          test_shaw_system(r, n, p, -1.0, NULL);
      }
  }

  gsl_rng_free(r);
} /* test_shaw() */
