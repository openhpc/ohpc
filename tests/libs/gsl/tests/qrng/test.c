/* Author: G. Jungman (+modifications from O. Teytaud olivier.teytaud@inria.fr)
 */
#include <config.h>
#include <stdlib.h>
#include <gsl/gsl_ieee_utils.h>

#include <gsl/gsl_qrng.h>
#include <gsl/gsl_test.h>
#include <math.h>

void test_sobol(void)
{
  int status = 0;
  double v[3];
  /* int i; */

  /* test in dimension 2 */
  gsl_qrng * g = gsl_qrng_alloc(gsl_qrng_sobol, 2);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.375 || v[1] != 0.375 );
  gsl_qrng_free(g);
  
  gsl_test (status, "Sobol d=2");

  status = 0;
  /* test in dimension 3 */
  g = gsl_qrng_alloc(gsl_qrng_sobol, 3);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.25 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.375 || v[1] != 0.375 || v[2] != 0.625 );

  gsl_test (status, "Sobol d=3");

  status = 0;
  gsl_qrng_init(g);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.25 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.375 || v[1] != 0.375 || v[2] != 0.625 );
  gsl_qrng_free(g);

  gsl_test (status, "Sobol d=3 (reinitialized)");
}

void test_halton(void)
{
	int status = 0;
	double v[1229];
	unsigned int i;

	/* test in dimension 1229 */

	gsl_qrng * g = gsl_qrng_alloc(gsl_qrng_halton, 1229);
	for (i=0;i<30;i++)
		gsl_qrng_get(g, v);
	gsl_qrng_free(g);

	gsl_test (status, "Halton d=1229");
	status = 0;

	/* test in dimension 2 */
	/*should be
	 * 0.5 0.333333
	 * 0.25 0.666667
	 * 0.75 0.111111
	 * 0.125 0.444444*/
	g = gsl_qrng_alloc(gsl_qrng_halton, 2);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 3.0/4.0, 1e-3, "halton(2) k=2 v[0]");
        gsl_test_rel (v[1], 1.0/9.0, 1e-3, "halton(2) k=2 v[1]");

	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 1.0/8.0, 1e-3, "halton(2) k=3 v[0]");
        gsl_test_rel (v[1], 4.0/9.0, 1e-3, "halton(2) k=3 v[1]");

	gsl_qrng_free(g);

	/* test in dimension 3 */
	g = gsl_qrng_alloc(gsl_qrng_halton, 3);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.75, 1e-3, "halton(3) k=3 v[0]");
        gsl_test_rel (v[1], 1.0/9.0, 1e-3, "halton(3) k=3 v[1]");
        gsl_test_rel (v[2], 0.6, 1e-3, "halton(3) k=3 v[2]");

	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.125, 1e-3, "halton(3) k=4 v[0]");
        gsl_test_rel (v[1], 4.0/9.0, 1e-3, "halton(3) k=4 v[1]");
        gsl_test_rel (v[2], 0.8, 1e-3, "halton(3) k=4 v[2]");

	gsl_qrng_init(g);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.75, 1e-3, "halton(3) reinitialized k=3 v[0]");
        gsl_test_rel (v[1], 1.0/9.0, 1e-3, "halton(3) reinitialized k=3 v[1]");
        gsl_test_rel (v[2], 0.6, 1e-3, "halton(3) reinitialized k=3 v[2]");

	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.125, 1e-3, "halton(3) reinitialized k=4 v[0]");
        gsl_test_rel (v[1], 4.0/9.0, 1e-3, "halton(3) reinitialized k=4 v[1]");
        gsl_test_rel (v[2], 0.8, 1e-3, "halton(3) reinitialized k=4 v[2]");

	gsl_qrng_free(g);
}

void test_reversehalton(void)
{
	int status = 0;
	double v[3];

	/* test in dimension 2 */
	gsl_qrng * g = gsl_qrng_alloc(gsl_qrng_reversehalton, 2);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
	/* should be
	 * 0.5 0.666667
	 * 0.25 0.333333
	 * 0.75 0.222222
	 * 0.125 0.888889*/

        gsl_test_rel (v[0], 3.0/4.0, 1e-3, "reversehalton(2) k=2 v[0]");
        gsl_test_rel (v[1], 2.0/9.0, 1e-3, "reversehalton(2) k=2 v[1]");

	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 1.0/8.0, 1e-3, "reversehalton(2) k=2 v[0]");
        gsl_test_rel (v[1], 8.0/9.0, 1e-3, "reversehalton(2) k=2 v[1]");

	gsl_qrng_free(g);


	/* test in dimension 3 */
	g = gsl_qrng_alloc(gsl_qrng_reversehalton, 3);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.75, 1e-3, "reversehalton(3) k=3 v[0]");
        gsl_test_rel (v[1], 2.0/9.0, 1e-3, "reversehalton(3) k=3 v[1]");
        gsl_test_rel (v[2], 0.4, 1e-3, "reversehalton(3) k=3 v[2]");

	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.125, 1e-3, "reversehalton(3) k=3 v[0]");
        gsl_test_rel (v[1], 8.0/9.0, 1e-3, "reversehalton(3) k=3 v[1]");
        gsl_test_rel (v[2], 0.2, 1e-3, "reversehalton(3) k=3 v[2]");

	status = 0;
	gsl_qrng_init(g);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.75, 1e-3, "reversehalton(3) reinitialized k=3 v[0]");
        gsl_test_rel (v[1], 2.0/9.0, 1e-3, "reversehalton(3) reinitialized k=3 v[1]");
        gsl_test_rel (v[2], 0.4, 1e-3, "reversehalton(3) reinitialized k=3 v[2]");

	gsl_qrng_get(g, v);
        gsl_test_rel (v[0], 0.125, 1e-3, "reversehalton(3) reinitialized k=3 v[0]");
        gsl_test_rel (v[1], 8.0/9.0, 1e-3, "reversehalton(3) reinitialized k=3 v[1]");
        gsl_test_rel (v[2], 0.2, 1e-3, "reversehalton(3) reinitialized k=3 v[2]");

	gsl_qrng_free(g);
}


void test_nied2(void)
{
  int status = 0;
  double v[3];
  /* int i; */

  /* test in dimension 2 */
  gsl_qrng * g = gsl_qrng_alloc(gsl_qrng_niederreiter_2, 2);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.75 || v[1] != 0.25 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 );
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.625 || v[1] != 0.125 );
  gsl_qrng_free(g);

  gsl_test (status, "Niederreiter d=2");

  status = 0;

  /* test in dimension 3 */
  g = gsl_qrng_alloc(gsl_qrng_niederreiter_2, 3);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.75 || v[1] != 0.25 || v[2] != 0.3125 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.5625 );
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.625 || v[1] != 0.125 || v[2] != 0.6875 );

  gsl_test (status, "Niederreiter d=3");

  status = 0;

  gsl_qrng_init(g);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.75 || v[1] != 0.25 || v[2] != 0.3125 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.5625 );
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.625 || v[1] != 0.125 || v[2] != 0.6875 );
  gsl_qrng_free(g);


  gsl_test (status, "Niederreiter d=3 (reinitialized)");
}


int main()
{

  gsl_ieee_env_setup ();

  test_sobol();
	test_halton();
	test_reversehalton();
  test_nied2();

  exit (gsl_test_summary ());
}
