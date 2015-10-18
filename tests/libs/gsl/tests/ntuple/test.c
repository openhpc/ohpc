#include <config.h>
#include <stdlib.h>
#include <string.h>
#include <gsl/gsl_ntuple.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_ieee_utils.h>

struct data
{
  int num;
  double x;
  double y;
  double z;
};
int sel_func (void *ntuple_data, void * params);
double val_func (void *ntuple_data, void * params);

int
main (void)
{
  struct data ntuple_row;
  int i;

  double x[1000], y[1000], z[1000], f[100];

  gsl_ntuple_select_fn S;
  gsl_ntuple_value_fn V;
  
  double scale = 1.5;
  
  gsl_ieee_env_setup ();

  /* zero struct including padding bytes to avoid valgrind errors */
  memset(&ntuple_row, 0, sizeof(struct data));

  S.function = &sel_func;
  S.params = &scale;
  
  V.function = &val_func;
  V.params = &scale;

  {
    gsl_ntuple *ntuple = gsl_ntuple_create ("test.dat", &ntuple_row, 
                                            sizeof (ntuple_row));

    int status = 0;

    for (i = 0; i < 100; i++) f[i] = 0;
    
    for (i = 0; i < 1000; i++)
      {
        double xi = 1.0 / (i + 1.5);
        double yi = xi * xi ;
        double zi = xi * xi * xi;
        
        ntuple_row.x = xi;
        ntuple_row.y = yi;
        ntuple_row.z = zi;
        ntuple_row.num = i;
        
        x[i] = xi; y[i] = yi; z[i] = zi;
        
        if (xi * scale < 0.1)
          {
            double v = xi + yi + zi;
            int k = (int)(100.0*v*scale);
            f[k]++;
          }

        /* printf ("x,y,z = %f,%f,%f; n=%x \n", ntuple_row.x,
           ntuple_row.y, ntuple_row.z, ntuple_row.num); */
        
        {
          int s = gsl_ntuple_bookdata (ntuple);

          if (s != GSL_SUCCESS)
            {
              status = 1;
            }
        }
      }
    
    gsl_ntuple_close (ntuple);

    gsl_test (status, "writing ntuples");
  }

  {
    gsl_ntuple *ntuple = gsl_ntuple_open ("test.dat", &ntuple_row, 
                                          sizeof (ntuple_row));
    int status = 0;

    for (i = 0; i < 1000; i++)
      {
        gsl_ntuple_read (ntuple);

        status = (ntuple_row.num != i);
        status |= (ntuple_row.x != x[i]);
        status |= (ntuple_row.y != y[i]);
        status |= (ntuple_row.z != z[i]);

        /* printf ("x,y,z = %f,%f,%f; n=%d\n", ntuple_row.x,
                ntuple_row.y, ntuple_row.z, ntuple_row.num); */
      }
    gsl_ntuple_close (ntuple);

    gsl_test (status, "reading ntuples");
  }    

  {
    int status = 0;

    gsl_ntuple *ntuple = gsl_ntuple_open ("test.dat", &ntuple_row, 
                                          sizeof (ntuple_row));

    gsl_histogram *h = gsl_histogram_calloc_uniform (100, 0., 1.);

    gsl_ntuple_project (h, ntuple, &V, &S);

    gsl_ntuple_close (ntuple);

    /* gsl_histogram_fprintf (stdout, h, "%f", "%f"); */

    for (i = 0; i < 100; i++)
      {
        /* printf ("h  %g f  %g\n", h->bin[i], f[i]); */

        if (h->bin[i] != f[i])
          {
            status = 1;
          }
      }

    gsl_test (status, "histogramming ntuples");

    gsl_histogram_free (h);
  }

  exit (gsl_test_summary());
}

int
sel_func (void *ntuple_data, void * params)
{
  double x, y, z, scale;
  scale = *(double *)params;

  x = ((struct data *) ntuple_data)->x;
  y = ((struct data *) ntuple_data)->y;
  z = ((struct data *) ntuple_data)->z;

  return (x*scale < 0.1);
}

double
val_func (void *ntuple_data, void * params)
{
  double x, y, z, scale;
  scale = *(double *)params;

  x = ((struct data *) ntuple_data)->x;
  y = ((struct data *) ntuple_data)->y;
  z = ((struct data *) ntuple_data)->z;

  return (x + y + z) * scale;
}
