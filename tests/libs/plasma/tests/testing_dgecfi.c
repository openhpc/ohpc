/**
 *
 * @file testing_dgecfi.c
 *
 *  PLASMA testings module
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 *  This program tests the implementation of the inplace format
 *  conversion based on the GKK algorithm by Gustavson, Karlsson,
 *  Kagstrom.
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 *
 * @generated d Fri Apr  1 11:03:06 2016
 *
 * Purpose :
 *    Test all the possibilities of matrix conversion (6*6)
 **/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <lapacke.h>
#include <plasma.h>
#include "testing_dmain.h"

static int conversions[36][2] = {
  /* No conversion */
  { PlasmaCM,   PlasmaCM   },
  { PlasmaCCRB, PlasmaCCRB },
  { PlasmaCRRB, PlasmaCRRB },
  { PlasmaRCRB, PlasmaRCRB },
  { PlasmaRRRB, PlasmaRRRB },
  { PlasmaRM,   PlasmaRM   },
  /* One Conversion */
  { PlasmaCM,   PlasmaCCRB },
  { PlasmaCCRB, PlasmaCM   },
  { PlasmaRM,   PlasmaRRRB },
  { PlasmaRRRB, PlasmaRM   },
  { PlasmaCCRB, PlasmaCRRB },
  { PlasmaCRRB, PlasmaCCRB },
  { PlasmaRCRB, PlasmaRRRB },
  { PlasmaRRRB, PlasmaRCRB },
  { PlasmaCCRB, PlasmaRCRB },
  { PlasmaRCRB, PlasmaCCRB },
  { PlasmaCRRB, PlasmaRRRB },
  { PlasmaRRRB, PlasmaCRRB },
  /* Two Conversions */
  { PlasmaRM,   PlasmaCRRB },
  { PlasmaCRRB, PlasmaRM   },
  { PlasmaCM,   PlasmaRCRB }, 
  { PlasmaRCRB, PlasmaCM   },
  { PlasmaCCRB, PlasmaRRRB },
  { PlasmaRRRB, PlasmaCCRB },
  { PlasmaCRRB, PlasmaRCRB },
  { PlasmaRCRB, PlasmaCRRB },
  { PlasmaCM,   PlasmaCRRB }, 
  { PlasmaCRRB, PlasmaCM   },
  { PlasmaRCRB, PlasmaRM   },
  { PlasmaRM,   PlasmaRCRB },
  /* Three Conversions */
  { PlasmaCM,   PlasmaRRRB },
  { PlasmaRRRB, PlasmaCM   },
  { PlasmaCCRB, PlasmaRM   },
  { PlasmaRM,   PlasmaCCRB },
  /* Three Conversions */
  { PlasmaCM,   PlasmaRM   },
  { PlasmaRM,   PlasmaCM   },
};

static int check_solution(int m, int n, int mba, int nba, int mbb, int nbb,
                          double *A, double *B, 
                          int (*mapA)(int, int, int, int, int, int), int (*mapB)(int, int, int, int, int, int)) {
    int i, j;

    for( j=0; j<n; j++) {
      for (i=0; i<m; i++) {
            if (A[ mapA(m, n, mba, nba, i, j) ] != B[ mapB(m, n, mbb, nbb, i, j) ] ) {
                return -1;
            }
        }
    }
    return 0;
}

int testing_dgecfi(int argc, char **argv){

    double *A, *B;
    int m, n, mb, nb, mb2, nb2;
    int i, ret, size;
    int f1, f2;

    /* Check for number of arguments*/
    if (argc != 6){
        USAGE("GECFI", "M N MB NB with \n",
              "   - M       : the number of rows of the matrix    \n"
              "   - N       : the number of columns of the matrix \n"
              "   - MB      : the number of rows of each block    \n"
              "   - NB      : the number of columns of each block \n"
              "   - MB2     : the number of rows of each block    \n"
              "   - NB2     : the number of columns of each block \n");
        return -1;
    }

    m   = atoi(argv[0]);
    n   = atoi(argv[1]);
    mb  = atoi(argv[2]);
    nb  = atoi(argv[3]);
    mb2 = atoi(argv[4]);
    nb2 = atoi(argv[5]);

    /* Initialize Plasma */
    size = m*n*sizeof(double);
    A = (double *)malloc(size);
    B = (double *)malloc(size);
    LAPACKE_dlarnv_work(1, ISEED, m*n, A);

    for(i=0; i<36; i++) {
        memcpy(B, A, size);

        f1 = conversions[i][0]-PlasmaRM;
        f2 = conversions[i][1]-PlasmaRM;
        
        printf(" - TESTING DGECFI (%4s => %4s) ...", formatstr[f1], formatstr[f2] );

        ret = PLASMA_dgecfi(m, n, B, conversions[i][0], mb, nb, conversions[i][1], mb2, nb2);

        if (ret != PLASMA_SUCCESS) {
            printf("Failed\n");
            continue;
        }
         
        if ( check_solution(m, n, mb, nb, mb2, nb2, A, B, 
                            (int (*)(int, int, int, int, int, int))formatmap[f1], 
                            (int (*)(int, int, int, int, int, int))formatmap[f2] ) == 0 )
            printf("............ PASSED !\n");
        else
            printf("... FAILED !\n");

#if 0
            {
              char cmd[256];
    
              PLASMA_Finalize();
              sprintf(cmd, "mv $PWD/dot_dag_file.dot $PWD/dgecfi_%s_%s.dot", formatstr[f1], formatstr[f2]);
              system(cmd);
              
              PLASMA_Init(0);
              PLASMA_Set( PLASMA_SCHEDULING_MODE, PLASMA_DYNAMIC_SCHEDULING );
            }
#endif
    }

    free( A ); free( B );

    return 0;
}
