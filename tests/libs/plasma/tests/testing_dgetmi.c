/**
 *
 * @file testing_dgetmi.c
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
 *    Test the in place matrix transposition with the 6 different
 *    formats of input.
 **/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <lapacke.h>
#include <plasma.h>
#include "testing_dmain.h"

static int check_solution(int m, int n, int mb, int nb,
                   double *A, double *B,
                   int (*mapA)(int, int, int, int, int, int)) {
    int i, j;

    for( j=0; j<n; j++) {
        for (i=0; i<m; i++) {
            if (A[ mapA(m, n, mb, nb, i, j) ] != B[ mapA(n, m, mb, nb, j, i) ] ) {
                return -1;
            }
        }
    }
    return 0;
}

int testing_dgetmi(int argc, char **argv){

    double *A, *B;
    int m, n, mb, nb;
    int i, ret, size;

    /* Check for number of arguments*/
    if (argc != 4){
        USAGE("GETMI", "M N MB NB ntdbypb with \n",
              "   - M       : the number of rows of the matrix    \n"
              "   - N       : the number of columns of the matrix \n"
              "   - MB      : the number of rows of each block    \n"
              "   - NB      : the number of columns of each block \n");
        return -1;
    }

    m      = atoi(argv[0]);
    n      = atoi(argv[1]);
    mb     = atoi(argv[2]);
    nb     = atoi(argv[3]);

    size = m*n*sizeof(double);
    A = (double *)malloc(size);
    B = (double *)malloc(size);
    LAPACKE_dlarnv_work(1, ISEED, m*n, A);

    for(i=0; i<6; i++) {
        memcpy(B, A, size);

        printf(" - TESTING DGETMI (%4s) ...", formatstr[i]);
        ret = PLASMA_dgetmi( m, n, A, format[i], mb, nb );

        if (ret != PLASMA_SUCCESS) {
            printf("Failed\n");
            continue;
        }

        if ( check_solution(m, n, mb, nb, B, A, 
                            (int (*)(int, int, int, int, int, int))formatmap[i]) == 0 )
            printf("............ PASSED !\n");
        else
            printf("... FAILED !\n");
    }

    free( A ); free( B );

    return 0;
}
