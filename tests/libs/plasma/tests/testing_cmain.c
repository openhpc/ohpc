/**
 *
 * @file testing_cmain.c
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @generated c Fri Apr  1 11:03:04 2016
 *
 **/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <plasma.h>
#undef REAL
#define COMPLEX
#undef DOUBLE
#define SINGLE
#include "testing_cmain.h"

int   IONE     = 1;
int   ISEED[4] = {0,0,0,1};   /* initial seed for clarnv() */

int   format[6] = { PlasmaRM, PlasmaCM, PlasmaCCRB, PlasmaCRRB, PlasmaRCRB, PlasmaRRRB };
int   side[2]   = { PlasmaLeft,    PlasmaRight };
int   uplo[2]   = { PlasmaUpper,   PlasmaLower };
int   diag[2]   = { PlasmaNonUnit, PlasmaUnit  };
int   trans[3]  = { PlasmaNoTrans, PlasmaTrans, PlasmaConjTrans };
int   itype[3]  = { 1, 2, 3 };
int   storev[2] = { PlasmaRowwise, PlasmaColumnwise };
int   norm[4]   = { PlasmaMaxNorm, PlasmaOneNorm, PlasmaInfNorm, PlasmaFrobeniusNorm };


char *formatstr[6]= { "RM", "CM", "CCRB", "CRRB", "RCRB", "RRRB"};
char *sidestr[2]  = { "Left ", "Right" };
char *uplostr[2]  = { "Upper", "Lower" };
char *diagstr[2]  = { "NonUnit", "Unit   " };
char *transstr[3] = { "N", "T", "H" };
char *itypestr[3] = { "inv(U')xAxinv(U) or inv(L)xAxinv(L')", "UxAxU' or L'xAxL", "UxAxU' or L'xAxL" };
char *storevstr[2]= { "Rowwise", "Columnwise" };
char *normstr[4]  = { "Max", "One", "Inf", "Fro" };

#define map_cm(m, n, i, j)   ((i) + (j) * (m))
#define map_rm(m, n, i, j)   ((i) * (n) + (j))

int map_CM(  int m, int n, int mb, int nb, int i, int j) { return map_cm(m, n, i, j); }
int map_RM(  int m, int n, int mb, int nb, int i, int j) { return map_rm(m, n, i, j); }
int map_CCRB(int m, int n, int mb, int nb, int i, int j) {
    int m0 = m - m%mb;
    int n0 = n - n%nb;
    if ( j < n0 )
        if (i < m0)
            /* Case in A11 */
            return ( map_cm( m/mb, n/nb, i/mb, j/nb )*mb*nb + map_cm( mb,   nb,   i%mb, j%nb) );
        else
            /* Case in A21 */
            return ( m0*n0 + ( (j/nb) * (nb*(m%mb)) )       + map_cm( m%mb, nb,   i%mb, j%nb) );
    else
        if (i < m0)
            /* Case in A12 */
            return ( m*n0  + ( (i/mb) * (mb*(n%nb)) )       + map_cm( mb,   n%nb, i%mb, j%nb) );
        else
            /* Case in A22 */
            return ( m*n0  + (n-n0)*m0                      + map_cm( m%mb, n%nb, i%mb, j%nb) );
}

int map_CRRB(int m, int n, int mb, int nb, int i, int j) {
    int m0 = m - m%mb;
    int n0 = n - n%nb;
    if ( j < n0 )
        if (i < m0)
            /* Case in A11 */
            return ( map_cm( m/mb, n/nb, i/mb, j/nb )*mb*nb + map_rm( mb,   nb,   i%mb, j%nb) );
        else
            /* Case in A21 */
            return ( m0*n0 + ( (j/nb) * (nb*(m%mb)) )       + map_rm( m%mb, nb,   i%mb, j%nb) );
    else
        if (i < m0)
            /* Case in A12 */
            return ( m*n0  + ( (i/mb) * (mb*(n%nb)) )       + map_rm( mb,   n%nb, i%mb, j%nb) );
        else
            /* Case in A22 */
            return ( m*n0  + (n-n0)*m0                      + map_rm( m%mb, n%nb, i%mb, j%nb) );
}

int map_RCRB(int m, int n, int mb, int nb, int i, int j) {
    int m0 = m - m%mb;
    int n0 = n - n%nb;
    if ( j < n0 )
        if (i < m0)
            /* Case in A11 */
            return ( map_rm( m/mb, n/nb, i/mb, j/nb )*mb*nb + map_cm( mb,   nb,   i%mb, j%nb) );
        else
            /* Case in A21 */
            return ( m0*n  + ( (j/nb) * (nb*(m%mb)) )       + map_cm( m%mb, nb,   i%mb, j%nb) );
    else
        if (i < m0)
            /* Case in A12 */
            return ( m0*n0 + ( (i/mb) * (mb*(n%nb)) )       + map_cm( mb,   n%nb, i%mb, j%nb) );
        else
            /* Case in A22 */
            return ( m*n0  + (n-n0)*m0                      + map_cm( m%mb, n%nb, i%mb, j%nb) );
}

int map_RRRB(int m, int n, int mb, int nb, int i, int j) {
    int m0 = m - m%mb;
    int n0 = n - n%nb;
    if ( j < n0 )
        if (i < m0)
            /* Case in A11 */
            return ( map_rm( m/mb, n/nb, i/mb, j/nb )*mb*nb + map_rm( mb,   nb,   i%mb, j%nb) );
        else
            /* Case in A21 */
            return ( m0*n  + ( (j/nb) * (nb*(m%mb)) )       + map_rm( m%mb, nb,   i%mb, j%nb) );
    else
        if (i < m0)
            /* Case in A12 */
            return ( m0*n0 + ( (i/mb) * (mb*(n%nb)) )       + map_rm( mb,   n%nb, i%mb, j%nb) );
        else
            /* Case in A22 */
            return ( m*n0  + (n-n0)*m0                      + map_rm( m%mb, n%nb, i%mb, j%nb) );
}

void *formatmap[6] = {  map_RM, map_CM, map_CCRB, map_CRRB, map_RCRB, map_RRRB };

int main (int argc, char **argv)
{
    int ncores, sched;
    int info;
    char func[32];

    /* Check for number of arguments*/
    if ( argc < 4) {
        printf(" Proper Usage is : ./ctesting ncores sched FUNC ...\n"
               "   - ncores : number of cores \n"
               "   - sched  : 0 for static, 1 for dynamic\n"
               "   - FUNC   : name of function to test\n");
        exit(1);
    }

    sscanf( argv[1], "%d", &ncores );
    sscanf( argv[2], "%d", &sched  );
    sscanf( argv[3], "%s", func   );

    PLASMA_Init(ncores);
    if ( sched == 0 )
        PLASMA_Set( PLASMA_SCHEDULING_MODE, PLASMA_STATIC_SCHEDULING );
    else
        PLASMA_Set( PLASMA_SCHEDULING_MODE, PLASMA_DYNAMIC_SCHEDULING );

    argc -= 4;
    argv += 4;
    info  = 0;

    /*
     * Norms
     */
    if ( strcmp(func, "LANGE") == 0 ) {
        info = testing_clange( argc, argv );
    /*
     * Blas Level 3
     */
    } else if ( strcmp(func, "GEMM") == 0 ) {
        info = testing_cgemm( argc, argv );
#ifdef COMPLEX
    } else if ( strcmp(func, "HEMM") == 0 ) {
        info = testing_chemm( argc, argv );
    } else if ( strcmp(func, "HERK") == 0 ) {
        info = testing_cherk( argc, argv );
    } else if ( strcmp(func, "HER2K") == 0 ) {
      info = testing_cher2k( argc, argv );
#endif
    } else if ( strcmp(func, "SYMM") == 0 ) {
        info = testing_csymm( argc, argv );
    } else if ( strcmp(func, "SYRK") == 0 ) {
        info = testing_csyrk( argc, argv );
        } else if ( strcmp(func, "SYR2K") == 0 ) {
                info = testing_csyr2k( argc, argv );
    } else if ( strcmp(func, "TRMM") == 0 ) {
        info = testing_ctrmm( argc, argv );
    } else if ( strcmp(func, "TRSM") == 0 ) {
        info = testing_ctrsm( argc, argv );
    } else if ( strcmp(func, "PEMV") == 0 ) {
        info = testing_cpemv( argc, argv );
    } else if ( strcmp(func, "GEADD") == 0 ) {
        info = testing_cgeadd( argc, argv );
    /*
     * Linear system
     */
    } else if ( strcmp(func, "POSV") == 0 ) {
        info = testing_cposv( argc, argv );
    } else if ( strcmp(func, "GELS") == 0 ) {
        info = testing_cgels( argc, argv );
    } else if ( strcmp(func, "GESV_INCPIV") == 0 ) {
        info = testing_cgesv_incpiv( argc, argv );
    } else if ( strcmp(func, "GESV") == 0 ) {
        info = testing_cgesv( argc, argv );
    /*
     * Matrix inversion
     */
    } else if ( strcmp(func, "POTRI") == 0 ) {
        info = testing_cpotri( argc, argv );
    } else if ( strcmp(func, "GETRI") == 0 ) {
        info = testing_cgetri( argc, argv );
    /*
     * Eigenvalue Problems
     */
    } else if ( strcmp(func, "HETRD") == 0 ) {
      info = testing_chetrd( argc, argv );
    } else if ( strcmp(func, "HEEV") == 0 ) {
      info = testing_cheev( argc, argv );
    } else if ( strcmp(func, "HEEVD") == 0 ) {
      info = testing_cheevd( argc, argv );
    } else if ( strcmp(func, "HEEVR") == 0 ) {
      info = testing_cheevr( argc, argv );
    } else if ( strcmp(func, "HEGV") == 0 ) {
      info = testing_chegv( argc, argv );
    } else if ( strcmp(func, "HEGVD") == 0 ) {
      info = testing_chegv( argc, argv );
    } else if ( strcmp(func, "HEGST") == 0 ) {
      info = testing_chegst( argc, argv );
    /*
     * Singular Value Decomposition
     */
    } else if ( strcmp(func, "GEBRD") == 0 ) {
      info = testing_cgebrd( argc, argv );
    } else if ( strcmp(func, "GESVD") == 0 ) {
      info = testing_cgesvd( argc, argv );
    } else if ( strcmp(func, "GESDD") == 0 ) {
      info = testing_cgesdd( argc, argv );
#ifdef DOUBLE
    /*
     * Mixed precision
     */
    } else if ( strcmp(func, "CPOSV") == 0 ) {
        info = testing_ccposv( argc, argv );
    } else if ( strcmp(func, "CGESV") == 0 ) {
        info = testing_ccgesv( argc, argv );
    } else if ( strcmp(func, "CUNGESV") == 0 ) {
        info = testing_ccungesv( argc, argv );
#endif
    /* Layout Transformation */
    } else if ( strcmp(func, "GECFI") == 0 ) {
        info = testing_cgecfi( argc, argv );
    } else if ( strcmp(func, "GETMI") == 0 ) {
        info = testing_cgetmi( argc, argv );
    } else {
        fprintf(stderr, "Function unknown\n");
    }

    if ( info == -1 ) {
        printf( "TESTING %s FAILED : incorrect number of arguments\n", func);
    } else if ( info == -2 ) {
        printf( "TESTING %s FAILED : not enough memory\n", func);
    }

    PLASMA_Finalize();

    return EXIT_SUCCESS;
}
