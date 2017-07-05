/**
 *
 * @file testing_dmain.h
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @generated d Fri Apr  1 11:03:06 2016
 *
 **/
#ifndef TESTING_DMAIN_H
#define TESTING_DMAIN_H

#define USAGE(name, args, details)                       \
  printf(" Proper Usage is : ./dtesting ncores sched " name " " args " with\n" \
         "   - ncores : number of cores \n"              \
         "   - sched  : 0 for static, 1 for dynamic\n"   \
         "   - " name "  : name of function to test\n"   \
         details);

#ifdef WIN32
#include <float.h>
#define isnan _isnan
#endif

#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

extern int IONE;
extern int ISEED[4];

extern int format[6];
extern int trans[3];
extern int uplo[2];
extern int side[2];
extern int diag[2];
extern int itype[3];
extern int storev[2];
extern int norm[4];

extern char *formatstr[6];
extern char *transstr[3];
extern char *uplostr[2];
extern char *sidestr[2];
extern char *diagstr[2];
extern char *itypestr[3];
extern char *storevstr[2];
extern char *normstr[4];

extern void *formatmap[6];

int map_CM  (int m, int n, int mb, int nb, int i, int j);
int map_CCRB(int m, int n, int mb, int nb, int i, int j);
int map_CRRB(int m, int n, int mb, int nb, int i, int j);
int map_RCRB(int m, int n, int mb, int nb, int i, int j);
int map_RRRB(int m, int n, int mb, int nb, int i, int j);
int map_RM  (int m, int n, int mb, int nb, int i, int j);

int testing_dgemm(int argc, char **argv);
int testing_dsymm(int argc, char **argv);
int testing_dsymm(int argc, char **argv);
int testing_dsyrk(int argc, char **argv);
int testing_dsyrk(int argc, char **argv);
int testing_dsyr2k(int argc, char **argv);
int testing_dsyr2k(int argc, char **argv);
int testing_dtrmm(int argc, char **argv);
int testing_dtrsm(int argc, char **argv);
int testing_dpemv(int argc, char **argv);
int testing_dgeadd(int argc, char **argv);

int testing_dposv(int argc, char **argv);
int testing_dgels(int argc, char **argv);
int testing_dgesv(int argc, char **argv);
int testing_dgesv_incpiv(int argc, char **argv);

int testing_dpotri(int argc, char **argv);
int testing_dgetri(int argc, char **argv);

int testing_dgebrd(int argc, char **argv);
int testing_dgeev(int argc, char **argv);
int testing_dgesvd(int argc, char **argv);
int testing_dgesdd(int argc, char **argv);
int testing_dsytrd(int argc, char **argv);
int testing_dsyev(int argc, char **argv);
int testing_dsyevd(int argc, char **argv);
int testing_dsyevr(int argc, char **argv);
int testing_dsygv(int argc, char **argv);
int testing_dsygst(int argc, char **argv);

int testing_dgecfi(int argc, char **argv);
int testing_dgetmi(int argc, char **argv);

#ifdef DOUBLE
int testing_dsposv(int argc, char **argv);
int testing_dsgesv(int argc, char **argv);
int testing_dsungesv(int argc, char **argv);
#endif

int testing_dlange(int argc, char **argv);

#endif /* TESTINGS_H */
