/**
 *
 * @file testing_zmain.h
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @precisions normal z -> c d s
 *
 **/
#ifndef TESTING_ZMAIN_H
#define TESTING_ZMAIN_H

#define USAGE(name, args, details)                       \
  printf(" Proper Usage is : ./ztesting ncores sched " name " " args " with\n" \
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

int testing_zgemm(int argc, char **argv);
int testing_zhemm(int argc, char **argv);
int testing_zsymm(int argc, char **argv);
int testing_zherk(int argc, char **argv);
int testing_zsyrk(int argc, char **argv);
int testing_zher2k(int argc, char **argv);
int testing_zsyr2k(int argc, char **argv);
int testing_ztrmm(int argc, char **argv);
int testing_ztrsm(int argc, char **argv);
int testing_zpemv(int argc, char **argv);
int testing_zgeadd(int argc, char **argv);

int testing_zposv(int argc, char **argv);
int testing_zgels(int argc, char **argv);
int testing_zgesv(int argc, char **argv);
int testing_zgesv_incpiv(int argc, char **argv);

int testing_zpotri(int argc, char **argv);
int testing_zgetri(int argc, char **argv);

int testing_zgebrd(int argc, char **argv);
int testing_zgeev(int argc, char **argv);
int testing_zgesvd(int argc, char **argv);
int testing_zgesdd(int argc, char **argv);
int testing_zhetrd(int argc, char **argv);
int testing_zheev(int argc, char **argv);
int testing_zheevd(int argc, char **argv);
int testing_zheevr(int argc, char **argv);
int testing_zhegv(int argc, char **argv);
int testing_zhegst(int argc, char **argv);

int testing_zgecfi(int argc, char **argv);
int testing_zgetmi(int argc, char **argv);

#ifdef DOUBLE
int testing_zcposv(int argc, char **argv);
int testing_zcgesv(int argc, char **argv);
int testing_zcungesv(int argc, char **argv);
#endif

int testing_zlange(int argc, char **argv);

#endif /* TESTINGS_H */
