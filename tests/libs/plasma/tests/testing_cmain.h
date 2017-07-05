/**
 *
 * @file testing_cmain.h
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @generated c Fri Apr  1 11:03:06 2016
 *
 **/
#ifndef TESTING_CMAIN_H
#define TESTING_CMAIN_H

#define USAGE(name, args, details)                       \
  printf(" Proper Usage is : ./ctesting ncores sched " name " " args " with\n" \
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

int testing_cgemm(int argc, char **argv);
int testing_chemm(int argc, char **argv);
int testing_csymm(int argc, char **argv);
int testing_cherk(int argc, char **argv);
int testing_csyrk(int argc, char **argv);
int testing_cher2k(int argc, char **argv);
int testing_csyr2k(int argc, char **argv);
int testing_ctrmm(int argc, char **argv);
int testing_ctrsm(int argc, char **argv);
int testing_cpemv(int argc, char **argv);
int testing_cgeadd(int argc, char **argv);

int testing_cposv(int argc, char **argv);
int testing_cgels(int argc, char **argv);
int testing_cgesv(int argc, char **argv);
int testing_cgesv_incpiv(int argc, char **argv);

int testing_cpotri(int argc, char **argv);
int testing_cgetri(int argc, char **argv);

int testing_cgebrd(int argc, char **argv);
int testing_cgeev(int argc, char **argv);
int testing_cgesvd(int argc, char **argv);
int testing_cgesdd(int argc, char **argv);
int testing_chetrd(int argc, char **argv);
int testing_cheev(int argc, char **argv);
int testing_cheevd(int argc, char **argv);
int testing_cheevr(int argc, char **argv);
int testing_chegv(int argc, char **argv);
int testing_chegst(int argc, char **argv);

int testing_cgecfi(int argc, char **argv);
int testing_cgetmi(int argc, char **argv);

#ifdef DOUBLE
int testing_ccposv(int argc, char **argv);
int testing_ccgesv(int argc, char **argv);
int testing_ccungesv(int argc, char **argv);
#endif

int testing_clange(int argc, char **argv);

#endif /* TESTINGS_H */
