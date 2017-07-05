/**
 *
 * @file testing_smain.h
 *
 *  PLASMA testing routines
 *  PLASMA is a software package provided by Univ. of Tennessee,
 *  Univ. of California Berkeley and Univ. of Colorado Denver
 *
 * @version 2.8.0
 * @author Mathieu Faverge
 * @date 2010-11-15
 * @generated s Fri Apr  1 11:03:06 2016
 *
 **/
#ifndef TESTING_SMAIN_H
#define TESTING_SMAIN_H

#define USAGE(name, args, details)                       \
  printf(" Proper Usage is : ./stesting ncores sched " name " " args " with\n" \
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

int testing_sgemm(int argc, char **argv);
int testing_ssymm(int argc, char **argv);
int testing_ssymm(int argc, char **argv);
int testing_ssyrk(int argc, char **argv);
int testing_ssyrk(int argc, char **argv);
int testing_ssyr2k(int argc, char **argv);
int testing_ssyr2k(int argc, char **argv);
int testing_strmm(int argc, char **argv);
int testing_strsm(int argc, char **argv);
int testing_spemv(int argc, char **argv);
int testing_sgeadd(int argc, char **argv);

int testing_sposv(int argc, char **argv);
int testing_sgels(int argc, char **argv);
int testing_sgesv(int argc, char **argv);
int testing_sgesv_incpiv(int argc, char **argv);

int testing_spotri(int argc, char **argv);
int testing_sgetri(int argc, char **argv);

int testing_sgebrd(int argc, char **argv);
int testing_sgeev(int argc, char **argv);
int testing_sgesvd(int argc, char **argv);
int testing_sgesdd(int argc, char **argv);
int testing_ssytrd(int argc, char **argv);
int testing_ssyev(int argc, char **argv);
int testing_ssyevd(int argc, char **argv);
int testing_ssyevr(int argc, char **argv);
int testing_ssygv(int argc, char **argv);
int testing_ssygst(int argc, char **argv);

int testing_sgecfi(int argc, char **argv);
int testing_sgetmi(int argc, char **argv);

#ifdef DOUBLE
int testing_dsposv(int argc, char **argv);
int testing_dsgesv(int argc, char **argv);
int testing_dsungesv(int argc, char **argv);
#endif

int testing_slange(int argc, char **argv);

#endif /* TESTINGS_H */
