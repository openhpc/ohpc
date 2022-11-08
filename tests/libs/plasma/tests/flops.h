/**
 *
 * @file
 *
 *  PLASMA is a software package provided by:
 *  University of Tennessee, US,
 *  University of Manchester, UK.
 *
 **/
#ifndef ICL_PLASMA_FLOPS_H
#define ICL_PLASMA_FLOPS_H

#include "plasma_types.h"

#ifdef __cplusplus
extern "C" {
#endif

//==============================================================================
// Generic formulas come from LAWN 41
// BLAS formulas generally assume alpha == 1 or -1, and beta == 1, -1, or 0;
// otherwise add some smaller order term.
// Some formulas are wrong when m, n, or k == 0; flops should be 0
// (e.g., syr2k, unmqr).
// Formulas may give negative results for invalid combinations of m, n, k
// (e.g., ungqr, unmqr).

//==============================================================================
// Level 2 BLAS
//==============================================================================

//------------------------------------------------------------ gemv
static double fmuls_gemv(double m, double n)
    { return m*n; }

static double fadds_gemv(double m, double n)
    { return m*n; }

static double  flops_zgemv(double m, double n)
    { return 6.*fmuls_gemv(m, n) + 2.*fadds_gemv(m, n); }

static double  flops_cgemv(double m, double n)
    { return 6.*fmuls_gemv(m, n) + 2.*fadds_gemv(m, n); }

static double  flops_dgemv(double m, double n)
    { return    fmuls_gemv(m, n) +    fadds_gemv(m, n); }

static double  flops_sgemv(double m, double n)
    { return    fmuls_gemv(m, n) +    fadds_gemv(m, n); }

//------------------------------------------------------------ symv/hemv
static double fmuls_symv(double n)
    { return fmuls_gemv(n, n); }

static double fadds_symv(double n)
    { return fadds_gemv(n, n); }

static double fmuls_hemv(double n)
    { return fmuls_symv(n); }

static double fadds_hemv(double n)
    { return fadds_symv(n); }

static double  flops_zhemv(double n)
    { return 6.*fmuls_hemv(n) + 2.*fadds_hemv(n); }

static double  flops_chemv(double n)
    { return 6.*fmuls_hemv(n) + 2.*fadds_hemv(n); }

static double  flops_zsymv(double n)
    { return 6.*fmuls_symv(n) + 2.*fadds_symv(n); }

static double  flops_csymv(double n)
    { return 6.*fmuls_symv(n) + 2.*fadds_symv(n); }

static double  flops_dsymv(double n)
    { return    fmuls_symv(n) +    fadds_symv(n); }

static double  flops_ssymv(double n)
    { return    fmuls_symv(n) +    fadds_symv(n); }

//==============================================================================
// Level 3 BLAS
//==============================================================================

//------------------------------------------------------------ gemm
static double fmuls_gemm(double m, double n, double k)
    { return m*n*k; }

static double fadds_gemm(double m, double n, double k)
    { return m*n*k; }

static double  flops_zgemm(double m, double n, double k)
    { return 6.*fmuls_gemm(m, n, k) + 2.*fadds_gemm(m, n, k); }

static double  flops_cgemm(double m, double n, double k)
    { return 6.*fmuls_gemm(m, n, k) + 2.*fadds_gemm(m, n, k); }

static double  flops_dgemm(double m, double n, double k)
    { return    fmuls_gemm(m, n, k) +    fadds_gemm(m, n, k); }

static double  flops_sgemm(double m, double n, double k)
    { return    fmuls_gemm(m, n, k) +    fadds_gemm(m, n, k); }

//------------------------------------------------------------ symm/hemm
static double fmuls_symm(plasma_enum_t side, double m, double n)
{
    return (side == PlasmaLeft)
        ? fmuls_gemm(m, m, n)
        : fmuls_gemm(m, n, n);
}

static double fadds_symm(plasma_enum_t side, double m, double n)
{
    return (side == PlasmaLeft)
        ? fadds_gemm(m, m, n)
        : fadds_gemm(m, n, n);
}

static double fmuls_hemm(plasma_enum_t side, double m, double n)
    { return fmuls_symm(side, m, n); }

static double fadds_hemm(plasma_enum_t side, double m, double n)
    { return fadds_symm(side, m, n); }

static double  flops_zhemm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_hemm(side, m, n) + 2.*fadds_hemm(side, m, n); }

static double  flops_chemm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_hemm(side, m, n) + 2.*fadds_hemm(side, m, n); }

static double  flops_zsymm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_symm(side, m, n) + 2.*fadds_symm(side, m, n); }

static double  flops_csymm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_symm(side, m, n) + 2.*fadds_symm(side, m, n); }

static double  flops_dsymm(plasma_enum_t side, double m, double n)
    { return    fmuls_symm(side, m, n) +    fadds_symm(side, m, n); }

static double  flops_ssymm(plasma_enum_t side, double m, double n)
    { return    fmuls_symm(side, m, n) +    fadds_symm(side, m, n); }

//------------------------------------------------------------ syrk/herk
static double fmuls_syrk(double n, double k)
    { return 0.5*k*n*(n + 1); }

static double fadds_syrk(double n, double k)
    { return 0.5*k*n*(n + 1); }

static double fmuls_herk(double n, double k)
    { return fmuls_syrk(n, k); }

static double fadds_herk(double n, double k)
    { return fadds_syrk(n, k); }

static double  flops_zherk(double n, double k)
    { return 6.*fmuls_herk(n, k) + 2.*fadds_herk(n, k); }

static double  flops_cherk(double n, double k)
    { return 6.*fmuls_herk(n, k) + 2.*fadds_herk(n, k); }

static double  flops_zsyrk(double n, double k)
    { return 6.*fmuls_syrk(n, k) + 2.*fadds_syrk(n, k); }

static double  flops_csyrk(double n, double k)
    { return 6.*fmuls_syrk(n, k) + 2.*fadds_syrk(n, k); }

static double  flops_dsyrk(double n, double k)
    { return    fmuls_syrk(n, k) +    fadds_syrk(n, k); }

static double  flops_ssyrk(double n, double k)
    { return    fmuls_syrk(n, k) +    fadds_syrk(n, k); }

//------------------------------------------------------------ syr2k/her2k
static double fmuls_syr2k(double n, double k)
    { return k*n*n; }

static double fadds_syr2k(double n, double k)
    { return k*n*n + n; }

static double fmuls_her2k(double n, double k)
    { return fmuls_syr2k(n, k); }

static double fadds_her2k(double n, double k)
    { return fadds_syr2k(n, k); }

static double  flops_zher2k(double n, double k)
    { return 6.*fmuls_her2k(n, k) + 2.*fadds_her2k(n, k); }

static double  flops_cher2k(double n, double k)
    { return 6.*fmuls_her2k(n, k) + 2.*fadds_her2k(n, k); }

static double  flops_zsyr2k(double n, double k)
    { return 6.*fmuls_syr2k(n, k) + 2.*fadds_syr2k(n, k); }

static double  flops_csyr2k(double n, double k)
    { return 6.*fmuls_syr2k(n, k) + 2.*fadds_syr2k(n, k); }

static double  flops_dsyr2k(double n, double k)
    { return    fmuls_syr2k(n, k) +    fadds_syr2k(n, k); }

static double  flops_ssyr2k(double n, double k)
    { return    fmuls_syr2k(n, k) +    fadds_syr2k(n, k); }

//------------------------------------------------------------ trmm
static double fmuls_trmm_2(double m, double n)
    { return 0.5*n*m*(m + 1); }

static double fadds_trmm_2(double m, double n)
    { return 0.5*n*m*(m - 1); }

static double fmuls_trmm(plasma_enum_t side, double m, double n)
{
    return (side == PlasmaLeft)
        ? fmuls_trmm_2(m, n)
        : fmuls_trmm_2(n, m);
}

static double fadds_trmm(plasma_enum_t side, double m, double n)
{
    return (side == PlasmaLeft)
        ? fadds_trmm_2(m, n)
        : fadds_trmm_2(n, m);
}

static double  flops_ztrmm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_trmm(side, m, n) + 2.*fadds_trmm(side, m, n); }

static double  flops_ctrmm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_trmm(side, m, n) + 2.*fadds_trmm(side, m, n); }

static double  flops_dtrmm(plasma_enum_t side, double m, double n)
    { return    fmuls_trmm(side, m, n) +    fadds_trmm(side, m, n); }

static double  flops_strmm(plasma_enum_t side, double m, double n)
    { return    fmuls_trmm(side, m, n) +    fadds_trmm(side, m, n); }

//------------------------------------------------------------ trsm
static double fmuls_trsm(plasma_enum_t side, double m, double n)
    { return fmuls_trmm(side, m, n); }

static double fadds_trsm(plasma_enum_t side, double m, double n)
    { return fadds_trmm(side, m, n); }

static double  flops_ztrsm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_trsm(side, m, n) + 2.*fadds_trsm(side, m, n); }

static double  flops_ctrsm(plasma_enum_t side, double m, double n)
    { return 6.*fmuls_trsm(side, m, n) + 2.*fadds_trsm(side, m, n); }

static double  flops_dtrsm(plasma_enum_t side, double m, double n)
    { return    fmuls_trsm(side, m, n) +    fadds_trsm(side, m, n); }

static double  flops_strsm(plasma_enum_t side, double m, double n)
    { return    fmuls_trsm(side, m, n) +    fadds_trsm(side, m, n); }

//==============================================================================
// LAPACK
//==============================================================================

//------------------------------------------------------------ getrf
// LAWN 41 omits (m < n) case
static double fmuls_getrf(double m, double n)
{
    return (m >= n)
        ? (0.5*m*n*n - 1./6.*n*n*n + 0.5*m*n - 0.5*n*n + 2./3.*n)
        : (0.5*n*m*m - 1./6.*m*m*m + 0.5*n*m - 0.5*m*m + 2./3.*m);
}

static double fadds_getrf(double m, double n)
{
    return (m >= n)
        ? (0.5*m*n*n - 1./6.*n*n*n - 0.5*m*n + 1./6.*n)
        : (0.5*n*m*m - 1./6.*m*m*m - 0.5*n*m + 1./6.*m);
}

static double  flops_zgetrf(double m, double n)
    { return 6.*fmuls_getrf(m, n) + 2.*fadds_getrf(m, n); }

static double  flops_cgetrf(double m, double n)
    { return 6.*fmuls_getrf(m, n) + 2.*fadds_getrf(m, n); }

static double  flops_dgetrf(double m, double n)
    { return    fmuls_getrf(m, n) +    fadds_getrf(m, n); }

static double  flops_sgetrf(double m, double n)
    { return    fmuls_getrf(m, n) +    fadds_getrf(m, n); }

//------------------------------------------------------------ getri
static double fmuls_getri(double n)
    { return 2./3.*n*n*n + 0.5*n*n + 5./6.*n; }

static double fadds_getri(double n)
    { return 2./3.*n*n*n - 1.5*n*n + 5./6.*n; }

static double  flops_zgetri(double n)
    { return 6.*fmuls_getri(n) + 2.*fadds_getri(n); }

static double  flops_cgetri(double n)
    { return 6.*fmuls_getri(n) + 2.*fadds_getri(n); }

static double  flops_dgetri(double n)
    { return    fmuls_getri(n) +    fadds_getri(n); }

static double  flops_sgetri(double n)
    { return    fmuls_getri(n) +    fadds_getri(n); }

//------------------------------------------------------------ getrs
static double fmuls_getrs(double n, double nrhs)
    { return nrhs*n*n; }

static double fadds_getrs(double n, double nrhs)
    { return nrhs*n*(n - 1); }

static double  flops_zgetrs(double n, double nrhs)
    { return 6.*fmuls_getrs(n, nrhs) + 2.*fadds_getrs(n, nrhs); }

static double  flops_cgetrs(double n, double nrhs)
    { return 6.*fmuls_getrs(n, nrhs) + 2.*fadds_getrs(n, nrhs); }

static double  flops_dgetrs(double n, double nrhs)
    { return    fmuls_getrs(n, nrhs) +    fadds_getrs(n, nrhs); }

static double  flops_sgetrs(double n, double nrhs)
    { return    fmuls_getrs(n, nrhs) +    fadds_getrs(n, nrhs); }

//------------------------------------------------------------ potrf
static double fmuls_potrf(double n)
    { return 1./6.*n*n*n + 0.5*n*n + 1./3.*n; }

static double fadds_potrf(double n)
    { return 1./6.*n*n*n - 1./6.*n; }

static double  flops_zpotrf(double n)
    { return 6.*fmuls_potrf(n) + 2.*fadds_potrf(n); }

static double  flops_cpotrf(double n)
    { return 6.*fmuls_potrf(n) + 2.*fadds_potrf(n); }

static double  flops_dpotrf(double n)
    { return    fmuls_potrf(n) +    fadds_potrf(n); }

static double  flops_spotrf(double n)
    { return    fmuls_potrf(n) +    fadds_potrf(n); }

//------------------------------------------------------------ potri
static double fmuls_potri(double n)
    { return 1./3.*n*n*n + n*n + 2./3.*n; }

static double fadds_potri(double n)
    { return 1./3.*n*n*n - 0.5*n*n + 1./6.*n; }

static double  flops_zpotri(double n)
    { return 6.*fmuls_potri(n) + 2.*fadds_potri(n); }

static double  flops_cpotri(double n)
    { return 6.*fmuls_potri(n) + 2.*fadds_potri(n); }

static double  flops_dpotri(double n)
    { return    fmuls_potri(n) +    fadds_potri(n); }

static double  flops_spotri(double n)
    { return    fmuls_potri(n) +    fadds_potri(n); }

//------------------------------------------------------------ potrs
static double fmuls_potrs(double n, double nrhs)
    { return nrhs*n*(n + 1); }

static double fadds_potrs(double n, double nrhs)
    { return nrhs*n*(n - 1); }

static double  flops_zpotrs(double n, double nrhs)
    { return 6.*fmuls_potrs(n, nrhs) + 2.*fadds_potrs(n, nrhs); }

static double  flops_cpotrs(double n, double nrhs)
    { return 6.*fmuls_potrs(n, nrhs) + 2.*fadds_potrs(n, nrhs); }

static double  flops_dpotrs(double n, double nrhs)
    { return    fmuls_potrs(n, nrhs) +    fadds_potrs(n, nrhs); }

static double  flops_spotrs(double n, double nrhs)
    { return    fmuls_potrs(n, nrhs) +    fadds_potrs(n, nrhs); }

//------------------------------------------------------------ geqrf
static double fmuls_geqrf(double m, double n)
{
    return (m > n)
        ? (m*n*n - 1./3.*n*n*n +   m*n + 0.5*n*n + 23./6.*n)
        : (n*m*m - 1./3.*m*m*m + 2*n*m - 0.5*m*m + 23./6.*m);
}

static double fadds_geqrf(double m, double n)
{
    return (m > n)
        ? (m*n*n - 1./3.*n*n*n + 0.5*n*n       + 5./6.*n)
        : (n*m*m - 1./3.*m*m*m + n*m - 0.5*m*m + 5./6.*m);
}

static double  flops_zgeqrf(double m, double n)
    { return 6.*fmuls_geqrf(m, n) + 2.*fadds_geqrf(m, n); }

static double  flops_cgeqrf(double m, double n)
    { return 6.*fmuls_geqrf(m, n) + 2.*fadds_geqrf(m, n); }

static double  flops_dgeqrf(double m, double n)
    { return    fmuls_geqrf(m, n) +    fadds_geqrf(m, n); }

static double  flops_sgeqrf(double m, double n)
    { return    fmuls_geqrf(m, n) +    fadds_geqrf(m, n); }

//------------------------------------------------------------ geqrt
static double fmuls_geqrt(double m, double n)
    { return 0.5*m*n; }

static double fadds_geqrt(double m, double n)
    { return 0.5*m*n; }

static double  flops_zgeqrt(double m, double n)
    { return 6.*fmuls_geqrt(m, n) + 2.*fadds_geqrt(m, n); }

static double  flops_cgeqrt(double m, double n)
    { return 6.*fmuls_geqrt(m, n) + 2.*fadds_geqrt(m, n); }

static double  flops_dgeqrt(double m, double n)
    { return    fmuls_geqrt(m, n) +    fadds_geqrt(m, n); }

static double  flops_sgeqrt(double m, double n)
    { return    fmuls_geqrt(m, n) +    fadds_geqrt(m, n); }

//------------------------------------------------------------ geqlf
static double fmuls_geqlf(double m, double n)
    { return fmuls_geqrf(m, n); }

static double fadds_geqlf(double m, double n)
    { return fadds_geqrf(m, n); }

static double  flops_zgeqlf(double m, double n)
    { return 6.*fmuls_geqlf(m, n) + 2.*fadds_geqlf(m, n); }

static double  flops_cgeqlf(double m, double n)
    { return 6.*fmuls_geqlf(m, n) + 2.*fadds_geqlf(m, n); }

static double  flops_dgeqlf(double m, double n)
    { return    fmuls_geqlf(m, n) +    fadds_geqlf(m, n); }

static double  flops_sgeqlf(double m, double n)
    { return    fmuls_geqlf(m, n) +    fadds_geqlf(m, n); }

//------------------------------------------------------------ gerqf
static double fmuls_gerqf(double m, double n)
{
    return (m > n)
        ? (m*n*n - 1./3.*n*n*n +   m*n + 0.5*n*n + 29./6.*n)
        : (n*m*m - 1./3.*m*m*m + 2*n*m - 0.5*m*m + 29./6.*m);
}

static double fadds_gerqf(double m, double n)
{
    return (m > n)
        ? (m*n*n - 1./3.*n*n*n + m*n - 0.5*n*n + 5./6.*n)
        : (n*m*m - 1./3.*m*m*m + 0.5*m*m       + 5./6.*m);
}

static double  flops_zgerqf(double m, double n)
    { return 6.*fmuls_gerqf(m, n) + 2.*fadds_gerqf(m, n); }

static double  flops_cgerqf(double m, double n)
    { return 6.*fmuls_gerqf(m, n) + 2.*fadds_gerqf(m, n); }

static double  flops_dgerqf(double m, double n)
    { return    fmuls_gerqf(m, n) +    fadds_gerqf(m, n); }

static double  flops_sgerqf(double m, double n)
    { return    fmuls_gerqf(m, n) +    fadds_gerqf(m, n); }

//------------------------------------------------------------ gelqf
static double fmuls_gelqf(double m, double n)
    { return  fmuls_gerqf(m, n); }

static double fadds_gelqf(double m, double n)
    { return  fadds_gerqf(m, n); }

static double  flops_zgelqf(double m, double n)
    { return 6.*fmuls_gelqf(m, n) + 2.*fadds_gelqf(m, n); }

static double  flops_cgelqf(double m, double n)
    { return 6.*fmuls_gelqf(m, n) + 2.*fadds_gelqf(m, n); }

static double  flops_dgelqf(double m, double n)
    { return    fmuls_gelqf(m, n) +    fadds_gelqf(m, n); }

static double  flops_sgelqf(double m, double n)
    { return    fmuls_gelqf(m, n) +    fadds_gelqf(m, n); }

//------------------------------------------------------------ ungqr
static double fmuls_ungqr(double m, double n, double k)
    { return 2.*m*n*k - (m + n)*k*k + 2./3.*k*k*k + 2.*n*k - k*k - 5./3.*k; }

static double fadds_ungqr(double m, double n, double k)
    { return 2.*m*n*k - (m + n)*k*k + 2./3.*k*k*k + n*k - m*k + 1./3.*k; }

static double  flops_zungqr(double m, double n, double k)
    { return 6.*fmuls_ungqr(m, n, k) + 2.*fadds_ungqr(m, n, k); }

static double  flops_cungqr(double m, double n, double k)
    { return 6.*fmuls_ungqr(m, n, k) + 2.*fadds_ungqr(m, n, k); }

static double  flops_dorgqr(double m, double n, double k)
    { return    fmuls_ungqr(m, n, k) +    fadds_ungqr(m, n, k); }

static double  flops_sorgqr(double m, double n, double k)
    { return    fmuls_ungqr(m, n, k) +    fadds_ungqr(m, n, k); }

//------------------------------------------------------------ ungql
static double fmuls_ungql(double m, double n, double k)
    { return  fmuls_ungqr(m, n, k); }

static double fadds_ungql(double m, double n, double k)
    { return fadds_ungqr(m, n, k); }

static double  flops_zungql(double m, double n, double k)
    { return 6.*fmuls_ungql(m, n, k) + 2.*fadds_ungql(m, n, k); }

static double  flops_cungql(double m, double n, double k)
    { return 6.*fmuls_ungql(m, n, k) + 2.*fadds_ungql(m, n, k); }

static double  flops_dorgql(double m, double n, double k)
    { return    fmuls_ungql(m, n, k) +    fadds_ungql(m, n, k); }

static double  flops_sorgql(double m, double n, double k)
    { return    fmuls_ungql(m, n, k) +    fadds_ungql(m, n, k); }

//------------------------------------------------------------ ungrq
static double fmuls_ungrq(double m, double n, double k)
    { return 2.*m*n*k - (m + n)*k*k + 2./3.*k*k*k + m*k + n*k - k*k - 2./3.*k; }

static double fadds_ungrq(double m, double n, double k)
    { return 2.*m*n*k - (m + n)*k*k + 2./3.*k*k*k + m*k - n*k + 1./3.*k; }

static double  flops_zungrq(double m, double n, double k)
    { return 6.*fmuls_ungrq(m, n, k) + 2.*fadds_ungrq(m, n, k); }

static double  flops_cungrq(double m, double n, double k)
    { return 6.*fmuls_ungrq(m, n, k) + 2.*fadds_ungrq(m, n, k); }

static double  flops_dorgrq(double m, double n, double k)
    { return    fmuls_ungrq(m, n, k) +    fadds_ungrq(m, n, k); }

static double  flops_sorgrq(double m, double n, double k)
    { return    fmuls_ungrq(m, n, k) +    fadds_ungrq(m, n, k); }

//------------------------------------------------------------ unglq
static double fmuls_unglq(double m, double n, double k)
    { return fmuls_ungrq(m, n, k); }

static double fadds_unglq(double m, double n, double k)
    { return fadds_ungrq(m, n, k); }

static double  flops_zunglq(double m, double n, double k)
    { return 6.*fmuls_unglq(m, n, k) + 2.*fadds_unglq(m, n, k); }

static double  flops_cunglq(double m, double n, double k)
    { return 6.*fmuls_unglq(m, n, k) + 2.*fadds_unglq(m, n, k); }

static double  flops_dorglq(double m, double n, double k)
    { return    fmuls_unglq(m, n, k) +    fadds_unglq(m, n, k); }

static double  flops_sorglq(double m, double n, double k)
    { return    fmuls_unglq(m, n, k) +    fadds_unglq(m, n, k); }

//------------------------------------------------------------ geqrs
static double fmuls_geqrs(double m, double n, double nrhs)
    { return nrhs*(2.*m*n - 0.5*n*n + 2.5*n); }

static double fadds_geqrs(double m, double n, double nrhs)
    { return nrhs*(2.*m*n - 0.5*n*n + 0.5*n); }

static double  flops_zgeqrs(double m, double n, double nrhs)
    { return 6.*fmuls_geqrs(m, n, nrhs) + 2.*fadds_geqrs(m, n, nrhs); }

static double  flops_cgeqrs(double m, double n, double nrhs)
    { return 6.*fmuls_geqrs(m, n, nrhs) + 2.*fadds_geqrs(m, n, nrhs); }

static double  flops_dgeqrs(double m, double n, double nrhs)
    { return    fmuls_geqrs(m, n, nrhs) +    fadds_geqrs(m, n, nrhs); }

static double  flops_sgeqrs(double m, double n, double nrhs)
    { return    fmuls_geqrs(m, n, nrhs) +    fadds_geqrs(m, n, nrhs); }

//------------------------------------------------------------ unmqr
static double fmuls_unmqr(plasma_enum_t side, double m, double n, double k)
{
    return (side == PlasmaLeft)
        ? (2.*n*m*k - n*k*k + 2.*n*k)
        : (2.*n*m*k - m*k*k + m*k + n*k - 0.5*k*k + 0.5*k);
}

static double fadds_unmqr(plasma_enum_t side, double m, double n, double k)
{
    return (side == PlasmaLeft)
        ? (2.*n*m*k - n*k*k + n*k)
        : (2.*n*m*k - m*k*k + m*k);
}

static double  flops_zunmqr(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmqr(side, m, n, k) + 2.*fadds_unmqr(side, m, n, k); }

static double  flops_cunmqr(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmqr(side, m, n, k) + 2.*fadds_unmqr(side, m, n, k); }

static double  flops_dormqr(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmqr(side, m, n, k) +    fadds_unmqr(side, m, n, k); }

static double  flops_sormqr(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmqr(side, m, n, k) +    fadds_unmqr(side, m, n, k); }

//------------------------------------------------------------ unmql
static double fmuls_unmql(plasma_enum_t side, double m, double n, double k)
    { return fmuls_unmqr(side, m, n, k); }

static double fadds_unmql(plasma_enum_t side, double m, double n, double k)
    { return fadds_unmqr(side, m, n, k); }

static double  flops_zunmql(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmql(side, m, n, k) + 2.*fadds_unmql(side, m, n, k); }

static double  flops_cunmql(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmql(side, m, n, k) + 2.*fadds_unmql(side, m, n, k); }

static double  flops_dormql(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmql(side, m, n, k) +    fadds_unmql(side, m, n, k); }

static double  flops_sormql(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmql(side, m, n, k) +    fadds_unmql(side, m, n, k); }

//------------------------------------------------------------ unmrq
static double fmuls_unmrq(plasma_enum_t side, double m, double n, double k)
    { return fmuls_unmqr(side, m, n, k); }

static double fadds_unmrq(plasma_enum_t side, double m, double n, double k)
    { return fadds_unmqr(side, m, n, k); }

static double  flops_zunmrq(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmrq(side, m, n, k) + 2.*fadds_unmrq(side, m, n, k); }

static double  flops_cunmrq(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmrq(side, m, n, k) + 2.*fadds_unmrq(side, m, n, k); }

static double  flops_dormrq(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmrq(side, m, n, k) +    fadds_unmrq(side, m, n, k); }

static double  flops_sormrq(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmrq(side, m, n, k) +    fadds_unmrq(side, m, n, k); }

//------------------------------------------------------------ unmlq
static double fmuls_unmlq(plasma_enum_t side, double m, double n, double k)
    { return fmuls_unmqr(side, m, n, k); }

static double fadds_unmlq(plasma_enum_t side, double m, double n, double k)
    { return fadds_unmqr(side, m, n, k); }

static double  flops_zunmlq(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmlq(side, m, n, k) + 2.*fadds_unmlq(side, m, n, k); }

static double  flops_cunmlq(plasma_enum_t side, double m, double n, double k)
    { return 6.*fmuls_unmlq(side, m, n, k) + 2.*fadds_unmlq(side, m, n, k); }

static double  flops_dormlq(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmlq(side, m, n, k) +    fadds_unmlq(side, m, n, k); }

static double  flops_sormlq(plasma_enum_t side, double m, double n, double k)
    { return    fmuls_unmlq(side, m, n, k) +    fadds_unmlq(side, m, n, k); }

//------------------------------------------------------------ trtri
static double fmuls_trtri(double n)
    { return 1./6.*n*n*n + 0.5*n*n + 1./3.*n; }

static double fadds_trtri(double n)
    { return 1./6.*n*n*n - 0.5*n*n + 1./3.*n; }

static double  flops_ztrtri(double n)
    { return 6.*fmuls_trtri(n) + 2.*fadds_trtri(n); }

static double  flops_ctrtri(double n)
    { return 6.*fmuls_trtri(n) + 2.*fadds_trtri(n); }

static double  flops_dtrtri(double n)
    { return    fmuls_trtri(n) +    fadds_trtri(n); }

static double  flops_strtri(double n)
    { return    fmuls_trtri(n) +    fadds_trtri(n); }

//------------------------------------------------------------ gehrd
static double fmuls_gehrd(double n)
    { return 5./3.*n*n*n + 0.5*n*n - 7./6.*n; }

static double fadds_gehrd(double n)
    { return 5./3.*n*n*n - n*n - 2./3.*n; }

static double  flops_zgehrd(double n)
    { return 6.*fmuls_gehrd(n) + 2.*fadds_gehrd(n); }

static double  flops_cgehrd(double n)
    { return 6.*fmuls_gehrd(n) + 2.*fadds_gehrd(n); }

static double  flops_dgehrd(double n)
    { return    fmuls_gehrd(n) +    fadds_gehrd(n); }

static double  flops_sgehrd(double n)
    { return    fmuls_gehrd(n) +    fadds_gehrd(n); }

//------------------------------------------------------------ sytrd
static double fmuls_sytrd(double n)
    { return 2./3.*n*n*n + 2.5*n*n - 1./6.*n; }

static double fadds_sytrd(double n)
    { return 2./3.*n*n*n + n*n - 8./3.*n; }

static double fmuls_hetrd(double n)
    { return fmuls_sytrd(n); }

static double fadds_hetrd(double n)
    { return fadds_sytrd(n); }

static double  flops_zhetrd(double n)
    { return 6.*fmuls_hetrd(n) + 2.*fadds_hetrd(n); }

static double  flops_chetrd(double n)
    { return 6.*fmuls_hetrd(n) + 2.*fadds_hetrd(n); }

static double  flops_dsytrd(double n)
    { return    fmuls_sytrd(n) +    fadds_sytrd(n); }

static double  flops_ssytrd(double n)
    { return    fmuls_sytrd(n) +    fadds_sytrd(n); }

//------------------------------------------------------------ gebrd
static double fmuls_gebrd(double m, double n)
{
    return (m >= n)
        ? (2.*m*n*n - 2./3.*n*n*n + 2.*n*n + 20./3.*n)
        : (2.*n*m*m - 2./3.*m*m*m + 2.*m*m + 20./3.*m);
}

static double fadds_gebrd(double m, double n)
{
    return (m >= n)
        ? (2.*m*n*n - 2./3.*n*n*n + n*n - m*n +  5./3.*n)
        : (2.*n*m*m - 2./3.*m*m*m + m*m - n*m +  5./3.*m);
}

static double  flops_zgebrd(double m, double n)
    { return 6.*fmuls_gebrd(m, n) + 2.*fadds_gebrd(m, n); }

static double  flops_cgebrd(double m, double n)
    { return 6.*fmuls_gebrd(m, n) + 2.*fadds_gebrd(m, n); }

static double  flops_dgebrd(double m, double n)
    { return    fmuls_gebrd(m, n) +    fadds_gebrd(m, n); }

static double  flops_sgebrd(double m, double n)
    { return    fmuls_gebrd(m, n) +    fadds_gebrd(m, n); }

//------------------------------------------------------------ larfg
static double fmuls_larfg(double n)
    { return 2*n; }

static double fadds_larfg(double n)
    { return   n; }

static double  flops_zlarfg(double n)
    { return 6.*fmuls_larfg(n) + 2.*fadds_larfg(n); }

static double  flops_clarfg(double n)
    { return 6.*fmuls_larfg(n) + 2.*fadds_larfg(n); }

static double  flops_dlarfg(double n)
    { return    fmuls_larfg(n) +    fadds_larfg(n); }

static double  flops_slarfg(double n)
    { return    fmuls_larfg(n) +    fadds_larfg(n); }

//------------------------------------------------------------ geadd
static double fmuls_geadd(double m, double n)
    { return 2*m*n; }

static double fadds_geadd(double m, double n)
    { return   m*n; }

static double flops_zgeadd(double m, double n)
    { return 6.*fmuls_geadd(m, n) + 2.*fadds_geadd(m, n); }

static double flops_cgeadd(double m, double n)
    { return 6.*fmuls_geadd(m, n) + 2.*fadds_geadd(m, n); }

static double flops_dgeadd(double m, double n)
    { return    fmuls_geadd(m, n) +    fadds_geadd(m, n); }

static double flops_sgeadd(double m, double n)
    { return    fmuls_geadd(m, n) +    fadds_geadd(m, n); }

//------------------------------------------------------------ lauum
static double fmuls_lauum(double n)
    { return fmuls_potri(n) - fmuls_trtri(n); }

static double fadds_lauum(double n)
    { return fadds_potri(n) - fadds_trtri(n); }

static double flops_zlauum(double n)
    { return 6.*fmuls_lauum(n) + 2.*fadds_lauum(n); }

static double flops_clauum(double n)
    { return 6.*fmuls_lauum(n) + 2.*fadds_lauum(n); }

static double flops_dlauum(double n)
    { return    fmuls_lauum(n) +    fadds_lauum(n); }

static double flops_slauum(double n)
    { return    fmuls_lauum(n) +    fadds_lauum(n); }

//------------------------------------------------------------ lange
static double fmuls_lange(double m, double n, plasma_enum_t norm)
    { return norm == PlasmaFrobeniusNorm ? m*n : 0.0; }

static double fadds_lange(double m, double n, plasma_enum_t norm)
{
    switch (norm) {
    case PlasmaOneNorm:       return (m-1)*n;
    case PlasmaInfNorm:       return (n-1)*m;
    case PlasmaFrobeniusNorm: return m*n-1;
    default:                  return 0.0;
    }
}

static double flops_zlange(double m, double n, plasma_enum_t norm)
    { return 6.*fmuls_lange(m, n, norm) + 2.*fadds_lange(m, n, norm); }

static double flops_clange(double m, double n, plasma_enum_t norm)
    { return 6.*fmuls_lange(m, n, norm) + 2.*fadds_lange(m, n, norm); }

static double flops_dlange(double m, double n, plasma_enum_t norm)
    { return    fmuls_lange(m, n, norm) +    fadds_lange(m, n, norm); }

static double flops_slange(double m, double n, plasma_enum_t norm)
    { return    fmuls_lange(m, n, norm) +    fadds_lange(m, n, norm); }

//------------------------------------------------------------ lanhe
static double fmuls_lanhe(double n, plasma_enum_t norm)
    { return norm == PlasmaFrobeniusNorm ? n*(n+1)/2 : 0.0; }

static double fadds_lanhe(double n, plasma_enum_t norm)
{
    switch (norm) {
    case PlasmaOneNorm:       return (n-1)*n;
    case PlasmaInfNorm:       return (n-1)*n;
    case PlasmaFrobeniusNorm: return n*(n+1)/2-1;
    default:                  return 0.0;
    }
}

static double flops_zlanhe(double n, plasma_enum_t norm)
    { return 6.*fmuls_lanhe(n, norm) + 2.*fadds_lanhe(n, norm); }

static double flops_clanhe(double n, plasma_enum_t norm)
    { return 6.*fmuls_lanhe(n, norm) + 2.*fadds_lanhe(n, norm); }

static double flops_dlansy(double n, plasma_enum_t norm)
    { return    fmuls_lanhe(n, norm) +    fadds_lanhe(n, norm); }

static double flops_slansy(double n, plasma_enum_t norm)
    { return    fmuls_lanhe(n, norm) +    fadds_lanhe(n, norm); }

#ifdef __cplusplus
}  // extern "C"
#endif

#endif // ICL_PLASMA_FLOPS_H
