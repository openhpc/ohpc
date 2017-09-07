TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
AM_FCFLAGS = "-I ./include"
AM_CFLAGS = "-DADD_ -I ./include"

TESTS                   = xscblat1
check_PROGRAMS          = xscblat1
xscblat1_SOURCES        = c_sblas1.c c_sblat1.f

TESTS                  += xdcblat1
check_PROGRAMS         += xdcblat1
xdcblat1_SOURCES        = c_dblas1.c c_dblat1.f

TESTS                  += xccblat1
check_PROGRAMS         += xccblat1
xccblat1_SOURCES        = c_cblas1.c c_cblat1.f

TESTS                  += xzcblat1
check_PROGRAMS         += xzcblat1
xzcblat1_SOURCES        = c_zblas1.c c_zblat1.f

check_PROGRAMS         += xscblat2
xscblat2_SOURCES        = c_sblas2.c c_sblat2.f c_s2chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += xdcblat2
xdcblat2_SOURCES        = c_dblas2.c c_dblat2.f c_d2chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += xccblat2
xccblat2_SOURCES        = c_cblas2.c c_cblat2.f c_c2chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += xzcblat2
xzcblat2_SOURCES        = c_zblas2.c c_zblat2.f c_z2chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += xscblat3
xscblat3_SOURCES        = c_sblas3.c c_sblat3.f c_s3chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += xdcblat3
xdcblat3_SOURCES        = c_dblas3.c c_dblat3.f c_d3chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += xccblat3
xccblat3_SOURCES        = c_cblas3.c c_cblat3.f c_c3chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += xzcblat3
xzcblat3_SOURCES        = c_zblas3.c c_zblat3.f c_z3chke.c c_xerbla.c auxiliary.c constant.c

check_PROGRAMS         += eigen
eigen_SOURCES           = eigen.cpp

TESTS                  += test_module
TESTS                  += test_lapack
TESTS                  += rm_execution

