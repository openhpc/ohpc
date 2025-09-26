TESTS                   = superlu

check_PROGRAMS          = superlu
superlu_LDADD           = -lsuperlu
superlu_SOURCES         = superlu.c sp_ienv.c

check_PROGRAMS         += f77
f77_LDADD               = -lsuperlu
f77_SOURCES             = f77_main.f hbcode1.f c_fortran_dgssv.c

TESTS                   = test_module
TESTS                  += rm_execution
