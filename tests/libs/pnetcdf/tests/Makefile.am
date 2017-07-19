TESTS_ENVIRONMENT           = BATS_NO_SUMMARY=1
SUBDIRS                     = ohpc_module

TESTS                       = pres_temp_4D_wr
check_PROGRAMS              = pres_temp_4D_wr
pres_temp_4D_wr_LDFLAGS     = -g -I. pres_temp_4D_wr.c
pres_temp_4D_wr_SOURCES     = testutils.c

TESTS                      += pres_temp_4D_rd
check_PROGRAMS             += pres_temp_4D_rd
pres_temp_4D_rd_LDFLAGS     = -g -I. pres_temp_4D_rd.c
pres_temp_4D_rd_SOURCES     = testutils.c

TESTS                      += nctst
check_PROGRAMS             += nctst
nctst_CXXFLAGS              = -Wno-write-strings
nctst_LDFLAGS               = -g -I. nctst.cpp
nctst_SOURCES               = testutils++.cxx

TESTS                      += test_classic
check_PROGRAMS             += test_classic
test_classic_CXXFLAGS       = -Wno-write-strings
test_classic_LDFLAGS        = -g -I. test_classic.cpp
test_classic_SOURCES        = testutils++.cxx

TESTS                      += f90tst_parallel
check_PROGRAMS             += f90tst_parallel
f90tst_parallel_LDFLAGS     = -g -I. f90tst_parallel.f90
f90tst_parallel_SOURCES     = testutilsf.F90

TESTS                      += f90tst_parallel2
check_PROGRAMS             += f90tst_parallel2
f90tst_parallel2_LDFLAGS    = -g -I. f90tst_parallel2.f90
f90tst_parallel2_SOURCES    = testutilsf.F90

TESTS                      += f90tst_parallel3
check_PROGRAMS             += f90tst_parallel3
f90tst_parallel3_LDFLAGS    = -g -I. f90tst_parallel3.f90
f90tst_parallel3_SOURCES    = testutilsf.F90

TESTS                      += f90tst_parallel4
check_PROGRAMS             += f90tst_parallel4
f90tst_parallel4_LDFLAGS    = -g -I. f90tst_parallel.f90
f90tst_parallel4_SOURCES    = testutilsf.F90

TESTS                      += f90tst_vars
check_PROGRAMS             += f90tst_vars
f90tst_vars_LDFLAGS         = -g -I. f90tst_vars.f90
f90tst_vars_SOURCES         = testutilsf.F90

TESTS                      += f90tst_vars2
check_PROGRAMS             += f90tst_vars2
f90tst_vars2_LDFLAGS        = -g -I. f90tst_vars2.f90
f90tst_vars2_SOURCES        = testutilsf.F90

TESTS                      += f90tst_vars3
check_PROGRAMS             += f90tst_vars3
f90tst_vars3_LDFLAGS        = -g -I. f90tst_vars3.f90
f90tst_vars3_SOURCES        = testutilsf.F90

TESTS                      += f90tst_vars4
check_PROGRAMS             += f90tst_vars4
f90tst_vars4_LDFLAGS        = -g -I. f90tst_vars4.f90
f90tst_vars4_SOURCES        = testutilsf.F90

TESTS                      += test_intent
check_PROGRAMS             += test_intent
test_intent_LDFLAGS         = -g -I. test_intent.f90
test_intent_SOURCES         = testutilsf.F90

TESTS                      += tst_f90
check_PROGRAMS             += tst_f90
tst_f90_LDFLAGS             = -g -I. tst_f90.f90
tst_f90_SOURCES             = testutilsf.F90

TESTS                      += tst_f90_cdf5
check_PROGRAMS             += tst_f90_cdf5
tst_f90_cdf5_LDFLAGS        = -g -I. tst_f90_cdf5.f90
tst_f90_cdf5_SOURCES        = testutilsf.F90

TESTS                      += tst_flarge
check_PROGRAMS             += tst_flarge
tst_flarge_LDFLAGS          = -g -I. tst_flarge.f90
tst_flarge_SOURCES          = testutilsf.F90

TESTS                      += tst_io
check_PROGRAMS             += tst_io
tst_io_LDFLAGS              = -g -I. tst_io.f90
tst_io_SOURCES              = testutilsf.F90

TESTS                      += tst_types2
check_PROGRAMS             += tst_types2
tst_types2_LDFLAGS          = -g -I. tst_types2.f90
tst_types2_SOURCES          = testutilsf.F90

TESTS                      += cdf_type
check_PROGRAMS             += cdf_type
cdf_type_LDFLAGS            = -g -I. cdf_type.c
cdf_type_SOURCES            = testutils.c

TESTS                      += dim_cdf12
check_PROGRAMS             += dim_cdf12
dim_cdf12_LDFLAGS           = -g -I. dim_cdf12.c
dim_cdf12_SOURCES           = testutils.c

TESTS                      += csnap
check_PROGRAMS             += csnap
csnap_LDFLAGS               = -g -I. csnap.c -lm
csnap_SOURCES               = testutils.c

TESTS                      += fixedform
check_PROGRAMS             += fixedform
if INTEL
fixedform_LDFLAGS           = -g -I. -fixed fixedform.f90
else
fixedform_LDFLAGS           = -g -I. -ffixed-form fixedform.f90
endif
fixedform_SOURCES           = testutilsf.F90

TESTS                      += freeform
check_PROGRAMS             += freeform
if INTEL
freeform_LDFLAGS            = -g -I. -free freeform.f
else 
freeform_LDFLAGS            = -g -I. -ffree-form freeform.f
endif
freeform_SOURCES            = testutilsf.F90

TESTS                      += pnctest
check_PROGRAMS             += pnctest
pnctest_LDFLAGS             = -g -I. pnctest.c
pnctest_SOURCES             = testutils.c

TESTS                      += pnctestf
check_PROGRAMS             += pnctestf
pnctestf_LDFLAGS            = -g -I. pnctestf.f
pnctestf_SOURCES            = testutilsf.F90

TESTS                      += pnf_test
check_PROGRAMS             += pnf_test
pnf_test_LDFLAGS            = -g -I. pnf_test.f
pnf_test_SOURCES            = testutilsf.F90

TESTS                      += header_consistency
check_PROGRAMS             += header_consistency
header_consistency_LDFLAGS  = -g -I. header_consistency.c
header_consistency_SOURCES  = testutils.c

TESTS                      += tst_atts
check_PROGRAMS             += tst_atts
tst_atts_LDFLAGS            = -g -I. tst_atts.c
tst_atts_SOURCES            = testutils.c

TESTS                      += tst_atts3
check_PROGRAMS             += tst_atts3
tst_atts3_LDFLAGS           = -g -I. tst_atts3.c
tst_atts3_SOURCES           = testutils.c

TESTS                      += tst_misc
check_PROGRAMS             += tst_misc
tst_misc_LDFLAGS            = -g -I. tst_misc.c
tst_misc_SOURCES            = testutils.c

TESTS                      += tst_names
check_PROGRAMS             += tst_names
tst_names_LDFLAGS           = -g -I. tst_names.c
tst_names_SOURCES           = testutils.c

TESTS                      += tst_nofill
check_PROGRAMS             += tst_nofill
tst_nofill_LDFLAGS          = -g -I. tst_nofill.c
tst_nofill_SOURCES          = testutils.c

TESTS                      += tst_norm
check_PROGRAMS             += tst_norm
tst_norm_LDFLAGS            = -g -I. tst_norm.c
tst_norm_SOURCES            = testutils.c

TESTS                      += tst_small
check_PROGRAMS             += tst_small
tst_small_LDFLAGS            = -g -I. tst_small.c
tst_small_SOURCES            = testutils.c

TESTS                      += rm_execution
