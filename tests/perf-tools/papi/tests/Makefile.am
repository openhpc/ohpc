noinst_LIBRARIES        = libtestlib.a
libtestlib_a_SOURCES    = do_loops.c test_utils.c dummy.c clockcore.c ftests_util.F

TESTS                   = dmem_info
check_PROGRAMS          = dmem_info
dmem_info_SOURCES       = dmem_info.c
dmem_info_LDADD         = libtestlib.a

check_PROGRAMS         += branches
branches_SOURCES        = branches.c
branches_LDADD          = libtestlib.a

TESTS                  += clockres_pthreads
check_PROGRAMS         += clockres_pthreads
clockres_pthreads_SOURCES = clockres_pthreads.c
clockres_pthreads_LDADD = libtestlib.a -lpthread -lm

TESTS                  += cycle_ratio
check_PROGRAMS         += cycle_ratio
cycle_ratio_SOURCES     = cycle_ratio.c
cycle_ratio_LDADD       = libtestlib.a

check_PROGRAMS         += first
first_SOURCES           = first.c
first_LDADD             = libtestlib.a

TESTS                  += memory
check_PROGRAMS         += memory
memory_SOURCES          = memory.c
memory_LDADD            = libtestlib.a

check_PROGRAMS         += cost
cost_SOURCES            = cost.F
cost_LDADD              = libtestlib.a

check_PROGRAMS         += fmultiplex1
fmultiplex1_SOURCES     = fmultiplex1.F
fmultiplex1_LDADD       = libtestlib.a

TESTS                  += avail
check_PROGRAMS         += avail
avail_SOURCES           = avail.F
avail_LDADD             = libtestlib.a

TESTS                  += strtest
check_PROGRAMS         += strtest
strtest_SOURCES         = strtest.F
strtest_LDADD           = libtestlib.a

check_PROGRAMS         += tenth
tenth_SOURCES           = tenth.F
tenth_LDADD             = libtestlib.a

check_PROGRAMS         += zero
zero_SOURCES            = zero.F
zero_LDADD              = libtestlib.a

check_PROGRAMS         += simple
simple_SOURCES          = simple.c

check_PROGRAMS         += threads
threads_SOURCES         = threads.c
threads_LDADD           = -lpthread

TESTS                  += native_avail
TESTS                  += test_module
TESTS                  += rm_execution
