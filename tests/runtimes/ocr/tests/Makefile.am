TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
SUBDIRS                 = ohpc_module

TESTS                   = printf.test
check_PROGRAMS          = printf
EXTRA_DIST              = printf.test
printf_LDFLAGS          = -locr_x86
printf_SOURCES          = printf.c

TESTS                  += fft.test
check_PROGRAMS         += fft
EXTRA_DIST             += fft.test
fft_LDFLAGS             = -locr_x86
fft_SOURCES             = fft.c verify.c

TESTS                  += fib.test
check_PROGRAMS         += fib
EXTRA_DIST             += fib.test
fib_LDFLAGS             = -locr_x86
fib_SOURCES             = fib.c

TESTS                  += quicksort.test
check_PROGRAMS         += quicksort
EXTRA_DIST             += quicksort.test
quicksort_LDFLAGS       = -locr_x86
quicksort_SOURCES       = quicksort.c
