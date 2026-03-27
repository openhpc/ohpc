check_PROGRAMS          = hello badfree simwork
hello_SOURCES           = hello.c
badfree_SOURCES         = badfree.c
badfree_CFLAGS          = -Wno-free-nonheap-object
simwork_SOURCES         = simwork.c
simwork_CPPFLAGS        = -I$(VALGRIND_INC)

TESTS = test_module rm_execution
CLEANFILES              = callgrind.out*
