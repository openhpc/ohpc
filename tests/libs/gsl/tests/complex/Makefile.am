TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

check_PROGRAMS    = test
TESTS             = $(check_PROGRAMS)
test_SOURCES      = test.c results.h results1.h results2.h results_real.h results_zreal.h

results:
	emacs -batch -l test.el -f test1 | sed  's/00\+e/0e/g' > results1.h
	emacs -batch -l test.el -f test2 | sed  's/00\+e/0e/g' > results.h
	emacs -batch -l test.el -f test3 | sed  's/00\+e/0e/g' > results_real.h
	emacs -batch -l test.el -f test4 | sed  's/00\+e/0e/g' > results2.h
	emacs -batch -l test.el -f test5 | sed  's/00\+e/0e/g' > results_zreal.h
