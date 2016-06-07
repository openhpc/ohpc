Option INS_HEAVY of the Branching code requires a customized version
of file func.c, which contains a variable number of functions of
variable size. This file is built by invoking script "func_gen,"
which takes two integer input parameters. The first is the rank of
the square matrices whose elements get initialized individually by
one of the functions in func.c. Hence, the number of instructions
associated with each such function is proportional to rank*rank.
The second parameters is the number of functions created. These
functions are all slightly different, but all have the same size.

Usage: func_gen <matrix_rank> <number_of_functions>
The function is invoked with default values 10 and 40 if no
values for these variables are supplied on the make command line.
  
