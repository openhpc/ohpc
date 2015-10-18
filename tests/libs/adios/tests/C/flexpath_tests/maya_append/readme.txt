# readme.txt
# Created on: Aug 21, 2013
# Author: Magda S. aka Magic Magg magg dot gatech at gmail.com
#
DESCRIPTION
===========
The idea is to test how to write in an appended mode and read from
that mode. This test is based on the Maya project.

It is designed for two methods (use -t flx or -t mpi)

1. MPI/ADIOS_READ_METHOD_BP
2. FLEXPATH/ADIOS_READ_METHOD_FLEXPATH


RUN
===== 
# to clean files hanging around after previous runs
$ make -f Makefile.generic clean_test

# you can run as many writers as you want and as many readers as you want
# they write and read independently; the default is to run one writer
# and one reader
$ ./writer -f mpi
$ ./reader -f mpi

See Makefile for other options of running the test.

PLAYING WITH TEST CONFIGURATION
===============================
To play with the test configuration, you can modify macros in the cfg.h file

TIMESTEP_COUNT seems to be the only reasonable setting to play so far

NOTES
======
2013-08-28 Test passes with the MPI method on my laptop; it fails with the FLEXPATH method
enabled (branch v1.5.1)

$ ./reader
ERROR: FLEXPATH staging method does not support file mode for reading. Use adios_read_open() to open a staged dataset.



TROUBLESHOOTING
================

2013-08-06, ERROR adios_allocate_buffer(): insufficient memory

ERROR: adios_allocate_buffer (): insufficient memory: 5242880000 requested, 860221440 available.  Using available.

$ grep ADS_BUFFER_SIZE cfg.h
#define ADS_BUFFER_SIZE 50

Try changing the ADS_BUFFER_SIZE in cfg.h to a smaller value.

2013-0x-0y, txt files left

There might be text files left; they should be removed for the next run.

# EOF

# EOF

