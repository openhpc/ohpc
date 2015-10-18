Example programs for HDF5

To build all examples type:
./configure
make

To test all the example programs type:
make check

Some notes:

- This package uses h5cc (and h5fc) to compile the examples.

- This package is not meant to be installed, `make install' will do
nothing.

- This package supports VPATH builds (run configure from a different
directory), but will not automatically copy the data files a few
examples need to run (*.h5) to the build directory.  This must be done
manually if you wish to run these examples from the build directory.
`make check' is supported with VPATH - examples do not need to be
manually copied for `make check' to work.

- To build only 1.6 examples, set environmental variable `H5EX_16' to
anything but null before runnning configure.  Similary to build only 1.8
examples, set `H5EX_18', and to build only 1.10 examples, set `H5EX_110'.
These variables can be set temporarily in the call to configure, e.g.
`./configure H5EX_16=1'.  Alternatively you can 'make' in a subdirectory
to only build examples under that directory.

- Similary examples can be built only for specified classes by setting:
	H5EX_G for groups
	H5EX_D for datasets
	H5EX_T for datatypes

- This package requires HDF5 Library version 1.8 or later.  If the
default h5cc is not the correct version, you must set CC to the path to
the correct h5cc.  Compiling only 1.6 examples with a 1.6 library
should work, but make check may fail.

- This package links dynamically with the hdf5 library by default.  To link
statically use the --disable-shared option with configure.
