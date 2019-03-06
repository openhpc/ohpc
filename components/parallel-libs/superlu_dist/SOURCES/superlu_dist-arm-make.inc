############################################################################
#
#  Program:         SuperLU_DIST
#
#  Module:          make.inc
#
#  Purpose:         Top-level Definitions
#
#  Creation date:   March 1, 2016	version 5.0.0
#
#  Modified:	    
#		    
#
############################################################################
#
#  The name of the libraries to be created/linked to
#
SuperLUroot	=
DSUPERLULIB   	= $(SuperLUroot)/SRC/libsuperlu_dist.a
INCLUDEDIR      = $(SuperLUroot)/SRC
BLASDEF		= -DUSE_VENDOR_BLAS
BLASLIB		= -L${ARMPL_LIBRARIES} -larmpl_mp -fopenmp

INCLUDES    = -I${ARMPL_INCLUDES_MP} -I$(PTSCOTCH_INC)
LIBS		= $(DSUPERLULIB) -L$(METIS_LIB) -L$(PTSCOTCH_LIB) -lptscotchparmetis -lptscotch -lptscotcherr -lmetis -lscotch $(BLASLIB) -lbz2 -lz

#
#  The archiver and the flag(s) to use when building archive (library)
#  If your system has no ranlib, set RANLIB = echo.
#
ARCH         = /usr/bin/ar
ARCHFLAGS    = cr
RANLIB       = /usr/bin/ranlib

CC           = mpicc
CXX          = mpicxx
CFLAGS 	     += -O3 -DNDEBUG $(BLASDEF) -DDEBUGlevel=0 -DPRNTlevel=0 $(INCLUDES) -fopenmp -std=c11 -fPIC -DPIC -g
CXXFLAGS     += -O3 -DNDEBUG $(BLASDEF) -DDEBUGlevel=0 -DPRNTlevel=0 $(INCLUDES) -fopenmp -fPIC -g
NOOPTS       = -O0
FORTRAN	     = mpif90

LOADER       = $(CC) -fopenmp
LOADOPTS     =
