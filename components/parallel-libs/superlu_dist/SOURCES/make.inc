############################################################################
#
#  Program:         SuperLU_DIST
#
#  Module:          make.inc
#
#  Purpose:         Top-level Definitions
#
#  Creation date:   February 4, 1999   version alpha
#
#  Modified:	    September 1, 1999  version 1.0
#                   March 15, 2003     version 2.0
#		    November 1, 2007   version 2.1
#		    September 1, 2011   version 3.0
#
############################################################################
#
#  The machine (platform) identifier to append to the library names
#
## edison at NERSC
#PLAT		= _xc30
VERSION		= 5.1.3
#
#  The name of the libraries to be created/linked to
#
DSuperLUroot 	= ${HOME}/Release_Codes/SuperLU_DIST_${VERSION}
DSUPERLULIB   	= $(DSuperLUroot)/lib/libsuperlu_dist_${VERSION}.a
#
BLASDEF	     	= -DUSE_VENDOR_BLAS
BLASLIB	        = ${MKL}
#
############################################################################
## parmetis 4.x.x, 32-bit integer
# PARMETIS_DIR	:= ${HOME}/Edison/lib/parmetis-4.0.3
## parmetis 4.x.x, 64-bit integer
# PARMETIS_DIR	:= ${HOME}/Edison/lib/parmetis-4.0.3_64

METISLIB := -L${METIS_LIB} -lmetis
#PARMETISLIB := -L${PARMETIS_DIR}/build/Linux-x86_64/libparmetis -lparmetis
#I_PARMETIS := -I${PARMETIS_DIR}/include -I${PARMETIS_DIR}/metis/include
############################################################################

# Define the required Fortran libraries, if you use C compiler to link
# FLIBS	 	= -lpgf90 -lpgf90_rpm1  ## for PGI compiler
#FLIBS	 	= -lifport -lifcore     ## for Intel compiler

# Define all the libraries
LIBS	     	= $(DSUPERLULIB) $(BLASLIB) $(METISLIB) $(FLIBS)

#
#  The archiver and the flag(s) to use when building archive (library)
#  If your system has no ranlib, set RANLIB = echo.
#
ARCH         	= ar
ARCHFLAGS    	= cr
RANLIB       	= ranlib

############################################################################
# C compiler setup
CC           	= mpicc
# CFLAGS should be set to be the C flags that include optimization
CFLAGS          = -fPIC -m64 -std=c99 -Wall \
		$(I_PARMETIS) -DDEBUGlevel=0 -DPRNTlevel=0 -DPROFlevel=0 \
# uncomment the following to use 64-bit integer
# CFLAGS 		+= -D_LONGINT

# NOOPTS should be set to be the C flags that turn off any optimization
NOOPTS		= -O0 -std=c99 -fPIC
############################################################################
# FORTRAN compiler setup
FORTRAN         = mpif90
F90FLAGS	= -Mnomain -fPIC
# uncomment the following to use 64-bit integer
# F90FLAGS	+= -i8
############################################################################
LOADER	        = mpicc
LOADOPTS	= -fPIC
############################################################################
#  C preprocessor defs for compilation (-DNoChange, -DAdd_, or -DUpCase)
#
#  Need follow the convention of how C calls a Fortran routine.
#
CDEFS        = -DAdd_
