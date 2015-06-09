#
# This file is part of MUMPS 4.9.2, built on Thu Nov 5 07:05:08 UTC 2009
#
#Begin orderings

# NOTE that PORD is distributed within MUMPS by default. If you would like to
# use other orderings, you need to obtain the corresponding package and modify
# the variables below accordingly.
# For example, to have Metis available within MUMPS:
# 1/ download Metis and compile it
# 2/ uncomment (suppress # in first column) lines
# starting with LMETISDIR, LMETIS
# 3/ add -Dmetis in line ORDERINGSF
# ORDERINGSF = -Dpord -Dmetis
# 4/ Compile and install MUMPS
# make clean; make (to clean up previous installation)
#
# Metis/ParMetis and SCOTCH/PT-SCOTCH (ver 5.1 and later) orderings are now available for MUMPS.
#

#SCOTCHDIR = ${HOME}/scotch_5.1_esmumps
#ISCOTCH = -I$(SCOTCHDIR)/include 
# You have to choose one among the following two lines depending on
# the type of analysis you want to perform. If you want to perform only
# sequential analysis choose the first (remember to add -Dscotch in the ORDERINGSF
# variable below); for both parallel and sequential analysis choose the second 
# line (remember to add -Dptscotch in the ORDERINGSF variable below)

#LSCOTCH = -L$(SCOTCHDIR)/lib -lesmumps -lscotch -lscotcherr
#LSCOTCH = -L$(SCOTCHDIR)/lib -lptesmumps -lptscotch -lptscotcherr

LPORDDIR = $(topdir)/PORD/lib/
IPORD = -I$(topdir)/PORD/include/
LPORD = -L$(LPORDDIR) -lpord

#LMETISDIR = /local/metis/
#IMETIS = # Metis doesn't need include files (Fortran interface avail.)

# You have to choose one among the following two lines depending on
# the type of analysis you want to perform. If you want to perform only
# sequential analysis choose the first (remember to add -Dmetis in the ORDERINGSF
# variable below); for both parallel and sequential analysis choose the second 
# line (remember to add -Dparmetis in the ORDERINGSF variable below)

#LMETIS = -L$(LMETISDIR) -lmetis
#LMETIS = -L$(LMETISDIR) -lparmetis -lmetis

# The following variables will be used in the compilation process.
# Please note that -Dptscotch and -Dparmetis imply -Dscotch and -Dmetis respectively.
#ORDERINGSF = -Dscotch -Dmetis -Dpord -Dptscotch -Dparmetis
ORDERINGSF = -Dpord
ORDERINGSC = $(ORDERINGSF)

LORDERINGS = $(LMETIS) $(LPORD) $(LSCOTCH)
IORDERINGSF = $(ISCOTCH)
IORDERINGSC = $(IMETIS) $(IPORD) $(ISCOTCH)

# #End orderings
# ########################################################################
# ################################################################################
MKLPATH = /opt/intel/Compiler/11.0/083/mkl/lib/em64t
PLAT =
RM = /bin/rm -f
CC = mpicc
FC = mpif77
FL = mpif77
AR = ar vr
#RANLIB = ranlib
RANLIB = echo
SCALAP = -L$(MKLPATH) -lmkl_scalapack_lp64 $(MKLPATH)/libmkl_solver_lp64.a -Wl,--start-group -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -lmkl_blacs_intelmpi_lp64 -Wl,--end-group -openmp -lpthread

INCPAR = -I$MPI_DIR/include

LIBPAR = $(SCALAP) -L$MPI_DIR/lib -lmpi

INCSEQ = -I$(topdir)/libseq

LIBSEQ = -L$(topdir)/libseq -lmpiseq

LIBBLAS = -L$(MKLPATH) $(MKLPATH)/libmkl_solver_lp64.a -Wl,--start-group -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -lmkl_blacs_intelmpi_lp64 -Wl,--end-group -openmp -lpthread

LIBOTHERS = -lpthread

#Preprocessor defs for calling Fortran from C (-DAdd_ or -DAdd__ or -DUPPER)
CDEFS = -DAdd_

#Begin Optimized options
OPTF = -O -Dintel_ -DALLOW_NON_INIT -nofor_main
OPTL = -O -nofor_main
OPTC = -O
#End Optimized options
INC = $(INCPAR)
LIB = $(LIBPAR)
LIBSEQNEEDED =
