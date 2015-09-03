#=============================================================================
#====================== SECTION 1: PATHS AND LIBRARIES =======================
#=============================================================================
#  The following macros specify the name and location of libraries required by
#  the BLACS and its tester.
#=============================================================================
 
#  --------------------------------------
#  Make sure we've got a consistent shell
#  --------------------------------------
   SHELL = /bin/sh

#  -----------------------------
#  The top level BLACS directory
#  -----------------------------
   BTOPdir = /home/abuild/rpmbuild/BUILD/BLACS

#  ---------------------------------------------------------------------------
#  The communication library your BLACS have been written for.
#  Known choices (and the machines they run on) are:
#
#     COMMLIB   MACHINE
#     .......   ..............................................................
#     CMMD      Thinking Machine's CM-5
#     MPI       Wide variety of systems
#     MPL       IBM's SP series (SP1 and SP2)
#     NX        Intel's supercomputer series (iPSC2, iPSC/860, DELTA, PARAGON)
#     PVM       Most unix machines; See PVM User's Guide for details
#  ---------------------------------------------------------------------------
   COMMLIB = MPI

#  -------------------------------------------------------------
#  The platform identifier to suffix to the end of library names
#  -------------------------------------------------------------
   PLAT = LINUX

#  ----------------------------------------------------------
#  Name and location of the BLACS library.  See section 2 for 
#  details on BLACS debug level (BLACSDBGLVL).
#  ----------------------------------------------------------
   BLACSdir    = $(BTOPdir)/LIB
   BLACSDBGLVL = 0
   BLACSFINIT  = $(BLACSdir)/blacsF77init_$(COMMLIB)-$(PLAT)-$(BLACSDBGLVL).a
   BLACSCINIT  = $(BLACSdir)/blacsCinit_$(COMMLIB)-$(PLAT)-$(BLACSDBGLVL).a
   BLACSLIB    = $(BLACSdir)/blacs_$(COMMLIB)-$(PLAT)-$(BLACSDBGLVL).a

#  -------------------------------------
#  Name and location of the MPI library.
#  -------------------------------------
#   MPIdir = /usr/local/mpich
#   MPILIBdir = $(MPIdir)/lib/
    MPIINCdir = $(MPI_DIR)/include
#   MPILIB = $(MPILIBdir)/libmpich.a

#  -------------------------------------
#  All libraries required by the tester.
#  -------------------------------------
   BTLIBS = $(BLACSFINIT) $(BLACSLIB) $(BLACSFINIT) $(MPILIB) 

#  ----------------------------------------------------------------
#  The directory to put the installation help routines' executables
#  ----------------------------------------------------------------
   INSTdir = $(BTOPdir)/INSTALL/EXE

#  ------------------------------------------------
#  The name and location of the tester's executable
#  ------------------------------------------------
   TESTdir = $(BTOPdir)/TESTING/EXE
   FTESTexe = $(TESTdir)/xFbtest_$(COMMLIB)-$(PLAT)-$(BLACSDBGLVL)
   CTESTexe = $(TESTdir)/xCbtest_$(COMMLIB)-$(PLAT)-$(BLACSDBGLVL)
#=============================================================================
#=============================== End SECTION 1 ===============================
#=============================================================================
 

#=============================================================================
#========================= SECTION 2: BLACS INTERNALS ========================
#=============================================================================
#  The following macro definitions set preprocessor values for the BLACS.
#  The file Bconfig.h sets these values if they are not set by the makefile.
#  User's compiling only the tester can skip this entire section.
#  NOTE: The MPI defaults have been set for MPICH.
#=============================================================================

#  -----------------------------------------------------------------------
#  The directory to find the required communication library include files, 
#  if they are required by your system.
#  -----------------------------------------------------------------------
#   SYSINC = -I$(MPIINCdir)

#  ---------------------------------------------------------------------------
#  The Fortran 77 to C interface to be used.  If you are unsure of the correct
#  setting for your platform, compile and run BLACS/INSTALL/xintface.
#  Choices are: Add_, NoChange, UpCase, or f77IsF2C.
#  ---------------------------------------------------------------------------
   INTFACE = -Df77IsF2C

#  ------------------------------------------------------------------------
#  Allows the user to vary the topologies that the BLACS default topologies
#  (TOP = ' ') correspond to.  If you wish to use a particular topology
#  (as opposed to letting the BLACS make the choice), uncomment the
#  following macros, and replace the character in single quotes with the
#  topology of your choice.
#  ------------------------------------------------------------------------
#  DEFBSTOP   = -DDefBSTop="'1'"
#  DEFCOMBTOP = -DDefCombTop="'1'"

#  -------------------------------------------------------------------
#  If your MPI_Send is locally-blocking, substitute the following line
#  for the empty macro definition below.
#  SENDIS = -DSndIsLocBlk
#  -------------------------------------------------------------------
   SENDIS =

#  --------------------------------------------------------------------
#  If your MPI handles packing of non-contiguous messages by copying to
#  another buffer or sending extra bytes, better performance may be
#  obtained by replacing the empty macro definition below with the
#  macro definition on the following line.
#  BUFF = -DNoMpiBuff
#  --------------------------------------------------------------------
   BUFF = 

#  -----------------------------------------------------------------------
#  If you know something about your system, you may make it easier for the
#  BLACS to translate between C and fortran communicators.  If the empty
#  macro defininition is left alone, this translation will cause the C
#  BLACS to globally block for MPI_COMM_WORLD on calls to BLACS_GRIDINIT
#  and BLACS_GRIDMAP.  If you choose one of the options for translating
#  the context, neither the C or fortran calls will globally block.
#  If you are using MPICH, or a derivitive system, you can replace the 
#  empty macro definition below with the following (note that if you let
#  MPICH do the translation between C and fortran, you must also indicate
#  here if your system has pointers that are longer than integers.  If so,
#  define -DPOINTER_64_BITS=1.)  For help on setting TRANSCOMM, you can
#  run BLACS/INSTALL/xtc_CsameF77 and BLACS/INSTALL/xtc_UseMpich as
#  explained in BLACS/INSTALL/README.
#   TRANSCOMM = -DUseMpich
#
#  If you know that your MPI uses the same handles for fortran and C
#  communicators, you can replace the empty macro definition below with
#  the macro definition on the following line.
  TRANSCOMM = -DCSameF77
#  -----------------------------------------------------------------------
#  TRANSCOMM =

#  --------------------------------------------------------------------------
#  You may choose to have the BLACS internally call either the C or Fortran77
#  interface to MPI by varying the following macro.  If TRANSCOMM is left
#  empty, the C interface BLACS_GRIDMAP/BLACS_GRIDINIT will globally-block if
#  you choose to use the fortran internals, and the fortran interface will
#  block if you choose to use the C internals.  It is recommended that the 
#  user leave this macro definition blank, unless there is a strong reason
#  to prefer one MPI interface over the other.
#  WHATMPI = -DUseF77Mpi
#  WHATMPI = -DUseCMpi
#  --------------------------------------------------------------------------
   WHATMPI =

#  ---------------------------------------------------------------------------
#  Some early versions of MPICH and its derivatives cannot handle user defined
#  zero byte data types.  If your system has this problem (compile and run
#  BLACS/INSTALL/xsyserrors to check if unsure), replace the empty macro
#  definition below with the macro definition on the following line.
#  SYSERRORS = -DZeroByteTypeBug
#  ---------------------------------------------------------------------------
   SYSERRORS =

#  ------------------------------------------------------------------
#  These macros set the debug level for the BLACS.  The fastest
#  code is produced by BlacsDebugLvl 0.  Higher levels provide
#  more debug information at the cost of performance.  Present levels
#  of debug are:
#  0 : No debug information
#  1 : Mainly parameter checking.
#  ------------------------------------------------------------------
   DEBUGLVL = -DBlacsDebugLvl=$(BLACSDBGLVL)

#  -------------------------------------------------------------------------
#  All BLACS definitions needed for compile (DEFS1 contains definitions used
#  by all BLACS versions).
#  -------------------------------------------------------------------------
   DEFS1 = -DSYSINC $(SYSINC) $(INTFACE) $(DEFBSTOP) $(DEFCOMBTOP) $(DEBUGLVL)
   BLACSDEFS = $(DEFS1) $(SENDIS) $(BUFF) $(TRANSCOMM) $(WHATMPI) $(SYSERRORS)
#=============================================================================
#=============================== End SECTION 2 ===============================
#=============================================================================
 

#=============================================================================
#=========================== SECTION 3: COMPILERS ============================
#=============================================================================
#  The following macros specify compilers, linker/loaders, the archiver,
#  and their options.  Some of the fortran files need to be compiled with no
#  optimization.  This is the F77NO_OPTFLAG.  The usage of the remaining
#  macros should be obvious from the names.
#=============================================================================
   F77            = mpif77
   F77NO_OPTFLAGS = 
   F77FLAGS       = $(F77NO_OPTFLAGS) -O
   F77LOADER      = $(F77)
   F77LOADFLAGS   = 
   CC             = mpicc
   CCFLAGS        = -O4
   CCLOADER       = $(CC)
   CCLOADFLAGS    = 

#  --------------------------------------------------------------------------
#  The archiver and the flag(s) to use when building an archive (library).
#  Also the ranlib routine.  If your system has no ranlib, set RANLIB = echo.
#  --------------------------------------------------------------------------
   ARCH      = ar
   ARCHFLAGS = r
   RANLIB    = ranlib

#=============================================================================
#=============================== End SECTION 3 ===============================
#=============================================================================
