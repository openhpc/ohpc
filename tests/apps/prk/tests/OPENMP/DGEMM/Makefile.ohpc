include ../../common/OPENMP.defs

##### User configurable options #####

OPTFLAGS     = $(DEFAULT_OPT_FLAGS) 
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#BLOCKFLAG    = -DDEFAULTBLOCK=32
#description: default tile size is 32 (used only for non-MKL version)

#OFFSETFLAG   = -DBOFFSET=0
#description: set this flag to some value to override default first array 
#             dimension padding (12) of tiles used in non-MKL version

#MKLFLAG      = -DMKL
#description: set this flag to call the tuned mkl library

#DEBUGFLAG    = -DVERBOSE
#description: default diagnostic style is silent

#NTHREADFLAG  = -DMAXTHREADS=n
#description: default thread limit is 256

USERFLAGS     =    
#description: parameter to specify optional flags

EXTOBJS      = 
#LIBS         = -lmkl_ia32
#LIBPATHS     = -L/opt/intel/mkl/10.0.3.020/lib/32
#INCLUDEPATHS = -I/opt/intel/mkl/10.0.3.020/include

### End User configurable options ###

TUNEFLAGS   = $(BLOCKFLAG) $(MKLFLAG)     $(OFFSETFLAG) \
              $(DEBUGFLAG) $(NTHREADFLAG) $(USERFLAGS)
PROGRAM     = dgemm
OBJS        = $(PROGRAM).o $(COMOBJS) 

include ../../common/make.common
