include ../../common/OPENMP.defs

##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

LOCKFLAG    = -DLOCK
#description: if flag is unset, no locks will be used 

#LONG64FLAG = -DLONG_IS_64BITS
#description: can use "long" for 64 bit integers instead of "long long"

#CACHEFLAG    = -DLINE_LENGTH=L
#description: default cache line size is 64 bytes

#DEBUGFLAG    = -DVERBOSE
#description: default diagnostic style is silent

#NTHREADFLAG  = -DMAXTHREADS=n
#description: default thread limit is 256

USERFLAGS     = 
#description: parameter to specify optional flags

EXTOBJS      = 
LIBS         =
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS    = $(LONG64FLAG) $(CACHEFLAG) $(LOCKFLAG) \
               $(DEBUGFLAG) $(NTHREADFLAG) $(USERFLAGS)
PROGRAM     = private
# objects below are the default, used by "clean," if invoked
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
