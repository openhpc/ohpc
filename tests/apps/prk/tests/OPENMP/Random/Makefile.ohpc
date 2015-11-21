include ../../common/OPENMP.defs

##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#RESTRICTFLAG = -DRESTRICT_KEYWORD
#description: the "restrict" keyword can be used on IA platforms to disambiguate  
#             data accessed through pointers

HPCCFLAG     = -DHPCC
#description: Uses HPC Challenge rules: non-atomic updates, shared table, 
#             1% errors allowed; overrides ATOMICFLAG, CHUNKFLAG, ERRORPERCENTFLAG

#ATOMICFLAG   = -DATOMIC
#description: protect threads from simultaneous access to same memory location

#CHUNKFLAG    = -DCHUNKED
#description: assign chunks of Table to different threads, so no atomic needed

ERRORPERCENTFLAG = -DERRORPERCENT=1
#description: percent errors allowed in solution; if unset, no errors allowed

#LONG64FLAG = -DLONG_IS_64BITS
#description: can use "long" for 64 bit integers instead of "long long"

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

TUNEFLAGS   = $(RESTRICTFLAG) $(LONG64FLAG) $(DEBUGFLAG) $(NTHREADFLAG) $(USERFLAGS) \
              $(ATOMICFLAG)   $(CHUNKFLAG)  $(HPCCFLAG)  $(ERRORPERCENTFLAG)
PROGRAM     = random
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
