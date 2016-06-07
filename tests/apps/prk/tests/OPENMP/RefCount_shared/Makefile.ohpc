include ../../common/OPENMP.defs

##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#DEBUGFLAG    = -DVERBOSE
#description: default diagnostic style is silent

DEPENDENCEFLAG = -DDEPENDENT
#description: default counter update is with inter-dependence

#NTHREADFLAG  = -DMAXTHREADS=n
#description: default thread limit is 256

USERFLAGS     = 
#description: parameter to specify optional flags

EXTOBJS      = 
LIBS         = -lm
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS    = $(DEPENDENCEFLAG) $(DEBUGFLAG) $(NTHREADFLAG) $(USERFLAGS)
PROGRAM     = shared
# objects below are the default, used by "clean," if invoked
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
