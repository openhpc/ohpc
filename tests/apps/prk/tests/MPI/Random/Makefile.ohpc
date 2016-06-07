include ../../common/MPI.defs

##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#VECTORFLAG = -DLOOKAHEAD=2048
#description: Default value for LOOKAHEAD is 1024 (HPCC compliant)

#RESTRICTFLAG = -DRESTRICT_KEYWORD
#description: the "restrict" keyword can be used on IA platforms to disambiguate  
#             data accessed through pointers

#LONG64FLAG = -DLONG_IS_64BITS
#description: can use "long" for 64 bit integers instead of "long long"

#DEBUGFLAG    = -DVERBOSE
#description: default diagnostic style is silent

USERFLAGS     = 
#description: parameter to specify optional flags

EXTOBJS      = 
LIBS         =
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS   = $(RESTRICTFLAG) $(LONG64FLAG) $(DEBUGFLAG) $(USERFLAGS) $(VECTORFLAG)
PROGRAM     = random
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
