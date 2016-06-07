include ../../common/OPENMP.defs
##### User configurable options #####
 
OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system
 
#uncomment any of the following flags (and change values) to change defaults
 
RESTRICTFLAG = -DRESTRICT_KEYWORD
#description: the "restrict" keyword can be used on IA platforms to disambiguate  
#             data accessed through pointers
 
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
 
TUNEFLAGS   = $(MEMFLAG) $(DEBUGFLAG) $(NTHREADFLAG) $(USERFLAGS)
PROGRAM     = reduce
OBJS        = $(PROGRAM).o $(COMOBJS)
 
include ../../common/make.common
