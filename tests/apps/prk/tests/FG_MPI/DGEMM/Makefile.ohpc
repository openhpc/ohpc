include ../../common/FG_MPI.defs

##### User configurable options #####

OPTFLAGS     = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#OFFSETFLAG   = -DBOFFSET=0
#description: set this flag to some value to override default first array 
#             dimension padding (12) of tiles used inside dgemm

#DEBUGFLAG    = -DVERBOSE
#description: default diagnostic style is silent

USERFLAGS     = 
#description: parameter to specify optional flags

#set the following variables for custom libraries and/or other objects
EXTOBJS      = 
LIBS         = -lm
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS   = $(DEBUGFLAG) $(OFFSETFLAG) $(USERFLAGS) 

PROGRAM  = dgemm
OBJS     = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
