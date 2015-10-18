include ../../common/MPI.defs
##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#DEBUGFLAG    = -DVERBOSE
#description: default diagnostic style is silent

USERFLAGS     = 
#description: parameter to specify optional flags

#set the following variables for custom libraries and/or other objects

EXTOBJS      = 
LIBS         =
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS   = $(DEBUGFLAG) $(USERFLAGS)
PROGRAM     = global
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
