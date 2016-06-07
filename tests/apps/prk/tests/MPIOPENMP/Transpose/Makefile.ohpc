include ../../common/MPIOPENMP.defs

##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#BLOCKFLAG    = -DSYNCHRONOUS
#description: turn on synchronous (blocking) communications

COLLAPSEFLAG = -DCOLLAPSE
#description: collapse outer 2 loops in case of tiling to increase concurrency

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

TUNEFLAGS   = $(BLOCKFLAG) $(DEBUGFLAG) $(USERFLAGS) $(COLLAPSEFLAG)
PROGRAM     = transpose
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
