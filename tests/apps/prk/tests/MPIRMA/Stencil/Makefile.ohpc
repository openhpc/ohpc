include ../../common/MPI.defs
##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#RESTRICTFLAG = -DRESTRICT_KEYWORD
#description: the "restrict" keyword can be used on IA platforms to disambiguate  
#             data accessed through pointers

#RADIUSFLAG  = -DRADIUS=R
#description: default radius of filter to be applied is 2

DOUBLEFLAG = -DDOUBLE
#description: default data type is single precision

STARFLAG   = -DSTAR
#description: default stencil is compact (dense, square)

#DEBUGFLAG   = -DVERBOSE
#description: default diagnostic style is silent

USERFLAGS    = 
#description: parameter to specify optional flags

#set the following variables for custom libraries and/or other objects
EXTOBJS      = 
LIBS         = -lm
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS    = $(RESTRICTFLAG) $(DEBUGFLAG)  $(USERFLAGS) \
               $(DOUBLEFLAG)   $(RADIUSFLAG) $(STARFLAG)
PROGRAM     = stencil
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
