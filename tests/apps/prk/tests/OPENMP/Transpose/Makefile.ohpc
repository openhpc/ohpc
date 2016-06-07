include ../../common/OPENMP.defs

##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS) 
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#RESTRICTFLAG = -DRESTRICT_KEYWORD
#description: the "restrict" keyword can be used on IA platforms to disambiguate  
#             data accessed through pointers

#DEBUGFLAG   = -DVERBOSE
#description: default diagnostic style is silent

#COLLAPSEFLAG = -DCOLLAPSE
#description: collapse outer two loops in case of tiling to increase concurrency

#NTHREADFLAG = -DMAXTHREADS=n
#description: default thread limit is 256

USERFLAGS    = 
#description: parameter to specify optional flags

#set the following variables for custom libraries and/or other objects
EXTOBJS      = 
LIBS         =
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS    = $(DEBUGFLAG) $(NTHREADFLAG) $(USERFLAGS) $(RESTRICTFLAG) $(COLLAPSEFLAG)
PROGRAM      = transpose
OBJS         = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
