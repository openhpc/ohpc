include ../../common/MPI.defs

##### User configurable options #####

OPTFLAGS    = $(DEFAULT_OPT_FLAGS)
#description: change above into something that is a decent optimization on you system

#uncomment any of the following flags (and change values) to change defaults

#DENSEFLAG = -DTESTDENSE
#description: if flag is set, sparse matrix will be embedded in dense matrix

#RESTRICTFLAG = -DRESTRICT_KEYWORD
#description: the "restrict" keyword can be used on IA platforms to disambiguate  
#             data accessed through pointers

#DEBUGFLAG    = -DVERBOSE
#description: default diagnostic style is silent

SCRAMBLEFLAG = -DSCRAMBLE
#description: if flag is set, grid indices are scrambled to produce irregular stride

USERFLAGS     =    
#description: parameter to specify optional flags

EXTOBJS      = 
LIBS         =
LIBPATHS     = 
INCLUDEPATHS = 

### End User configurable options ###

TUNEFLAGS   = $(DENSEFLAG) $(DEBUGFLAG) $(NTHREADFLAG) $(USERFLAGS) $(SCRAMBLEFLAG)
PROGRAM     = sparse
OBJS        = $(PROGRAM).o $(COMOBJS)

include ../../common/make.common
